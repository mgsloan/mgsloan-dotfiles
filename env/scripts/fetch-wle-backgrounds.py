#!/usr/bin/env python3
"""Download Wiki Loves Earth international winners from Wikimedia Commons."""

from __future__ import annotations

import argparse
import html
import json
import re
import time
import urllib.parse
import urllib.request
import urllib.error
from pathlib import Path


API = "https://commons.wikimedia.org/w/api.php"
USER_AGENT = "mgsloan-wle-backgrounds/1.0 (personal wallpaper collection)"
BATCH_SIZE = 20


def api(**params: object) -> dict:
    query = urllib.parse.urlencode(
        {"format": "json", "formatversion": 2, **params}
    )
    request = urllib.request.Request(f"{API}?{query}", headers={"User-Agent": USER_AGENT})
    for attempt in range(5):
        try:
            with urllib.request.urlopen(request, timeout=90) as response:
                result = json.load(response)
            # extmetadata and parsing are expensive Commons queries. Wikimedia
            # asks unauthenticated clients to leave a longer gap after them.
            time.sleep(5)
            return result
        except urllib.error.HTTPError as error:
            if attempt == 4 or error.code != 429:
                raise
            time.sleep(int(error.headers.get("Retry-After", 60)))
        except Exception:
            if attempt == 4:
                raise
            time.sleep(2**attempt)
    raise AssertionError("unreachable")


def winner_titles(year: int) -> list[str]:
    page = f"Commons:Wiki Loves Earth {year}/Winners" if year > 2013 else "Commons:Wiki Loves Earth 2013"
    parsed = api(action="parse", page=page, prop="wikitext")["parse"]
    source = parsed["wikitext"]

    # Annual pages put the international gallery first, followed by the much
    # larger set of country finalists/local winners.
    start = re.search(r"(?im)^==\s*(?:international\s+)?winners\s*==\s*$", source)
    if start:
        source = source[start.end() :]
    stop = re.search(r"(?im)^==\s*(?:finalists|local winners)\s*==\s*$", source)
    if stop:
        source = source[: stop.start()]

    titles: list[str] = []
    for gallery in re.findall(r"(?is)<gallery\b[^>]*>(.*?)</gallery>", source):
        for line in gallery.splitlines():
            candidate = line.split("|", 1)[0].strip()
            candidate = re.sub(r"^(?:File|Image):", "", candidate, flags=re.I)
            if re.search(r"\.(?:jpe?g|png|webp|tiff?)$", candidate, re.I):
                titles.append("File:" + candidate.replace("_", " "))
    return list(dict.fromkeys(titles))


def plain(value: str | None) -> str | None:
    if not value:
        return None
    value = re.sub(r"<br\s*/?>", "\n", value, flags=re.I)
    value = re.sub(r"<[^>]+>", "", value)
    return html.unescape(value).strip()


def ext_value(metadata: dict, key: str) -> str | None:
    item = metadata.get(key)
    return item.get("value") if isinstance(item, dict) else None


def chunks(items: list[dict], size: int):
    for offset in range(0, len(items), size):
        yield items[offset : offset + size]


def download(url: str, destination: Path) -> None:
    if destination.exists():
        return
    temporary = destination.with_suffix(destination.suffix + ".part")
    request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    for attempt in range(6):
        try:
            started = time.monotonic()
            transferred = 0
            with urllib.request.urlopen(request, timeout=180) as response, temporary.open("wb") as output:
                while block := response.read(256 * 1024):
                    output.write(block)
                    transferred += len(block)
                    # Stay below Wikimedia's 25 Mbps aggregate media limit.
                    expected = transferred / (1.0 * 1024 * 1024)
                    if expected > time.monotonic() - started:
                        time.sleep(expected - (time.monotonic() - started))
            temporary.replace(destination)
            time.sleep(2)
            return
        except urllib.error.HTTPError as error:
            if attempt == 5 or error.code != 429:
                raise
            delay = int(error.headers.get("Retry-After", 60))
            print(f"media rate limit; waiting {delay}s", flush=True)
            time.sleep(delay)
    raise AssertionError("unreachable")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path, default=Path("untracked/backgrounds/wiki-loves-earth"))
    parser.add_argument("--first-year", type=int, default=2013)
    parser.add_argument("--last-year", type=int, default=2025)
    args = parser.parse_args()

    args.output.mkdir(parents=True, exist_ok=True)
    image_dir = args.output / "originals"
    image_dir.mkdir(exist_ok=True)

    selections: list[dict] = []
    for year in range(args.first_year, args.last_year + 1):
        titles = winner_titles(year)
        print(f"{year}: {len(titles)} winners", flush=True)
        source = f"Commons:Wiki Loves Earth {year}" + ("/Winners" if year > 2013 else "")
        selections.extend({"year": year, "source": source, "title": title} for title in titles)

    # A title can win in more than one year; download it once but retain every selection.
    unique = list(dict.fromkeys(item["title"] for item in selections))
    selection_path = args.output / "selection.jsonl"
    with selection_path.open("w", encoding="utf-8") as output:
        for item in selections:
            output.write(json.dumps(item, ensure_ascii=False) + "\n")

    records: list[dict] = []
    metadata_path = args.output / "metadata.jsonl"
    for batch_number, title_batch in enumerate(chunks(unique, BATCH_SIZE), 1):
        data = api(
            action="query",
            prop="imageinfo",
            titles="|".join(title_batch),
            iiprop="url|size|mime|sha1|extmetadata",
            iiextmetadatalanguage="en",
        )
        for page in data["query"]["pages"]:
            if "imageinfo" not in page:
                print(f"missing: {page['title']}", flush=True)
                continue
            info = page["imageinfo"][0]
            metadata = info.get("extmetadata", {})
            suffix = Path(urllib.parse.unquote(urllib.parse.urlparse(info["url"]).path)).suffix
            filename = f"{page['pageid']}_{info['sha1'][:12]}{suffix.lower()}"
            destination = image_dir / filename
            download(info["url"], destination)
            records.append(
                {
                    "commons_title": page["title"],
                    "commons_page": info["descriptionurl"],
                    "local_file": f"originals/{filename}",
                    "sha1": info["sha1"],
                    "mime": info.get("mime"),
                    "original_width": info.get("width"),
                    "original_height": info.get("height"),
                    "original_url": info["url"],
                    "description": plain(ext_value(metadata, "ImageDescription")),
                    "artist": plain(ext_value(metadata, "Artist")),
                    "credit": plain(ext_value(metadata, "Credit")),
                    "license": plain(ext_value(metadata, "LicenseShortName")),
                    "license_url": ext_value(metadata, "LicenseUrl"),
                    "extmetadata": metadata,
                }
            )
        with metadata_path.open("w", encoding="utf-8") as output:
            for record in records:
                output.write(json.dumps(record, ensure_ascii=False) + "\n")
        print(f"metadata batch {batch_number}: {len(records)}/{len(unique)} downloaded", flush=True)

    print(f"Done: {len(records)} images in {image_dir}")


if __name__ == "__main__":
    main()
