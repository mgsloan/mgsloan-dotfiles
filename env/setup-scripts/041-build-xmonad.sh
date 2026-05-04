#!/bin/bash -e

# The Haskell pcre-light package (pulled in via rex) links against PCRE1.
# Debian forky/sid removed libpcre3 in favor of libpcre2, so when apt has no
# candidate, fall back to the .deb files still available in the Debian pool.
if ! dpkg -s libpcre3-dev >/dev/null 2>&1; then
  if [ -n "$(apt-cache madison libpcre3-dev 2>/dev/null)" ]; then
    sudo apt install --yes libpcre3-dev
  else
    echo "libpcre3-dev not in apt sources; fetching from Debian pool"
    PCRE_VER=8.39-15
    POOL=http://ftp.debian.org/debian/pool/main/p/pcre3
    TMP=$(mktemp -d)
    trap "rm -rf '$TMP'" EXIT
    for pkg in libpcre3 libpcre16-3 libpcre32-3 libpcrecpp0v5 libpcre3-dev; do
      curl -fsSL -o "$TMP/${pkg}_${PCRE_VER}_amd64.deb" \
        "$POOL/${pkg}_${PCRE_VER}_amd64.deb"
    done
    sudo apt install --yes \
      "$TMP/libpcre3_${PCRE_VER}_amd64.deb" \
      "$TMP/libpcre16-3_${PCRE_VER}_amd64.deb" \
      "$TMP/libpcre32-3_${PCRE_VER}_amd64.deb" \
      "$TMP/libpcrecpp0v5_${PCRE_VER}_amd64.deb" \
      "$TMP/libpcre3-dev_${PCRE_VER}_amd64.deb"
  fi
fi

~/env/scripts/rebuild.sh
