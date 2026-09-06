import os
from pathlib import Path
import re
import subprocess
import tempfile
import unittest


HOME_REPOSITORY = Path(__file__).resolve().parents[2]


class GhosttyThemeTests(unittest.TestCase):
    def test_hooks_copy_theme_and_signal_only_matching_user_processes(self):
        for mode in ("dark", "light"):
            with self.subTest(mode=mode), tempfile.TemporaryDirectory() as temporary:
                home = Path(temporary)
                configuration = home / ".config/ghostty"
                configuration.mkdir(parents=True)
                (configuration / f"theme-{mode}").write_text(mode)
                binaries = home / "bin"
                binaries.mkdir()
                signal = binaries / "pkill"
                signal.write_text('#!/bin/sh\nprintf "%s\\n" "$@" > "$HOME/signal-arguments"\n')
                signal.chmod(0o755)
                environment = os.environ | {"HOME": str(home), "PATH": f"{binaries}:/usr/bin:/bin"}
                hook = HOME_REPOSITORY / f".data/{mode}-mode.d/ghostty-theme.sh"
                subprocess.run(["bash", str(hook)], env=environment, check=True)
                self.assertEqual((configuration / "theme").read_text(), mode)
                arguments = (home / "signal-arguments").read_text().splitlines()
                self.assertEqual(arguments[:4], ["-USR2", "-u", str(os.getuid()), "-x"])
                self.assertIsNotNone(re.fullmatch(arguments[4], "ghostty"))
                self.assertIsNotNone(re.fullmatch(arguments[4], ".ghostty-wrappe"))
                self.assertIsNone(re.fullmatch(arguments[4], "unrelated-ghostty"))


if __name__ == "__main__":
    unittest.main()
