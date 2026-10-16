import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
FRESHEN = ROOT.parent / ".local/bin/freshen"


class AptPackageTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.home = Path(self.temporary.name)
        self.binaries = self.home / "bin"
        self.binaries.mkdir()
        (self.home / "env/nix/scripts").mkdir(parents=True)
        shutil.copyfile(ROOT / "apt-packages.md", self.home / "env/apt-packages.md")
        self.log = self.home / "commands.jsonl"
        command = f"#!{sys.executable}\n" + '''
import json
import os
from pathlib import Path
import sys

name = Path(sys.argv[0]).name
with open(os.environ["TEST_COMMAND_LOG"], "a") as log:
    log.write(json.dumps([name, *sys.argv[1:]]) + "\\n")
'''
        for path in (self.binaries / "sudo",
                     self.home / "env/nix/scripts/freshen-nix.sh"):
            path.write_text(command)
            path.chmod(0o755)
        self.environment = os.environ.copy()
        self.environment.update({
            "HOME": str(self.home), "PATH": f"{self.binaries}:/usr/bin:/bin",
            "TEST_COMMAND_LOG": str(self.log),
        })

    def run_script(self):
        if not FRESHEN.exists():
            self.skipTest(f"{FRESHEN} is unavailable")
        result = subprocess.run(["bash", str(FRESHEN)], env=self.environment,
                                capture_output=True, text=True)
        commands = [json.loads(line) for line in self.log.read_text().splitlines()] \
            if self.log.exists() else []
        return result, commands

    def test_freshen_installs_programs_and_shoopdaloop_dependencies_before_upgrade(self):
        result, commands = self.run_script()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(commands[0], ["sudo", "apt-get", "update"])
        self.assertEqual(commands[1][:4], ["sudo", "apt-get", "install", "--yes"])
        packages = commands[1][4:]
        for package in ("gcc", "clang", "pkgconf", "swaylock", "libjack-jackd2-dev",
                        "libxcb-xfixes0-dev", "libxkbcommon-dev"):
            self.assertIn(package, packages)
        self.assertNotIn("spotify-client", packages)
        self.assertEqual(len(packages), 22)
        self.assertEqual(commands[2], ["sudo", "apt-get", "upgrade", "--yes"])
        self.assertEqual(commands[3], ["freshen-nix.sh"])

    def test_empty_list_skips_install(self):
        (self.home / "env/apt-packages.md").write_text("# No packages\n\n")
        result, commands = self.run_script()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(any("install" in command for command in commands))

    def test_missing_list_prevents_package_mutations(self):
        (self.home / "env/apt-packages.md").unlink()
        result, commands = self.run_script()
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(commands, [])


if __name__ == "__main__":
    unittest.main()
