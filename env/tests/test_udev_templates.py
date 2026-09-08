import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]


@unittest.skipUnless(shutil.which("envsubst"), "envsubst is required")
class UdevTemplateTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.directory = Path(self.temporary.name)
        for filename in ("generate.sh", "99-batify.rules.template"):
            shutil.copyfile(ROOT / "system/udev-rules" / filename, self.directory / filename)

    def generate(self, username):
        environment = os.environ.copy()
        environment["USER_NAME"] = username
        return subprocess.run(["bash", str(self.directory / "generate.sh")],
                              env=environment, capture_output=True, text=True)

    def test_substitutes_username_without_expanding_udev_variables(self):
        result = self.generate("test-user")
        self.assertEqual(result.returncode, 0, result.stderr)
        output = (self.directory / "99-batify.rules").read_text()
        self.assertEqual(output.count("/usr/local/bin/batify-notify test-user"), 5)
        self.assertEqual(output.count("$attr{capacity}"), 3)
        self.assertNotIn("${USER_NAME}", output)

    def test_rejects_missing_or_shell_sensitive_username_before_writing(self):
        for username in ("", "user;command", "user name", "$(command)"):
            with self.subTest(username=username):
                self.assertNotEqual(self.generate(username).returncode, 0)
                self.assertFalse((self.directory / "99-batify.rules").exists())


if __name__ == "__main__":
    unittest.main()
