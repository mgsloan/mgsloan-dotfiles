from pathlib import Path
import os
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]


class RiverSessionTests(unittest.TestCase):
    def run_session(self, nix):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            binaries = home / "bin"
            binaries.mkdir()
            (home / "env").symlink_to(ROOT, target_is_directory=True)
            init = home / ".config/river/init-test"
            init.parent.mkdir(parents=True)
            init.write_text('#!/bin/sh\nprintf "INIT_LD=%s\\n" "${LD_LIBRARY_PATH-unset}"\n'
                            'printf "INIT_GL=%s\\n" "${LIBGL_DRIVERS_PATH-unset}"\n')
            init.chmod(0o755)
            commands = {
                "systemctl": "exit 0",
                "readlink": 'echo "${TEST_RIVER_PATH}"',
                "river": '''
printf 'RIVER_LD=%s\\n' "${LD_LIBRARY_PATH-unset}"
if [ "$TEST_RIVER_NIX" = 1 ]; then
    export LD_LIBRARY_PATH=/nix/test/lib LIBGL_DRIVERS_PATH=/nix/test/dri
fi
exec /bin/sh -c "$4"
''',
            }
            for name, contents in commands.items():
                executable = binaries / name
                executable.write_text("#!/bin/sh\n" + contents + "\n")
                executable.chmod(0o755)
            environment = os.environ.copy()
            environment.update({
                "HOME": str(home), "PATH": f"{binaries}:/usr/bin:/bin",
                "XDG_CONFIG_HOME": str(home / ".config"),
                "NIX_ENV_PROFILE": str(home / "absent-profile"),
                "LD_LIBRARY_PATH": "/legacy/lib", "LIBGL_DRIVERS_PATH": "/legacy/dri",
                "TEST_RIVER_NIX": "1" if nix else "0",
                "TEST_RIVER_PATH": "/nix/store/test/bin/river" if nix else str(binaries / "river"),
            })
            subprocess.run(["bash", str(ROOT / "desktop/scripts/river-session.sh"), "init-test"],
                           env=environment, check=True)
            return (home / ".local/state/river-session-init-test.log").read_text(), home

    def test_nix_graphics_environment_does_not_reach_session_clients(self):
        output, _ = self.run_session(nix=True)
        self.assertIn("RIVER_LD=unset", output)
        self.assertIn("INIT_LD=unset", output)
        self.assertIn("INIT_GL=unset", output)

    def test_legacy_river_keeps_local_wlroots_path(self):
        output, home = self.run_session(nix=False)
        self.assertIn(f"RIVER_LD={home}/.local/lib:/legacy/lib", output)
        self.assertIn(f"INIT_LD={home}/.local/lib:/legacy/lib", output)


if __name__ == "__main__":
    unittest.main()
