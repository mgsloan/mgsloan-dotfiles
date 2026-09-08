import importlib.util
from pathlib import Path
import tempfile
import unittest


specification = importlib.util.spec_from_file_location(
    "nix_services", Path(__file__).resolve().parents[1] / "setup/050-nix-services.py")
nix_services = importlib.util.module_from_spec(specification)
specification.loader.exec_module(nix_services)


class ServiceLinkTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.home = Path(self.temporary.name)
        self.store = self.home / "store"
        self.store.mkdir()
        self.source = self.store / "service"
        self.source.write_text("service")
        self.destination = self.home / "service"
        self.links = {self.destination: self.source}

    def test_missing_destination_and_existing_managed_link_are_allowed(self):
        nix_services.check_links(self.links, store=self.store)
        self.destination.symlink_to(self.source)
        nix_services.check_links(self.links, store=self.store)

    def test_existing_user_file_is_preserved(self):
        self.destination.write_text("user configuration")
        with self.assertRaisesRegex(ValueError, "existing user file"):
            nix_services.check_links(self.links, store=self.store)
        self.assertEqual(self.destination.read_text(), "user configuration")

    def test_existing_unrelated_symlink_is_preserved(self):
        self.destination.symlink_to("missing-service")
        with self.assertRaisesRegex(ValueError, "existing user file"):
            nix_services.check_links(self.links, store=self.store)
        self.assertEqual(self.destination.readlink(), Path("missing-service"))

    def test_source_outside_store_is_rejected(self):
        source = self.home / "non-nix-service"
        source.write_text("service")
        with self.assertRaisesRegex(ValueError, "activate"):
            nix_services.check_links({self.destination: source}, store=self.store)
        self.assertFalse(self.destination.exists())


if __name__ == "__main__":
    unittest.main()
