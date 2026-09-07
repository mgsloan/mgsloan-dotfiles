fn main() {
    pkg_config::probe_library("libsystemd").expect("libsystemd development files are required");
}
