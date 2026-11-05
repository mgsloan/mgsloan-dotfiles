# Native selection is enabled only after all required packages are installed.
env_package_backend=${ENV_PACKAGE_BACKEND:-$(cat "$HOME/.local/state/debian-packages/backend" 2>/dev/null || printf nix)}
if [ "$env_package_backend" != debian ]; then
    . "$HOME/env/nix/scripts/nix-session.sh"
else
    env_package_path=""
    env_package_previous_ifs=$IFS
    IFS=:
    for env_package_directory in $PATH; do
        case "$env_package_directory" in
            /nix/store/*|/nix/var/nix/profiles/*|"$HOME/.nix-profile"/*|"$HOME/.local/state/nix/profiles/"*) ;;
            *) env_package_path="${env_package_path:+$env_package_path:}$env_package_directory" ;;
        esac
    done
    PATH="$HOME/env/bin:$HOME/.local/bin:$env_package_path"
    env_package_data=""
    for env_package_directory in ${XDG_DATA_DIRS:-/usr/local/share:/usr/share}; do
        case "$env_package_directory" in
            /nix/store/*|/nix/var/nix/profiles/*|"$HOME/.nix-profile"/*|"$HOME/.local/state/nix/profiles/"*) ;;
            *) env_package_data="${env_package_data:+$env_package_data:}$env_package_directory" ;;
        esac
    done
    XDG_DATA_DIRS=${env_package_data:-/usr/local/share:/usr/share}
    IFS=$env_package_previous_ifs
    export PATH XDG_DATA_DIRS
    unset LD_LIBRARY_PATH LIBGL_DRIVERS_PATH LIBVA_DRIVERS_PATH GBM_BACKENDS_PATH \
        __EGL_VENDOR_LIBRARY_FILENAMES GHOSTTY_RESOURCES_DIR GIO_EXTRA_MODULES \
        GI_TYPELIB_PATH GDK_PIXBUF_MODULE_FILE LOCALE_ARCHIVE \
        GST_PLUGIN_SYSTEM_PATH_1_0 GST_PLUGIN_SYSTEM_PATH GST_PLUGIN_PATH_1_0 GST_PLUGIN_PATH \
        GHOSTTY_BIN_DIR TERMINFO
    unset env_package_path env_package_data env_package_directory env_package_previous_ifs
fi
unset env_package_backend
