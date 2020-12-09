#! /bin/bash

set -o nounset
set -o errexit

# This ensures we have an absolute path to the dotfiles directory
readonly dotfiles_dir_path=$( cd "$( dirname "$0" )" && pwd )


main() {
    parse_arguments "$@"
    deploy_dotfiles
}

parse_arguments() {
    symlink_command="ln -s"

    while [[ $# -gt 0 ]]
    do
        local key="$1"
        case "$key" in
            -h|--help)
                usage
                exit 1
            ;;
            -f|--force)
                symlink_command="ln -sf"
            ;;
            *)
                pr_err "Unknown argument: ${key}\n"
                usage
                exit 1
            ;;
        esac
        shift
    done
}

usage () {
    cat <<EOF
Deploys dotfiles into the home directory as symlinks.

Usage: $0 [options]
Options:
    -h, --help         Print help and exit
    -f, --force        Force symlinks even when dotfiles exist
EOF
}

deploy_dotfiles() {
    for dot_file in $(find $dotfiles_dir_path -type f -name ".*"); do
        if $symlink_command $dot_file ~/$(basename $dot_file) 2>/dev/null; then
            echo "$(basename $dot_file): Created symlink in home directory"
        else
            echo "$(basename $dot_file): Already exists (re-run with -f to overwrite)"
        fi
    done
}

main "$@"

