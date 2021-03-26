#! /bin/bash

set -o nounset
set -o errexit

# This ensures we have an absolute path to dotfiles repo
readonly dotfiles_dir_path=$( cd "$( dirname "$0" )" && pwd )

main() {
    parse_arguments "$@"
    deploy_dotfiles
	deploy_emacs_libraries
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
	echo "Deploying dotfiles..."
    for dot_file in $(find $dotfiles_dir_path -type f -name ".*"); do
        if $symlink_command $dot_file ~/$(basename $dot_file) 2>/dev/null; then
            echo "  $(basename $dot_file): Created symlink in home directory"
        else
            echo "  $(basename $dot_file): Already exists"
        fi
    done
}

deploy_emacs_libraries() {
	echo "Deploying emacs libraries..."
	mkdir -p ~/.emacs.d/lisp
	for emacs_lib_file in $(find $dotfiles_dir_path/.emacs.d/lisp -type f -name "*.el"); do
		if $symlink_command $emacs_lib_file ~/.emacs.d/lisp/$(basename $emacs_lib_file) 2>/dev/null; then
			echo "  $(basename $emacs_lib_file): Created symlink in emacs lib directory"
		else
			echo "  $(basename $emacs_lib_file): Already exists"
		fi
	done
}

main "$@"
