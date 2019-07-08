#!/bin/bash
#
# copy_hartifacts: move Habitat harts and keys into /hab/cache
#
# Usage:
#
#  copy_hartifacts "results/"
#
src="$1"
pkg_dst="/hab/cache/artifacts"
key_dst="/hab/cache/keys"

echo "Copying harts from $src to $pkg_dst"
echo "Copying keys from $src to $key_dst"
echo "Current working directory is: $(pwd)"

# safe_copy_to_dir takes a target file and a destination directory.
# It copies the file into the directory using a copy&rename strategy
# to try to avoid corruption when run concurrently.
safe_copy_to_dir() {
    local src=$1
    local dst=$2
    local name
    local tmp_dst
    name=$(basename "$src")
    tmp_dst=$(mktemp "$dst/.safe_copy-XXXXXXX")
    final_dst=$dst/$name

    echo "Moving $name from $src to $final_dst"
    echo " - sha256 before move: $(sha256sum "$src")"
    cp "$src" "$tmp_dst"
    mv "$tmp_dst" "$final_dst"
    echo "- sha256 after: $(sha256sum "$final_dst")"
}

if [[ ! -d "$src" ]]; then
    echo "$src is not a directory, not copying hartifacts"
    exit
else
    echo "$src is a directory"
fi

previous_umask=$(umask)
umask 022
mkdir -p "$pkg_dst"
mkdir -p "$key_dst"
umask "$previous_umask"

if [[ -n "$(ls -A "$src/"*.hart)" ]]; then
   for hart_path in "$src/"*.hart; do
       safe_copy_to_dir "$hart_path" "$pkg_dst"
   done
else
    echo "No harts in $src"
fi

if [[ -n "$(ls -A "$src/"*.pub)" ]]; then
    for key_path in "$src/"*.pub; do
        safe_copy_to_dir "$key_path" "$key_dst"
    done
else
    echo "No public keys in $src"
fi
