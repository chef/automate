#!/bin/bash

# builds Cargo.toml for linux and macos
# and only includes files in src/bin/*/*.rs which
# are not excluded by a #!cfg(target_os="...") attribute

cd "$(dirname "$0")"

# for os in linux macos; do
	# echo "creating Cargo.toml for "
	mkdir -p /tmp/chef-load.toml
	# cp Cargo.toml $os/Cargo.toml
	# echo "" >> $os/Cargo.toml
	# for l in `ls src/bin/*/*.rs`; do
	# 	# check if the file is legit for this os (e.g. has target_os=$os in first line or has no target attribute)
	# 	m=`head -1 $l | grep -vE "target_os.*=.*$os" | grep -E '^#!\[cfg\(.*target_os'`;
	# 	if [ "$m" == "" ]; then 
	# 		bin=${l//src\/bin\/[^\/]*\//}
	# 		bin=${bin//.rs/}
	# 		echo "[[bin]]"
	# 		echo name=\"$bin\"
	# 		echo path = \"../$l\"
	# 		echo
	# 	fi;
	# done >> $os/Cargo.toml
# done

# if [ `uname -s` == "Darwin" ]; then
# 	echo "building for Macos"
# 	cargo build --manifest-path macos/Cargo.toml
# elif [ `uname -s` == "Linux" ]; then
# 	echo "building for Linux"
# 	cargo build --manifest-path linux/Cargo.toml
# fi