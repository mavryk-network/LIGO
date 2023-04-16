YARN_PKG_CONFIG_BIN_PATH=$1

PATH_V=$(echo "$YARN_PKG_CONFIG_BIN_PATH;$PATH" | sed 's/;;/;/g')
env PATH=$(cygpath -u "$PATH_V") dune build -p class_group_vdf
