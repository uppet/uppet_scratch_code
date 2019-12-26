
prepend_PATH() {
    export PATH=$1:$PATH
}

prepend_PATH ~/apps/bin
prepend_PATH ~/work/cc/depot_tools
