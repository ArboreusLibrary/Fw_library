#!/usr/bin/env bash

# --------------------------------------------
# @doc Arboreus Library service handler configuration
# @author Alexandr KIRILOV (http://alexandr.kirilov.me)
# @copyright (C) 2015, http://arboreus.system
# --------------------------------------------

# System settings
USER_NAME="alexandr"
YAWS_RUN="sudo yaws"
PROJECT_NAME="Arboreus library"

# Erlang Node names
NODE_TEST_NAME="yaws_test"
NODE_NAME="yaws"

# Project directories
DIR_ROOT="/Users/alexandr/projects/Arboreus/Arboreus_fw_library";
DIR_SRC="${DIR_ROOT}/src";
DIR_BIN="${DIR_ROOT}/bin";
DIR_OLD_BIN="${DIR_BIN}_old";

# Directory and files exclusion
EXCLUSION_DIRS=(
	'/Gen_servers'
)
EXCLUSION_FILES=(

)
function load_to_yaws(){
	echo 'Fktrcfylh456' | sudo -S yaws --load $1
}