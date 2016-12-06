#!/usr/bin/env bash

# --------------------------------------------
# @doc Arboreus Library service script
# Only for BASH usage, no DASH compatibility
# @author Alexandr KIRILOV (http://alexandr.kirilov.me)
# @copyright (C) 2015, Arboreus Library, http://arboreus.system
# --------------------------------------------

# --------------------------------------------
# Check required variables and parameters
# --------------------------------------------
function require(){
	if [ -z ${USER_NAME+x} ]; then variable_error "USER_NAME"; fi
	if [ -z ${NODE_TEST_NAME+x} ]; then variable_error "NODE_API_TEST_NAME"; fi
	if [ -z ${NODE_NAME+x} ]; then variable_error "NODE_API_NAME"; fi
	if [ -z ${DIR_ROOT+x} ]; then variable_error "DIR_ROOT"; fi
	if [ -z ${DIR_SRC+x} ]; then variable_error "DIR_SRC"; fi
	if [ -z ${DIR_BIN+x} ]; then variable_error "DIR_BIN"; fi
	if [ -z ${DIR_OLD_BIN+x} ]; then variable_error "DIR_OLD_BIN"; fi
	if [ ! -d "$DIR_ROOT" ]; then
		printf "Error: no defined root direcory, check configuration file";
		exit 1
	fi
}

# --------------------------------------------
# Variables checking error handler
# --------------------------------------------
function variable_error(){
	if [ $1 ]; then
		printf "Error: variable $1 does not set, check configuration file\n";
		exit 1;
	else
		printf "Error: execution error_var(), no id_var\n";
		exit 1;
	fi
}

# --------------------------------------------
# Checking exclusions
# --------------------------------------------
function exclusion(){
	if [ $1 == "--dir" ];
	then
		if [ ! -z ${EXCLUSION_DIRS+x} ];
		then
			for DIR in "${EXCLUSION_DIRS[@]}"
			do
				if [ "$DIR" == "$2" ];
				then CHECK="true"; break;
				else CHECK="false"; fi
			done
		else
			CHECK="false";
		fi
	elif [ $1 == "--file" ];
	then
		if [ ! -z ${EXCLUSION_FILES+x} ];
		then
			for FILE in "${EXCLUSION_FILES[@]}"
			do
				if [ "$FILE" == "$2" ];
				then CHECK="true"; break;
				else CHECK="false"; fi
			done
		else
			CHECK="false";
		fi
	fi
	if [ "$CHECK" == "false" ];
	then echo "false"; else echo "true"; fi
}

# --------------------------------------------
# Project compilation
# --------------------------------------------
function compile(){
	printf "Compilation started\n"
	if [ -d "$DIR_BIN" ];
	then
		if [ -d "$DIR_OLD_BIN" ];
		then
			rm -rf ${DIR_OLD_BIN}/*;
		fi
		if [ ! -d "$DIR_OLD_BIN" ];
		then
			mkdir -p ${DIR_OLD_BIN};
		fi
		mv ${DIR_BIN}/* ${DIR_OLD_BIN}/;
		mkdir -p ${DIR_BIN};
	else
		mkdir -p ${DIR_BIN};
	fi
	cd ${DIR_SRC};
	FILES_ERL=$(find . -name '*.erl');
	for FILE_PATH_SOURCE in ${FILES_ERL}
	do
		FILE_PATH=${FILE_PATH_SOURCE:1};
		IFS="/" read -ra PATH_DEVIDED <<< "$FILE_PATH";
		FILE_NAME_POSITION=$((${#PATH_DEVIDED[@]}-1));
		FILE_NAME=${PATH_DEVIDED[$FILE_NAME_POSITION]};
		MODULE_NAME=$(echo $FILE_NAME | sed "s/.erl$//");
		RELATIVE_PATH=$(echo $FILE_PATH | sed "s/\/$FILE_NAME$//");
		EXCLUSION_DIR=$(exclusion --dir ${RELATIVE_PATH});
		EXCLUSION_FILE=$(exclusion --file ${RELATIVE_PATH}${FILE_NAME});
		if [ ${EXCLUSION_DIR} == "false" ]; then
			if [ ${EXCLUSION_FILE} == "false" ]; then
				if [ ! -d "$DIR_BIN$RELATIVE_PATH" ]; then
					mkdir -p ${DIR_BIN}${RELATIVE_PATH};
				fi
				COMPILATION_OUT=$(erlc -W2 -o ${DIR_BIN}${RELATIVE_PATH} ${DIR_SRC}${FILE_PATH} 2>&1);
				if [ $? -eq "1" ]; then
					echo "Compilation ERR: $COMPILATION_OUT";
					exit 2;
				fi
				printf "Done! $FILE_PATH\n"
			fi
		fi
	done
	printf "***\nDone!\n\n";
}

# --------------------------------------------
# File compilation
# --------------------------------------------
function compile_file(){
	printf "File compilation started\n"
	if [ ! -f $1 ];
	then
		if [ ! -f ${DIR_SRC}$1 ];
			then
				printf "Error: no defined file for compilation\n";
				exit 1;
			else
				FILE_PATH=${DIR_SRC}$1;
				IFS="/" read -ra PATH_DEVIDED <<< "$1";
				FILE_NAME_POSITION=$((${#PATH_DEVIDED[@]}-1));
				FILE_NAME=${PATH_DEVIDED[$FILE_NAME_POSITION]};
				RELATIVE_PATH=$(echo $1 | sed "s/\/$FILE_NAME$//");
		fi
	else
		FILE_PATH=$1
		IFS="/" read -ra PATH_DEVIDED <<< "$1";
		FILE_NAME_POSITION=$((${#PATH_DEVIDED[@]}-1));
		FILE_NAME=${PATH_DEVIDED[$FILE_NAME_POSITION]};
		PATH_FROM_SRC=$(echo "$1" | sed "s/^[$DIR_SRC]*//");
		RELATIVE_PATH=$(echo "/$PATH_FROM_SRC" | sed "s/\/$FILE_NAME$//");
	fi
	EXCLUSION_FILE=$(exclusion --file ${DIR_SRC}${RELATIVE_PATH}"/"${FILE_NAME});
	if [ ${EXCLUSION_FILE} == "true" ];
	then
		printf "Error: file for compilation excluded\n";
		exit 1;
	fi
	MODULE_NAME=$(echo $FILE_NAME | sed "s/.erl$//");
	BINARY_FILE=${DIR_BIN}${RELATIVE_PATH}"/"${MODULE_NAME}".beam";
	OLD_BINARY_DIR=${DIR_OLD_BIN}${RELATIVE_PATH};
	OLD_BINARY_FILE=${OLD_BINARY_DIR}"/"${MODULE_NAME}".beam"
	if [ ! -d ${DIR_BIN}${RELATIVE_PATH} ]; then mkdir -p ${DIR_BIN}${RELATIVE_PATH}; fi
	if [ ! -d ${OLD_BINARY_DIR} ]; then mkdir -p ${OLD_BINARY_DIR}; fi
	if [ -f ${BINARY_FILE} ]; then mv ${BINARY_FILE} ${OLD_BINARY_FILE}; fi
	COMPILATION_OUT=$(erlc -W2 -o ${DIR_BIN}${RELATIVE_PATH} ${DIR_SRC}${FILE_PATH} 2>&1);
	if [ $? -eq "1" ]; then
		echo "Compilation ERR: $COMPILATION_OUT";
		exit 2;
	fi
	printf "***\nDone! $FILE_PATH\n\n"
}

# --------------------------------------------
# Rollback project
# --------------------------------------------
function rollback {
	printf "Rollback started\n"
	if [ -d "$DIR_OLD_BIN" ];
	then
		rm -rf ${DIR_BIN}
		mv ${DIR_OLD_BIN} ${DIR_BIN}
		reload;
	else
		echo "No old binaries to restore"
	fi
}

# --------------------------------------------
# Update project
# --------------------------------------------
function update(){
	printf "Erlang source updating, started\n"
	cd ${DIR_ROOT}
	echo ${DIR_ROOT}
	git pull
	printf "***\nDone!\n\n";
}

# --------------------------------------------
# Reload application
# --------------------------------------------
function reload(){
	if [ -d ${DIR_BIN} ];
	then
		printf "Binaries reloading started\n"
		cd ${DIR_BIN};
		FILES_BIN=$(find . -name '*.beam');
		for FILE_PATH_BIN in ${FILES_BIN}
		do
			FILE_PATH=${FILE_PATH_BIN:1};
			IFS="/" read -ra PATH_DEVIDED <<< "$FILE_PATH";
			FILE_NAME_POSITION=$((${#PATH_DEVIDED[@]}-1));
			FILE_NAME=${PATH_DEVIDED[$FILE_NAME_POSITION]};
			MODULE_NAME=$(echo $FILE_NAME | sed "s/.beam$//");
			YAWS_ANSWER=$(load_to_yaws ${MODULE_NAME});
			printf "Module: $MODULE_NAME -> Yaws: $YAWS_ANSWER\n";
		done
		printf "***\nDone!\n\n"
	else
		printf "Error: no binary directory\n";
		exit 1
	fi
}

# --------------------------------------------
# Upgrade application
# --------------------------------------------
function upgrade(){
	if [ $1 ]; then
		if [ "$1" == "full" ];
		then
			update;
		else
			printf "Error: wrong upgrade parameter\n";
			exit 1;
		fi
	fi
	compile;
	reload;
	printf "***\nDone! Application $PROJECT_NAME\n"
}

# --------------------------------------------
# Script help
# --------------------------------------------
function help(){
	printf "Script usage:\n\n";
	printf "\t $ bash service.sh --help\n\t\tshow help\n";
	printf "\t $ bash service.sh --conf /path/to/conf --do compile\n\t\tcompile all erlang sources\n";
	printf "\t $ bash service.sh --conf /path/to/conf --do compile_file /path/to/file\n\t\tcompile defined file\n";
	printf "\t $ bash service.sh --conf /path/to/conf --do update\n\t\tupdate repository within sources\n";
	printf "\t $ bash service.sh --conf /path/to/conf --do rollback\n\t\trollback to previous binaries\n";
	printf "\t $ bash service.sh --conf /path/to/conf --do reload\n\t\treload binaries to Yaws\n";
	printf "\t $ bash service.sh --conf /path/to/conf --do upgrade\n\t\tfull upgrade application within update repository\n";
}

# --------------------------------------------
# Script actions
# --------------------------------------------
if [ $1 ];
then
	if [ $1 == "--conf" ];
	then
		if [ ! -f $2 ];
		then
			printf "Error: no defined configuration file: $2\n";
		else
			source $2;
			require;
			if [ $(whoami) != ${USER_NAME} ]; then
				printf "Error: current user have no privileges to run this script\n";
				exit 1;
			fi
			if [ ! $3 ]; then
				printf "Error: no action parameter\n";
				exit 1;
			fi
			if [ $3 == "--do" ]
			then
				if [ ! $4 ]; then
					printf "Error: action must be defined\n";
					exit 1;
				fi
				if [ $4 == "compile" ]; then compile;
				elif [ $4 == "update" ]; then update;
				elif [ $4 == "rollback" ]; then rollback;
				elif [ $4 == "reload" ]; then reload;
				elif [ $4 == "upgrade" ];
				then
					if [ $5 ];
					then upgrade $5;
					else upgrade;
					fi
				elif [ $4 == "compile_file" ];
				then
					if [ $5 ];
					then compile_file $5;
					else printf "Error: no file definition\n";
					fi
				else
					printf "Error: wrong action defined\n";
				fi
			else
				printf "Error: action parameter wrong\n";
			fi
		fi
	elif [ $1 == "--help" ]; then help;
	elif [ $1 == "--test" ]; then echo "ok";
	else
		printf "Error: you should define configuration file at first\n";
	fi
else
	printf "Error: no any parameters found\n";
fi