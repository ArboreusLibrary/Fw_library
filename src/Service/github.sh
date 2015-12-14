#!/usr/bin/env bash

# --------------------------------------------
# @doc Arboreus Library Github deploy script
# Only for BASH usage, no DASH compatibility
# @author Alexandr KIRILOV (http://alexandr.kirilov.me)
# @copyright (C) 2015, Arboreus Library, http://arboreus.system
# --------------------------------------------

# --------------------------------------------
# Synchronisation
# --------------------------------------------
function sync(){
	printf "Rsync started\n***\n";
	rsync -arv --delete-before ${DIR_SRC} ${DIR_GIT};
	printf "***\nDone!\n";
}

# --------------------------------------------
# Pull
# --------------------------------------------
function push(){
	printf "Push started\n***\n";
	cd ${DIR_ROOT_GIT};
	echo -n "Enter the message for commit [ENTER]: ";
	read MESSAGE;
	git add -A;
	git commit -m "$MESSAGE";
	git push origin master;
	printf "\n***\nDone!\n"
}

# --------------------------------------------
# Update
# --------------------------------------------
function update(){
	sync;
	push;
}

# --------------------------------------------
# Help
# --------------------------------------------
function help(){
	printf "Script usage:\n\n";
	printf "\t $ bash service.sh --help\n\t\tshow help\n";
	printf "\t $ bash service.sh --conf /path/to/conf --do sync\n\t\tsynchronise directories\n";
	printf "\t $ bash service.sh --conf /path/to/conf --do push\n\t\tpush to Github\n";
	printf "\t $ bash service.sh --conf /path/to/conf --do update\n\t\tsynchronise and push\n";
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
				if [ $4 == "update" ]; then update;
				elif [ $4 == "sync" ]; then sync;
				elif [ $4 == "push" ]; then push;
				else
					printf "Error: wrong action defined\n";
				fi
			else
				printf "Error: action parameter wrong\n";
			fi
		fi
	elif [ $1 == "--help" ]; then help;
	else
		printf "Error: you should define configuration file at first\n";
	fi
else
	printf "Error: no any parameters found\n";
fi