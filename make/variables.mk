# ----------------------------------------------
# Arboreus make file variables
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------


# Root directories pathes
ROOT_DIR = $(PWD)
ROOT_DIR_EBIN = $(PWD)/ebin
ROOT_DIR_BACKUP = $(PWD)/backup
ROOT_DIR_SRC = $(PWD)/src
ROOT_DIR_MAKE = $(PWD)/make

# Github directory
GITHUB_DIR = $(PWD)/../../Github/Fw_library

# Modules
MODULES += universal
MODULES += yaws_addons
MODULES += handlers
MODULES += behaviours
MODULES += a_loger
MODULES += a_structure_tree
MODULES += a_event_storage

# Environment variables
ERLANG_SETTINGS_FILE = ~/.erlang