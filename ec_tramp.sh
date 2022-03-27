#!/bin/bash -f
# Set this script as the "EDITOR" environment variable
# Set the SERVER_FILE variable to wherever the server authetication
# file is located
#
# For versions of emacsclient prior to 26.1 we need
# to manually add the tramp prefix to the file we are editing
# /ssh:<hostname>:
# For versions >= 26.1 we can just use the -T option

# wherever the server file is supposed to be
SERVER_FILE=~/emacs_server_file

FILE=$(realpath $1)
HOST=$(hostname)


# command
COMMAND="emacsclient -f ${SERVER_FILE} /ssh:${HOST}:${FILE}"
echo "FILE = $FILE"
echo "COMMAND = ${COMMAND}"
eval "$COMMAND"
