#!/bin/bash -f
#
# User $EDITOR as a pager
# This is used primarily from within emacs shell buffers
# that cannot support a proper pager like less or more.
#
# To use
#   export PAGER=ec_pager.sh
#   export GIT_PAGER=ec_pager.sh
#

tmpfile=$(mktemp /tmp/ec_pager.XXXXXX)
trap "rm -f $tmpfile" EXIT

# read stdin into tmpfile
cat > "$tmpfile"

# start the editor
$EDITOR "$tmpfile"
