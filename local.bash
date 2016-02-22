#!/usr/bin/env bash
#

REPO=$HOME/.bash

# local (and private) settings may go here

# Source local definitions
if [ -f $REPO/secrets.bash ]; then
	. $REPO/secrets.bash
fi
