#!/bin/bash
#

GLOBIGNORE='.:..';          export GLOBIGNORE
PROMPT_COMMAND=;            export PROMPT_COMMAND
TODAY=$(date +"%Y-%m-%d");  export TODAY
PS1='$ '

shopt -s dotglob extglob
