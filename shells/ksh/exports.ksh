#!/usr/bin/env bash
#

THIS_SHELL=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
case "${THIS_SHELL##/**/}" in
  bash|ksh|zsh) ;;
  *) >&2 echo "This script probably wont work with your shell, so bailing out now...bye!";
     exit 1;;
esac

# required for mosh
LC_CTYPE="en_US.UTF-8"

# required for hub (cli tool for github management)
#export BROWSER='links2 -no-g'
#export BROWSER='w3m -v -no-mouse -s -cookie -no-proxy'

# Handles: (-R) ANSI colors with ESC, (-FX) quit on one screen without destroying text,
#          (-q) quiet mode and (-e) terminate after second encounter with EOF
#export LESS='-iMRS -x2'
export LESS='FceqRSX'
export EDITOR="less -$LESS"
if [ -s /usr/local/bin/vim ]; then
  export VISUAL=/usr/local/bin/vim
fi

export MANPAGER=$EDITOR
export FCEDIT=$EDITOR
export PAGER=$EDITOR

# vim:nospell:ft=sh:
