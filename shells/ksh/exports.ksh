#!/usr/bin/env bash
#

THIS_SHELL=$(ps o command -p $$ | sed '/^COMMAND$/d' | tr -d '-' | cut -d' ' -f1)
case "${THIS_SHELL##/**/}" in
  bash|ksh|zsh) ;;
  *) >&2 echo "This script probably wont work with your shell, so bailing out now...bye!";
     exit 1;;
esac

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
# terminal clients need local
if [[ -t 0 ]]; then
  # some scripts/settings might require/prefer C type collating sequences
  #export LC_ALL=C
  #export LC_COLLATE=C
  export LANGUAGE=en_US.UTF-8
  TODAY=$(date +"%Y-%b-%d"); [ -n "${TODAY}" ] && export TODAY
fi

# required for hub (cli tool for github management)
#export BROWSER='links2 -no-g'
#export BROWSER='w3m -v -no-mouse -s -cookie -no-proxy'

# Handles: (-R) ANSI colors with ESC, (-FX) quit on one screen without destroying text,
#          (-q) quiet mode and (-e) terminate after second encounter with EOF
#export LESS='-iMRS -x2'
export LESS='FceqRSX'
export EDITOR="less -${LESS}"
export MANPAGER=$EDITOR
export PAGER=$EDITOR

if [ -s /usr/local/bin/vim ]; then
  export VISUAL=/usr/local/bin/vim
else
  if [ -s /usr/bin/vim ]; then
    export VISUAL=/usr/bin/vim
  else
    export VISUAL=$(whereis vim | cut -d' ' -f2)
  fi
fi
export FCEDIT=$VISUAL

# vim:nospell:ft=sh:
