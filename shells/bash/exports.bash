#!/usr/bin/env bash
#

THIS_SHELL=$(ps o command -p $$ | sed '/^COMMAND$/d' | tr -d '-' | cut -d' ' -f1)
case "${THIS_SHELL##/**/}" in
  bash|ksh|zsh) ;;
  *) >&2 echo "This script probably wont work with your shell, so bailing out now...bye!";
     exit 1;;
esac

# remove duplicate lines in the history; force ignoredups and ignorespace
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
export HISTCONTROL=ignoreboth

# terminal clients need local
if [[ -t 0 ]]; then
  #export LC_ALL=C
  LC_ALL=en_US.UTF-8; export LC_ALL
  LC_COLLATE=C; export LC_COLLATE
  LC_CTYPE=en_US.UTF-8; export LC_CTYPE
  LANG=en_US.UTF-8; export LANG
  LANGUAGE=en_US.UTF-8; export LANGUAGE
  TODAY=$(date +"%Y-%m-%d"); [ -n "${TODAY}" ] && export TODAY
fi


# make Bash work like Zsh/ksh
GLOBIGNORE='.:..'; export GLOBIGNORE

# Handles: (-R) ANSI colors with ESC, (-FX) quit on one screen without destroying text,
#          (-q) quiet mode and (-e) terminate after second encounter with EOF
LESS='FeqRSX'; export LESS
PAGER="less -${LESS}"; export PAGER
MANPAGER=$PAGER; export MANPAGER

if [ -s /usr/local/bin/vim ]; then
  VISUAL=/usr/local/bin/vim; export VISUAL
else
  if [ -s /usr/bin/vim ]; then
    VISUAL=/usr/bin/vim; export VISUAL
  else
    VISUAL=$(whereis vim | cut -d' ' -f2); export VISUAL
  fi
fi
EDITOR=$VISUAL; export EDITOR
FCEDIT=$VISUAL; export FCEDIT

# vim:nospell:ft=sh:
