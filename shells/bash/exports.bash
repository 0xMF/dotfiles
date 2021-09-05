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
  export LC_ALL=en_US.UTF-8
  #export LC_ALL=C
  export LC_CTYPE=en_US.UTF-8
  export LC_COLLATE=C
  export LANG=en_US.UTF-8
  export LANGUAGE=en_US.UTF-8
  TODAY=$(date +"%Y-%b-%d"); [ -n "${TODAY}" ] && export TODAY
fi


# Handles: (-R) ANSI colors with ESC, (-FX) quit on one screen without destroying text,
#          (-q) quiet mode and (-e) terminate after second encounter with EOF
export LESS='FeqRSX'
export MANPAGER="less -$LESS"

[ "$(uname)" = "CYGWIN_NT-10.0" ] && export DISPLAY=:0.0


# build related exports
export ERL_TOP=$HOME/repos/otp

# vim:nospell:ft=sh:
