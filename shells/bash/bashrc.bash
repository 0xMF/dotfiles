# .bashrc
#-----------------------------------------------------------------

#
REPO=$HOME/.bash

# don't do anything if we don't have a prompt (not an interactive shell)
[[ $- != *i* ]] && return || [ -z "$PS1" ] && return

# Source global definitions
if [ -f /etc/bashrc ]; then
  source /etc/bashrc
fi

# source local and private settings
# changes to local.bash should not be publicly tracked and shared (recommended)
if [ -f $REPO/local.bash ]; then
  source $REPO/local.bash
fi

# source environment variables exported
if [ -f $REPO/exports.bash ]; then
  source $REPO/exports.bash
fi

# source common aliases used by power users
if [ -f $REPO/aliases.bash ]; then
  source $REPO/aliases.bash
fi

# source various utility functions
if [ -f $REPO/functions.bash ]; then
  source $REPO/functions.bash
fi


# setup our prompt PS1, first get OS release+version
OSRV=
if [ $(uname -o) == "GNU/Linux" ]; then
  if [ -e /etc/os-release ]; then
    OSRV=$(grep "^ID=" /etc/os-release|awk -F'=' '{ print $NF }')
    OSRV=$(grep "PRETTY_NAME=" /etc/os-release|sed 's/"//g'|awk '{print $NF}')
    #OSRV=${OSRV}$(grep "^VERSION_ID=" /etc/os-release|awk -F'=' '{ print $NF }'|sed 's/"//g;s/^/ /')
    #OSRV=$(grep "^PRETTY_NAME=" /etc/os-release|awk -F' '  '{$1=""; print $0}'|sed 's/^ //;s/"$//')
    #OSRV=$(grep "^PRETTY_NAME=" /etc/os-release|awk -F' ' '{$1=""; ($2 == "GNU/Linux") && $2=""; print $0}'|sed 's/^ *//g;s/"$//')
    PRETTY_NAME=$(grep ^PRETTY_NAME /etc/os-release|cut -d'=' -f2|sed 's/"//g')
    NAME=$(grep ^NAME /etc/os-release|cut -d'=' -f2|sed 's/"//g')
    OSRV=$(echo ${PRETTY_NAME#${NAME}})

  else
    if [ $(cat /etc/*-release|wc -l) -eq 1 ]; then
        OSRV=$(cat /etc/*-release)
    else
        OSRV=$(cat /etc/lsb-release|grep DESCRIPTION|sed -e 's/.*=//;s/\"//g')
    fi
  fi
fi

OSRVT=$(echo $OSRV|sed 's/[a-zA-Z0-9  ]//g')
if [[ "$OSRVT" == "()" || -z "$OSRV" || "`hostname`" != "localhost" ]]; then
  OSRV=$(hostname)
fi
if [ $(uname -o) == "FreeBSD" ]; then
    OSRV=$(freebsd-version)
fi

# next vary prompt according to regular user or root
if [ $(/usr/bin/id -u) -ne 0 ]; then
  psl 2>/dev/null
  if [ $? -eq 0 ]; then
    PROMPT_COMMAND="psl"
  else
    if [ $(/usr/bin/id -u) -eq 1000 ]; then
      PS1="$GREEN${OSRV}$BLUE:\W$(parse_git_branch_colour 2>/dev/null)$NOCOLOR$ "
    else
      PS1="$PURPLE${OSRV}$BLUE:\W$(parse_git_branch_colour 2>/dev/null)$NOCOLOR$ "
    fi
  fi
else
  PS1="$RED${OSRV}$BLUE:\W$(parse_git_branch_colour 2>/dev/null)$NOCOLOR# "
fi

# editor settings for bash
set -o emacs
shopt -s extglob

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# override system umask settings because they are nonsense in some distros
umask 0022

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# command line calendar
# https://github.com/0xMF/catholic/calendar
pal 2> /dev/null
if [ $? -ne 0 ]; then
  echo "Missing pal - the command line calendar." >&2
else
  echo "Ah! Good. You've got pal - the command line calendar." >&2
  echo "If you'd like a Catholic saints calendar, check out saints.pal in" >&2
  echo "    https://github.com/0xMF/catholic"  >&2
fi

if declare -f done_init > /dev/null
then
  done_init
fi

# vim:nospell:ft=sh:
