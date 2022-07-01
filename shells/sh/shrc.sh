# .shrc
#-----------------------------------------------------------------

#
export SHELL_REPO=~/.sh

#
print .shrc called ...

[ -f $HOME/.profile ] && . ~/.profile
PATH=$PATH:/usr/local/bin:~/bin

# source various utility functions
if [ -f $SHELL_REPO/functions ]; then
  . $SHELL_REPO/functions
fi

# source common aliases used by power users
if [ -f $SHELL_REPO/aliases ]; then
  . $SHELL_REPO/aliases
fi

# source environment variables exported
if [ -f $SHELL_REPO/exports ]; then
  . $SHELL_REPO/exports
fi

# source local and private settings
# setup our prompt PS1, first get OS release+version
OSRV=
if [ $(uname -o) == "GNU/Linux" ]; then
  if [ $(cat /etc/*-release|wc -l) -eq 1 ]; then
      OSRV=$(cat /etc/*-release)
  else
      OSRV=$(cat /etc/lsb-release|grep -w DESCRIPTION|sed -e 's/.*=//;s/\"//g')
  fi
fi
if [ $(uname -o) == "FreeBSD" ]; then
    OSRV=$(uname -sr)
fi

set -o emacs             # vi-style editing
bind -m '^L'=clear'^J'   # clear the screen
FCEDIT='/usr/bin/vim'    # fc usese vi too

# resembles the bash equivalent of '\w$ ' with green colour highlighting
# next vary prompt according to regular user or root
export PS1=`print '\e[0m\e[32;1m$(basename $(echo $PWD|sed "s,^$HOME$,~," ))\e[0m$ '`

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

# changes to local.sh should not be publicly tracked and shared (recommended)
if [ -f $SHELL_REPO/local.sh ]; then
  . $SHELL_REPO/local.sh
fi

# vim:nospell:ft=sh:
