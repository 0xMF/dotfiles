# .kshrc 
#-----------------------------------------------------------------

#
REPO=~/.sh

#
print .kshrc called ...

[ -f $HOME/.profile ] && . ~/.profile
PATH=$PATH:/usr/local/bin:.:~/bin

# source local and private settings
# changes to local.bash should not be publicly tracked and shared (recommended)
if [ -f $REPO/local.sh ]; then
  . $REPO/local.sh
fi

# source common aliases used by power users
if [ -f $REPO/aliases ]; then
  . $REPO/aliases
fi

# source environment variables exported
if [ -f $REPO/exports ]; then
  . $REPO/exports
fi

# source various utility functions
if [ -f $REPO/functions ]; then
  . $REPO/functions
fi


# setup our prompt PS1, first get OS release+version
OSRV=
if [ $(uname -o) == "GNU/Linux" ]; then
  if [ $(cat /etc/*-release|wc -l) -eq 1 ]; then
      OSRV=$(cat /etc/*-release)
  else
      OSRV=$(cat /etc/lsb-release|grep DESCRIPTION|sed -e 's/.*=//;s/\"//g')
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

# vim:nospell:ft=sh:
