# .kshrc
#-----------------------------------------------------------------

# don't do anything if we don't have a prompt (not an interactive shell)
[[ $- != *i* ]] && return || [ -z "$PS1" ] && return

THIS_SHELL=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
case "${THIS_SHELL##/**/}" in
  bash|ksh|zsh) ;;
  *) >&2 echo "This script probably wont work with your shell, so bailing out now...bye!";
     exit 1;;
esac

#
REPO=~/.ksh

#
#print .kshrc called ...

# don't do anything if we don't have a prompt (not an interactive shell)
#[[ $- != *i* ]] && return || [ -z "$PS1" ] && return
[[ "`uname`" != "OpenBSD" ]] && { >&2 echo "Sorry! These KSH settings were tested on OpenBSD only."; return ; }

[ -f $HOME/.profile ] && . ~/.profile
PATH=$PATH:/usr/local/bin:~/bin

# source various utility functions
if [ -f $REPO/functions.ksh ]; then
  . $REPO/functions.ksh
fi

# source common aliases used by power users
if [ -f $REPO/aliases.ksh ]; then
  . $REPO/aliases.ksh
fi

# source environment variables exported
if [ -f $REPO/exports.ksh ]; then
  . $REPO/exports.ksh
fi

# setup our prompt PS1, first get OS release+version
OSRV=
case "$(uname)" in
  "OpenBSD" ) OSRV=$(uname) ;;
  * ) case "$(uname -o)" in
        "FreeBSD"   ) OSRV=$(uname -sr) ;;
        "GNU/Linux" ) if [[ $(cat /etc/*-release|wc -l) -eq 1 ]]; then
                        OSRV=$(cat /etc/*-release)
                      else
                        OSRV=$(cat /etc/lsb-release|grep -w DESCRIPTION|sed -e 's/.*=//;s/\"//g')
                      fi ;;
        * ) ;;
      esac
esac

set -o emacs             # vi-style editing
bind -m '^L'=clear'^J'   # clear the screen
if [ "$(uname)" = "OpenBSD" ]; then
  FCEDIT=$(whereis vim)    # fc uses vim if found (installed)
else
  FCEDIT=$(whereis vim | cut -d' ' -f2)
fi
if [ -z "$FCEDIT" ]; then
  FCEDIT='/usr/bin/vi'
fi


# resembles the bash equivalent of '\w$ ' with green colour highlighting
# next vary prompt according to regular user or root
if [ -f $REPO/0xMF/prompt.sh ]; then
  . $REPO/0xMF/prompt.sh
else
  print "WARNING: Missing dependency prompt.sh!" >&2
fi

if [ -f $REPO/0xMF/git.ksh ]; then
  . $REPO/0xMF/git.ksh
  psl
else
  export PS1=`print '\e[0m\e[32;1m$(basename $(echo $PWD|sed "s,^$HOME$,~," ))\e[0m% '`
  export PROMPT_COMMAND="ps1;$PROMPT_COMMAND"
  print "WARNING: Missing critical dependency git.ksh!" >&2
fi

[ -f $REPO/0xMF/pleasure.ksh ] \
  && . $REPO/0xMF/pleasure.ksh \
  || print "WARNING: Missing critical dependency pleasure.ksh!" >&2

[ -f $REPO/0xMF/sysadmin.ksh ] \
  && . $REPO/0xMF/sysadmin.ksh \
  || print "WARNING: Missing critical dependency sysadmin.ksh!" >&2

# command line calendar
pal 2> /dev/null
if [ $? -eq 0 ]; then
  echo "Ah! Good. You've got pal - the command line calendar." >&2
  echo "If you'd like a Catholic saints calendar, check out" >&2
  echo "   misc/pal/saints.pal"  >&2
fi

# source local and private settings last so they take precedence over everything
# changes to local.ksh should not be publicly tracked and shared (recommended)
if [ -f $REPO/local.ksh ]; then
  . $REPO/local.ksh
fi

# vim:nospell:ft=sh:
