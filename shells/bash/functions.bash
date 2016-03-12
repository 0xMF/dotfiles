#!/usr/bin/env bash
#

# user defined aliases .bash
#

OS=$(uname -s)
export PROMPT_COMMAND="ps1;$PROMPT_COMMAND"

BLACK="\[\033[30m\]"
GREY="\[\033[1;30m\]"
RED="\[\033[1;31m\]"
GREEN="\[\033[1;32m\]"
YELLOW="\[\033[1;33m\]"
BLUE="\[\033[1;34m\]"
PURPLE="\[\033[1;35m" 
CYAN="\[\033[1;36m\]"
WHITE="\[\033[1;37m\]"
NOCOLOR="\[\033[00m\]"

function dark {
  # yellow dir
  LS_COLORS="`echo $LS_COLORS|sed 's/di=0[01];3[0-9]/di=01;34:/'`"
  # cyan link
  LS_COLORS="`echo $LS_COLORS|sed 's/ln=0[01];3[0-9]/ln=00;36:/'`"
  # green executables
  LS_COLORS="`echo $LS_COLORS|sed 's/ex=0[01];3[0-9]/ex=00;32:/'`"
  export LS_COLORS
}

function light {
  # black dir
  # LS_COLORS="`echo $LS_COLORS|sed 's/di=01;33/di=00;30:/'`"
  # brown dir
  LS_COLORS="`echo $LS_COLORS|sed 's/di=0[01];3[0-9]/di=00;34:/'`"
  # purple link
  LS_COLORS="`echo $LS_COLORS|sed 's/ln=0[01];3[0-9]/ln=01;35:/'`"
  # green executables
  LS_COLORS="`echo $LS_COLORS|sed 's/ex=0[01];3[0-9]/ex=00;32:/'`"
  export LS_COLORS
}

function parse_git_branch {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}

function parse_git_branch_colour {
  br=$(parse_git_branch)
  if [ -z "$br" ]; then
    return
  elif [ "$br" == "master" ]; then
    echo " $BLUE($RED"$br"$BLUE $(parse_git_dirty)$BLUE)"
  else
    echo " $BLUE($YELLOW"$br"$BLUE $(parse_git_dirty)$BLUE)"
  fi
}

function parse_git_dirty {

  local large_repos=~/.bash_git_large_repos
  local unchanged="$GREEN✔$NOCOLOR"
  local changed="$RED✗$NOCOLOR"
  local sts_skip="$RED❔$YELLOW❓$GREEN❓$NOCOLOR"

  # check for large repo (fastest)
  [ -n "$GIT_LARGE_REPO" ] && echo $sts_skip && return

  # (slower) check for large repo in filenames of all large repos
  /bin/grep -qw "$PWD$" $large_repos 2>/dev/null
  if [ $? -eq 0 ]; then 
    echo $sts_skip
  else
    # not in list of large repos, run a one time check for this being a large repo
    if [ $OS == "FreeBSD" ]; then
      sts=$(/usr/bin/time -p git status --porcelain 2>&1)
      echo -e '\b'
    else
      sts=$(/usr/bin/time -f "%E" git status --porcelain 2>&1)
      lns=$(echo "$sts"|wc -l)
      secs=$(echo "$sts"|tail -1 |cut -d: -f2|cut -d. -f1)

      # do we need to add it to the list of large repos?
      if [ $secs -gt 1 ]; then
          echo "$PWD" >> $large_repos
          export GIT_LARGE_REPO="$PWD"
          echo $sts_skip && return
      fi
      
      if [ $lns -eq 1 ]; then
        echo $unchanged
      else
        echo $changed
        # no then 
        #echo "$sts"|head -1|grep -Ee '^[0-9]:[0-9][0-9].[0-9][0-9]$' 2>&1 >/dev/null
        #if [ $? -eq 0 ]; then
        #  echo $unchanged 
        #fi
      fi
    fi
  fi
}

# show all git aliases
function gits(){
  alias|sed 's/alias //'|grep ^g
}
function ghist() {
  #git log --pretty=format:\"%h %ad | %s%d [%an]\" --graph --date=short
  git log --pretty=format:"%h %ad | %s" --graph --date=short
}

# Edit your current day's todo list. 
function todo(){ 
  ${EDITOR:-/usr/local/bin/vim} + ~/$(date +todolist-%Y%m%d); 
}

function ps1 {
  if [ `id -u` -eq 0 ]; then
    PS1="$RED${OSRV}$BLUE:\w$(parse_git_branch_colour)$RED#$NOCOLOR "
  else
    PS1="$GREEN${OSRV}$BLUE:\w$(parse_git_branch_colour)$NOCOLOR$ "
  fi
  PROMPT_COMMAND="ps1"
}

function psh {
  if [ `id -u` -eq 0 ]; then
    PS1="$RED\h:\W$(parse_git_branch_colour)$RED#$NOCOLOR "
  else
    PS1="$BLUE\h:\W$(parse_git_branch_colour)$NOCOLOR$ "
  fi
  PROMPT_COMMAND="psh"
}

function pss {
  if [ `id -u` -eq 0 ]; then
    PS1="$RED\W$(parse_git_branch_colour)$RED#$NOCOLOR "
  else
    PS1="$BLUE\W$(parse_git_branch_colour)$NOCOLOR$ "
  fi
  PROMPT_COMMAND="pss"
}

function psl {
  if [ `id -u` -eq 0 ]; then
    PS1="$PURPLE\W$(parse_git_branch_colour)$RED#$NOCOLOR "
  else
    PS1="$CYAN\W$(parse_git_branch_colour)$NOCOLOR$ "
  fi
  PROMPT_COMMAND="psl"
}

function psd {
  dark
}
function poof {
  sudo sync
  sudo sync
  sudo sync
  sudo systemctl start poweroff.target
}

function share {
  sudo mount -t vboxsf -o uid=1000,gid=1000,dmode=700,fmode=600,umask=077 share /home/mark/share
}

function su {
  [ -z "$1" ] && /bin/su -m || /bin/su "$1"
}

function anc {
  alias ls='lsn'
  alias tree='tree -n'
  alias grep='grep --color=none -i'
}

function dpkg-get-selections {
  dpkg --get-selections
  >&2 echo 
  >&2 echo -----------------------------
  >&2 echo Usage: dpkg --get-selections 
  >&2 echo -----------------------------
}

function dpkg-list {
  dpkg-query --list|awk -F' ' '{printf("%s\t%-32s\t",$1,substr($2,0,40));$1=$2=$3=$4=""; print $0}'
}

function pkg_locate {
  [ -z "$1" ] && echo "Usage: pkg_locate name" && return
  ports "$1"
}

function ports {
  [ -z "$1" ] && echo "Usage: ports name" && return
  pushd /usr/ports > /dev/null
  echo */*|tr ' ' '\n'|grep $1
  popd  > /dev/null
}

function alarm {
  if [ ! -z "$1" ]
  then
    echo "alarm will go off in $1"
    sleep $1 && while true; do espeak "Time's up!"; done &
  else
    sleep 1m && while true; do espeak "Time's up!"; done &
  fi
}

function g+++ {
  rm -f a.out
  if [ -e /usr/bin/g++-4.9 ];
  then
    /usr/bin/g++-4.9 -std=c++1y -Wall $*
  else
    /usr/bin/g++ -std=c++0x -Wall $*
  fi

  if [ -e a.out ]
  then
    if [ -e /usr/bin/valgrind ]
    then
      valgrind 2>&1 a.out|sed '1,5d'
    else
      a.out
    fi
  fi
}

function gcr {
  case $# in
    1)  git add . ;  git commit -c $1  ;;
    *) echo "Usage: gcr <commit>" >&2 ;;
  esac
}

# queries git log based on arguments
#   no args : show last 10 one-line commit messages
#   1 arg   : show that (relative to HEAD~) commit message details
#   2 arts  : show summary of commit messages within the given numbers
function gh {
  case $# in
    1) [ $1 -eq 1 ] && git log -p --stat HEAD \
                    || git log -p --stat HEAD~`expr $1 - 1`...HEAD~$1 ;;
    2) [ $1 -eq 1 ] && git log --stat  HEAD...HEAD~$2 \
                    || git log --stat  HEAD~$1...HEAD~$2 ;;
    *) git log --graph --decorate --pretty=oneline --abbrev-commit --all -10 ;;
  esac
}

function gshow {
  case $# in
    1) [ $1 -eq 1 ] && git show HEAD \
                    || git show $1 ;;
    *) git show HEAD ;;
  esac
}

# vim:nospell:ft=sh:
