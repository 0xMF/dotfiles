#!/usr/bin/env ksh
#

[[ "$(uname -s)" == "FreeBSD" ]] && OS="BSD"
[[ "$(uname)" == "OpenBSD" ]] && OS="BSD"

function 0xMF-cv {
  command -V "$1"
  typeset -f "$1"
}

function 0xMF-sudo {
  print 'default to doas instead of sudo' >&2
  [[ "$1" == "-i" ]] \
    && doas su - \
    || doas "$@"
}

function 0xMF-declare {
  case "$1" in
    "-f") typeset -f $2 ;;
    "-F") typeset +f ;;
    "*" | "" ) functions ;;
  esac
}

function anc {
  alias ls='lsn'
  alias tree='tree -n'
  alias grep='grep --color=none -i'
}

function eman {
  [[ -z "$1" ]] && { print >&2 "Usage: eman man-page-with-EXAMPLES-section"; return; }
  man -Tascii $1 | col -bx | sed -n '/^EXAMPLES/,/^[A-Z]/p' | sed -nr '/^(EXAMPLES| |$)/p'
}

function dpkg-list {
  dpkg-query --list|awk -F' ' '{printf("%s\t%-32s\t",$1,substr($2,0,40));$1=$2=$3=$4=""; print $0}'
}

function 0xMF-pkg_locate {
  [ -z "$1" ] && echo "Usage: pkg_locate name" && return
  ports "$1"
}

function ports {
  [ -z "$1" ] && echo "Usage: ports name" && return
  pushd /usr/ports > /dev/null
  echo */*|tr ' ' '\n'|grep $1
  popd  > /dev/null
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

# vim:nospell:ft=sh:
