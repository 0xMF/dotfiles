#!/usr/bin/env ksh
#

THIS_SHELL=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
case "${THIS_SHELL##/**/}" in
  bash|ksh|zsh) ;;
  *) >&2 echo "This script probably wont work with your shell, so bailing out now...bye!";
     exit 1;;
esac

[[ "$(uname -s)" == "FreeBSD" ]] && OS="BSD"
[[ "$(uname)" == "OpenBSD" ]] && OS="BSD"

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

# vim:nospell:ft=sh:
