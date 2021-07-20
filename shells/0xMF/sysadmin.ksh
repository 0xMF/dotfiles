[ -z "$PS1" ] && return

THIS_SHELL=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
case "${THIS_SHELL##/**/}" in
  bash|ksh|zsh) ;;
  *) >&2 echo "This script probably wont work with $THIS_SHELL, so bailing out now...bye!";
     exit 1;;
esac

function dfh {
  /bin/df -h | egrep '^(Filesystem|/)' | sort -hk5
}

function jc {
  [ "`uname`" != "Linux" ] && { echo "not running on Linux"; return; }
  local myjc=$( whereis journalctl | awk '{print $2}' )
  [ `id -u` -ne 0 ] && myjc="sudo $myjc"
  case "$1" in
    "" ) $myjc -xe | less -FeqRSX ;;
    "help"|"h"|"-h"|"--help") $myjc --help "$@" ;;
    * ) $myjc "$@" ;;
  esac
}

function sc {
  [ "`uname`" != "Linux" ] && { echo "not running on Linux"; return; }
  local mysc=$( whereis systemctl | awk '{print $2}' )
  [ `id -u` -ne 0 ] && mysc="sudo $mysc"
  case "$1" in
    "help"|"h"|"-h"|"--help") $mysc --help "$@" ;;
    * ) $mysc "$@" ;;
  esac
}

# jump to EXAMPLES section of man page if exists else quit
function eman {
  [ -z "$1" ] && { >&2 echo "Usage: eman man-page-with-EXAMPLES-section"; return; }
  ( man -f $1
    echo
    man -Tutf8 $1 | col -bx | sed -n '/^SYNOPSIS/,/^[A-Z]/p' | sed -nr '/^(SYNOPSIS| |$)/p'
    man -Tutf8 $1 | col -bx | sed -n '/^EXAMPLES/,/^[A-Z]/p' | sed -nr '/^(EXAMPLES| |$)/p' ) | less -FeqRSX
}

function sadu {

  mydf=$( whereis df 2>/dev/null | awk '{print $2}' )
  [ -z $mydf ] && return
  [[ ! -s /usr/bin/df && ! -s /bin/df ]] && return

  used=$($mydf -lh / | perl -lane '$. != 1 && print $F[-2]')

  echo -n "You have used $used of disk space on your / partition...continue(y/N)? "
  read REPLY
  [[ "$REPLY" != "y"  &&  "$REPLY" != "Y" && "$REPLY" != "yes" ]] && return

  if [ "`uname`" = "Linux" ]; then
    distro=$(\grep -w ID /etc/os-release | cut -d= -f2 | tr -d '"')
    case "${distro}" in
      arch|manjaro)
        sudo pacman -Syu
          ;;
      centos)
        sudo yum update
          ;;
      debian|kali|mint|ubuntu)
        sudo apt update
        sudo apt dist-upgrade
          ;;
      *) echo does not support ${distro} yet;;
    esac
  fi

  if [ "`uname`" = "OpenBSD" ]; then
    doas syspatch
    doas pkg_add -Uu
  fi
}

function saru {
  if [ "`uname`" = "Linux" ]; then
    distro=$(\grep -w ID /etc/os-release | cut -d= -f2 | tr -d '"')
    case "${distro}" in
      arch|manjaro)
        unneeded=`pacman -Qdtq`
        [[ -n "$unneeded" ]] && sudo pacman -Rsn `echo $unneeded`
        ;;
      centos)
        sudo yum autoremove "$@"
        ;;
      *)
        sudo apt autoremove
    esac
  fi
}

function pacsearch {
  [ -z "$1" ] && { >&2 echo "Usage: pacsearch SEARCH_TERM"; return 1; }
  pacman -Ss "$1" |  perl -pe 's/\n// if $. % 2 == 1' | sed 's/\t//g'
}

function apt {
    if [ "$1" = "info" ]; then
      shift
      /usr/bin/apt show "$@"
    else
      /usr/bin/apt "$@"
    fi
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
  echo */*|tr ' ' '\n'|$GREP $1
  popd  > /dev/null
}

function pkg {
  [ "`uname -s`" != "FreeBSD" ] && return

  local old_pkg=/usr/sbin/pkg
  case "$1" in
    "add")  shift && $old_pkg install "$@"
            ;;
    *) $old_pkg "$@"
            ;;
  esac
}
