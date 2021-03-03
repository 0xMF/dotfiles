THIS_SHELL=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
case "${THIS_SHELL}" in
  bash|ksh|zsh) ;;
  *) >&2 echo "This script probably wont work with your shell, so bailing out now...bye!";
     exit 1;;
esac

function jc {
  case "$1" in
    "" ) /usr/bin/journalctl -xe | less -FeqRSX ;;
    "help"|"h"|"-h"|"--help") /usr/bin/journalctl --help "$@" ;;
    * ) /usr/bin/journalctl "$@" ;;
  esac
}

function sc {
  case "$1" in
    "help"|"h"|"-h"|"--help") /usr/bin/systemctl --help "$@" ;;
    * ) /usr/bin/systemctl "$@" ;;
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

  mydf=$(which df 2>/dev/null)
  [ -z $mydf ] && return
  [[ ! -s /usr/bin/df && ! -s /bin/df ]] && return

  used=$($mydf -lh / | perl -lane '$. != 1 && print $F[-2]')

  echo -n "You have used $used of disk space on your / partition...continue(y/N)? "
  read REPLY
  [[ "$REPLY" != "y"  &&  "$REPLY" != "Y" && "$REPLY" != "yes" ]] && return

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
}

function saru {
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
}

function pacsearch {
  [ -z "$1" ] && { >&2 echo "Usage: pacsearch SEARCH_TERM"; return 1; }
  pacman -Ss "$1" |  perl -pe 's/\n// if $. % 2 == 1' | sed 's/\t//g'
}

function apt {
    echo $1
    [ "$1" == "info" ] && shift && /usr/bin/apt show "$@" \
                       || /usr/bin/apt "$@"
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
