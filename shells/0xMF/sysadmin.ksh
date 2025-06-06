[ -z "$PS1" ] && return

THIS_SHELL=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
case "${THIS_SHELL##/**/}" in
  bash|ksh|zsh) ;;
  *) >&2 echo "This script probably wont work with $THIS_SHELL, so bailing out now...bye!";
     exit 1;;
esac

alias visudo='sudo EDITOR=vi visudo'
function can_sudo {
  if [ "$(uname)" = "OpenBSD" ]; then
    echo "doas"
    return
  fi
  if sudo -Al -U $USER 2>/dev/null 1>/dev/null; then
    echo "sudo"
  else
    echo ""
  fi
}

#sdo=$(can_sudo)

function dfh {
  /bin/df -h | grep -E '^(Filesystem|/)' | sort -hk5
}

function jc {
  local myjc=$( whereis journalctl | awk '{print $2}' )
  local sdo=$(can_sudo)

  [ "$(uname)" != "Linux" ] && { echo "not running on Linux"; return; }
  [ $(id -u) -ne 0 ] && myjc="${sdo} ${myjc}"
  case "$1" in
    "" ) eval "${myjc} -xe" | less -FeqRSX ;;
    "help"|"h"|"-h"|"--help") eval "${myjc} --help $@" ;;
    * ) eval "${myjc} $@" ;;
  esac

  unset myjc sdo
}

function sc {
  local mysc=$( whereis systemctl | awk '{print $2}' )
  local sdo=$(can_sudo)

  [ "$(uname)" != "Linux" ] && { echo "not running on Linux"; return; }
  [ $(id -u) -ne 0 ] && mysc="${sdo} ${mysc}"
  case "$1" in
    "help"|"h"|"-h"|"--help") eval "${mysc} --help $@" ;;
    * ) eval "${mysc} $@" ;;
  esac

  unset mysc sdo
}

# jump to EXAMPLES section of man page if exists else quit
function eman {
  local manopts

  [ -z "$1" ] && { >&2 echo "Usage: eman man-page-with-EXAMPLES-section"; return; }
  if man -Eutf8 $1 > /dev/null 2>&1;  then
    manopts=-Eutf8
  fi
  ( man -f $1
    echo
    man $manopts $1 | col -bx | sed -n '/^SYNOPSIS/,/^[A-Z]/p' | sed -nr '/^(SYNOPSIS| |$)/p'
    man $manopts $1 | col -bx | sed -n '/^EXAMPLES/,/^[A-Z]/p' | sed -nr '/^(EXAMPLES| |$)/p'
  ) | less -FeqRSX

  unset manopts
}

function 0xMF-sysadmin-query-package-list {
  [ -z "$1" ] && { >&2 echo "Usage: $0 PACKAGE-NAME"; return; }

  os=$(uname)
  if [ "${os}" = "Linux" ]; then
    local distro=$(\grep -w ID /etc/os-release | cut -d= -f2 | tr -d '"')
    case "${distro}" in
      arch|manjaro)
        pacman -Ql "$1"
          ;;
      centos)
        rpm -ql "$1"
          ;;
      debian|kali|mint|ubuntu)
        dpkg -L "$1"
          ;;
      *) echo does not support ${distro} yet;;
    esac
  fi

  if [ "${os}" = "OpenBSD" ]; then
    pkg_info -L "$1"
  fi
}

alias sadu='0xMF-sysadmin-upgrade'
function 0xMF-sysadmin-upgrade {

  local sdo=$(can_sudo)
  [[ "${sdo}" != "sudo" && "${sdo}" != "doas" ]] && return

  os=$(uname)

  case "${os}" in
    "Linux"   ) local mydf=$( whereis df 2>/dev/null | awk '{print $2}' ) ;;
    "OpenBSD" ) local mydf=$( whereis df 2>/dev/null ) ;;
    *         ) local mydf="" ;;
  esac
  [ -z "${mydf}" ] && return

  local used=$($mydf -lh / | perl -lane '$. != 1 && print $F[-2]')

  echo -n "You have used $used of disk space on your / partition...continue(y/N)? "
  read REPLY
  [[ "$REPLY" != "y"  &&  "$REPLY" != "Y" && "$REPLY" != "yes" ]] && return

  if [ "${os}" = "Linux" ]; then
    local distro=$(\grep -w ID /etc/os-release | cut -d= -f2 | tr -d '"')
    case "${distro}" in
      arch|manjaro)
        "${sdo}" pacman -Syu
          ;;
      centos)
        "${sdo}" yum update
          ;;
      debian|kali|mint|ubuntu)
        if "${sdo}" apt update; then
          "${sdo}" apt dist-upgrade
        fi
          ;;
      *) echo does not support ${distro} yet;;
    esac
  fi

  if [ "${os}" = "OpenBSD" ]; then
    ${sdo} syspatch 2> /dev/null
    case "$?" in
     1 ) TERM=dumb "${sdo}" pkg_add -D snap -Uu ;;
     2 ) TERM=dumb "${sdo}" pkg_add -Uu ;;
     * ) TERM=dumb >&2 echo "run syspatch before updating" ;;
    esac
  fi

  unset sdo
}

alias saru='0xMF-sysadmin-remove-unused'
function 0xMF-sysadmin-remove-unused {
  local sdo=$(can_sudo)
  if [ "`uname`" = "Linux" ]; then
    distro=$(\grep -w ID /etc/os-release | cut -d= -f2 | tr -d '"')
    case "${distro}" in
      arch|manjaro)
        unneeded=`pacman -Qdtq`
        [[ -n "$unneeded" ]] && "${sdo}" pacman -Rsn `echo $unneeded`
        ;;
      centos)
        "${sdo}" yum autoremove "$@"
        ;;
      *)
        "${sdo}" apt autoremove
    esac
  fi
  if [ "${os}" = "OpenBSD" ]; then
    TERM=dumb "${sdo}" pkg_delete -aic
  fi

  unset sdo
}

alias pacsearch='0xMF-sysadmin-pacsearch'
function 0xMF-sysadmin-pacsearch {
  [ ! -s /usr/bin/pacman ] && { echo "not running on Arch"; return; }
  [ -z "$1" ] && { >&2 echo "Usage: pacsearch SEARCH_TERM"; return 1; }
  /usr/bin/pacman -Ss "$1" |  perl -pe 's/\n// if $. % 2 == 1' | sed 's/\t//g'
}

alias apt='0xMF-sysadmin-apt'
function 0xMF-sysadmin-apt {
  [ ! -s /usr/bin/apt ] && { echo "not running on Debian or its derivative"; return; }
    if [ "$1" = "info" ]; then
      shift
      /usr/bin/apt show "$@"
    else
      /usr/bin/apt "$@"
    fi
}

function 0xMF-sysadmin-dpkg-get-selections {
  dpkg-get-selections
}
function dpkg-get-selections {
  /usr/bin/dpkg --get-selections
  >&2 echo
  >&2 echo -----------------------------
  >&2 echo Usage: dpkg --get-selections
  >&2 echo -----------------------------
}

function 0xMF-sysadmin-dpkg-list {
 dpkg-list
}
function dpkg-list {
  /usr/bin/dpkg-query --list|awk -F' ' '{printf("%s\t%-32s\t",$1,substr($2,0,40));$1=$2=$3=$4=""; print $0}'
}
function 0xMF-sysadmin-dpkg-list-by-size {
 dpkg-list-by-size
}
function dpkg-list-by-size {
  /usr/bin/dpkg-query -Wf '${Installed-Size}\t${Package}\n' | sort -n
}

function pkg_locate {
  0xMF-sysadmin-pkg_locate "$@"
}
function 0xMF-sysadmin-pkg_locate {
  [ ! -d /usr/ports ] && { echo "ports tree is not installed"; return; }
  [ -z "$1" ] && echo "Usage: pkg_locate name" && return
  ports "$1"
}

function ports {
 0xMF-sysadmin-ports "$@"
}
function 0xMF-sysadmin-ports {
  [ ! -d /usr/ports ] && { echo "ports tree is not installed"; return; }
  [ -z "$1" ] && echo "Usage: ports name" && return
  pushd /usr/ports > /dev/null
  echo */*|tr ' ' '\n'| \grep $1
  popd  > /dev/null
}

function pkg {
  0xMF-sysadmin-pkg "$@"
}
function 0xMF-sysadmin-pkg {
  [ "`uname -s`" != "FreeBSD" ] && { echo "not on FreeBSD"; return; }

  local old_pkg=/usr/sbin/pkg
  case "$1" in
    "add")  shift && $old_pkg install "$@"
            ;;
    *) $old_pkg "$@"
            ;;
  esac
}

function perl-repair {
  echo -n "Repair perl locally? This might take a long time...continue(y/N)? "
  read REPLY
  [[ "$REPLY" != "y"  &&  "$REPLY" != "Y" && "$REPLY" != "yes" ]] && return
  env PERL5LIB= PERL_LOCAL_LIB_ROOT= cpan -r
}

alias enabled=0xMF-services-enabled
function 0xMF-services-enabled {
    /usr/bin/systemctl list-unit-files | /bin/grep -w 'enabled' | /bin/grep -vwE '(systemd|dbus|getty)' | sort
}

alias running=0xMF-services-running
function 0xMF-services-running {
    /usr/bin/systemctl | /bin/grep -w running | /bin/awk '{ service=$1 ; $1=$2=$3=$4="" ; printf("%-40s\t%s\n", service, $0) }' | /bin/grep -vwE '(systemd|dbus|getty)' | sort
}
