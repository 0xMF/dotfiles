#!/usr/bin/env bash
#

# user defined aliases .bash
#

OS=$(uname -s)

showdoc() {

  [[ ! -s "$1" ]] && { 2>&1 echo "Usage: showdoc FILE from dotfiles/doc"; return 1; }
  which -a vimpager > /dev/null 2>&1
  [[ $? -eq 0 ]] && vimpager $1 || cat $1
}

ll() {

  [[ -z "$1" ]]\
    && ls -lhFt --time-style=+"%Y-%b-%d %H:%S" | sed "/^total /d"\
    || ls -lhFt --time-style=+"%Y-%b-%d %H:%S" "$@" | sed "/^total /d"
}

ls-files-only() {
  eval '[[ $# -eq 0 ]] && \ls -dF * || { for d in "$@"; do \ls -dF ${d}/*; done } | sed "\|/$|d"'
}

ls-all-files-only() {
  find "$@" -maxdepth 1 -type f
}

record() {
  if [[ `type -t termtosvg` == "file" ]]
  then
    termtosvg -c '/bin/bash --init-file ~/.bash/minimal.bash' \
              -g "80x24" "$@"
  fi
}

lsc() {
  ls "$@" | cut -c1-44 | column -c "${COLUMNS:-88}"
}

xfci() {
  git update-index --assume-unchanged $HOME/repos/dotfiles/misc/config/xfce4/terminal/terminalrc
  pushd $HOME/repos/dotfiles/misc/config/xfce4/xfconf/xfce-perchannel-xml  > /dev/null
  for f in `echo *`
  do
    git update-index --assume-unchanged $f
  done
  popd > /dev/null
}

xfcni() {
  git update-index --no-assume-unchanged $HOME/repos/dotfiles/misc/config/xfce4/terminal/terminalrc
  pushd $HOME/repos/dotfiles/misc/config/xfce4/xfconf/xfce-perchannel-xml  > /dev/null
  for f in `echo *`
  do
    git update-index --no-assume-unchanged $f
  done
  popd > /dev/null
}

# show directory tree
trd() {
    depth="$1"
    level="-L 1"
    path="$2"

    # if $1 is a number use it for maxdepth
    if [ "$depth" -eq "$depth" ] 2> /dev/null
    then
        level="-L $1";
        shift
    else
        path="$1"
    fi

    [[ "$1" == "$path" && -n "$1" ]] && shift

    eval "tree $level $path $@"
}

poof() {
  sudo sync
  sudo sync
  sudo sync
  sudo systemctl start poweroff.target
}

# Environment variable settings for setting DISPLAY to local or remote
get_xserver() {
  case $TERM in
    xterm )
        XSERVER=$(who am i | awk '{print $NF}' | tr -d ')''(' )
        XSERVER=${XSERVER%%:*}
        ;;
    aterm | rxvt)
        ;;
  esac
}

set_display_env() {
  if [ -z ${DISPLAY:=""} ]; then
    get_xserver
    if [[ -z ${XSERVER} || ${XSERVER} == $(hostname) || ${XSERVER} == "unix" ]]; then
      DISPLAY=":0.0" # Display on local host.
    else
       DISPLAY=${XSERVER}:0.0 # Display on remote host.
    fi
  fi
  export DISPLAY
}

share() {
  mkdir -p $HOME/share
  sudo mount -t vboxsf -o uid=$UID,gid=$(id -g),dmode=700,fmode=600,umask=077 share $HOME/share
  [ $? -eq 0 ] \
    && echo "Successfully mounted $HOME/share" \
    || ( \
        sudo mount -t vboxsf -o uid=$UID,gid=$(id -g),dmode=700,fmode=600,umask=077 eshare $HOME/share; \
        [ $? -eq 0 ] && echo "Successfully mounted $HOME/share" \
      )
}

su() {
  SU=$(which su)
  [ -z "$1" ] && $SU -m || $SU "$@"
}

anc() {
  alias ls='lsn'
  alias tree='tree -n'
  alias grep='grep --color=none -i'
}

beep() {
  paplay /usr/share/sounds/freedesktop/stereo/service-login.oga
  paplay /usr/share/sounds/freedesktop/stereo/service-logout.oga
}

alarm() {
  if [ ! -z "$1" ]
  then
    echo "alarm will go off in $1 minutes"
    sleep $(( 60 * $1 )) && beep &
  else
    echo "alarm will go off in 30 seconds"
    sleep 30 && beep &
  fi
}

function g+++ {
  rm -f a.out
  if [ -e /usr/bin/g++-4.9 ];
  then
    /usr/bin/g++-4.9 -std=c++1y -Wall "$@"
  else
    /usr/bin/g++ -std=c++0x -Wall "$@"
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

loc() {
  `which localc` "$@" 2>/dev/null &
}

low() {
  `which lowriter` "$@" 2>/dev/null &
}

erls() {
  `which erl` "$@" -pa ebin -pa deps/*/ebin
}

emacs() {
  EMACS=$(which -a emacs|sed '1q')
  if [ -z "${EMACS}" ]
  then
    [[ -e "$HOME/repos/x/emacs/src/emacs" ]] \
      &&  EMACS="$HOME/repos/x/emacs/src/emacs" \
      ||  EMACS="/usr/local/share/emacs/src/emacs"
  fi
  ${EMACS} "$@" &
  if [ $? -eq 0 ]
  then
   which pal 2>/dev/null
   [ $? -eq 0 ] && pal
  fi
}

ew() {
  `which -a emacs-works|sed '1q'` "$@" 2>/dev/null && [ `which pal 2>/dev/null` ] && pal &
}

end() {
  #`which -a emacs|sed '1q'` "$@" --no-desktop 2>/dev/null &
  emacs "$@" --no-desktop &
}

otp() {
  if [ -d $HOME/repos/otp ];
  then
    pushd $HOME/repos/otp > /dev/null
    if [ ! -z "$ERL_TOP" ];
    then
      git remotes | grep "up" > /dev/null
      do=$([ $? -eq 0 ] && echo pupup || echo pup)
      git "$do" | grep "^Already up[- ]to[- ]date.$"
      if [ $? -ne 0 ];
      then
        make 2>&1 | tee ../erlang-update.log
        [ $? -eq 0 ] && make tests 2>&1 | tee ../erlang-make-tests.log
      fi
    fi
    popd > /dev/null
  fi
}

sl() {
  SL=$(which -a sl|sed '1q')
  echo ${SL}
  if [ -n "${SL}" ]
  then
    ${SL} -ale
  fi
}

# filter markdown file given through pandoc and read in w3m
# else locate [rRiI] (readme/index) markdown files in given path or use pwd
lessmd() {

  mdfiles=[rRiI]*.md
  path=${1:-.}
  files=$(echo "$path"|grep '\.md$' > /dev/null 2>&1; [ $? -ne 0 ] && echo "${path%/}/$mdfiles")
  file=$([ `echo "$files"` = "${path%/}/$mdfiles" ] && echo "nil")

  [ "$file" = "nil" ] \
              && echo Usage: lessmd filename.md \
              || pandoc `echo "$files"` | w3m -num -s -T text/html
}

ds() {
  h=$(df -hTxtmpfs -xdevtmpfs | head -1)
  b=$(df -hTxtmpfs -xdevtmpfs | sed -n '2,$p'| sort -hk6)
  printf "%s\n%s\n" "$h" "$b"
  dpigs -H
}

recent() {
  OLDPWD=$(pwd)
  cd $HOME
  find ~ -atime 0 -ctime 0 -mtime 0 -type f |\
    \grep -vwEe ".ICEauthority|.Xauthority"\
          -vwEe ".bash_history|.git|.keychain|mesa_shader_cache|.viminfo|.xsession-errors"\
          -vwEe "dbus|icons|fontconfig|gvfs-metadata|vimb|webkitgtk|WebKitCache"\
          -vwEe "elpa|chromium|pulse|qtshadercache|swp$" |\
    \grep -vEe  "emacs\.d\/(auto-|\.)" -ve "~$"
  cd $OLDPWD
}

mouse-reset() {
  sudo modprobe -r psmouse
  sleep 1
  sudo modprobe psmouse
  [[ -e ~/.right-mouseconfig ]] && xmodmap ~/.right-mouseconfig
}

# vim:nospell:ft=sh:
