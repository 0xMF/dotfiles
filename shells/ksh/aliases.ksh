#!/usr/bin/env bash
#
# User defined aliases

THIS_SHELL=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
case "${THIS_SHELL##/**/}" in
  bash|ksh|zsh) ;;
  *) >&2 echo "This script probably wont work with your shell, so bailing out now...bye!";
     exit 1;;
esac

[[ "$(uname -s)" == "FreeBSD" ]] && OS="BSD"
[[ "$OSRV" == "OpenBSD" ]] && OS="BSD"
export OS

bsd() {
  alias declare='0xMF-declare'
  alias eg='egrep -i'
  alias grep='grep -i'
  alias h='fc -l'
  alias iotop='top -mio -ototal'
  alias j=jobs
  alias l='ls -l'
  alias links='elinks'
  alias ll='ls -lFaho'
  alias lld='ls -lFahdo'
  alias ls='/bin/ls -pF'
  alias lsof='fstat'
  alias lynx='elinks'
  alias m=more
  alias pg='less -Feq'
  alias pgr='vi -R'
  alias pkg_check='pkg_libchk'
  alias pkg_locate="echo /usr/ports/*/*|tr ' ' '\n'"
  alias portaudit='/usr/sbin/pkg audit -F'
  alias su='su -l'
  alias sudo='0xMF-sudo'
  alias stats="man -k '*' | grep -Ee 'stat\((1|8)\) '"
  alias tree='tree -n'
}

linux() {
  alias ls='/bin/ls --color=always'
  alias lsn='ls -pF --color=none'
  alias la='ls -lah'
  alias lc='ls -Cp -w 120|less -FRMS'
  alias ll='ls -lh'
  alias lld='ls -lhd'
  alias grep='grep --color=always -i'
  alias su='su -'
  alias stats="man -k ' ' | grep -Ee 'stat \((1|8)\) '"
}

alias lsd='ls -d *'

case "$OSRV" in
  "OpenBSD" )    bsd ;;
  "GNU/Linux" )  linux ;;
  * ) [[ "$OS" == "BSD" ]] && { bsd; } || { bsd; } ;;
esac



alias ..='cd ..'
alias ack='ack-grep'
alias cd..='cd ..'
alias cls='clear'
alias cpi='cp -i'
alias cpu='cp -u'
alias gnupg='gpg'
alias mvi='mv -i'
alias pg='vi -R'
alias rehash='hash -r'
alias reset='reset -e ^?'
alias vi='vim'

# git based aliases
type hub >/dev/null
[[ $? -eq 0 ]] && alias git='hub'
alias gbr='git branch'
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gci='git commit'
alias gcm='git checkout master'
alias gcmsg='git commit -m'
alias gco='git checkout'
alias gco='git checkout'
alias gd='git diff'
alias gdiff='git diff'
alias gdump='git cat-file -p'
alias glgp='git log -p'
alias glgs='git log --stat'
alias glogs='git log --stat'
alias glol='git log --graph --decorate --pretty=oneline --abbrev-commit'
alias glola='git log --graph --decorate --pretty=oneline --abbrev-commit --all'
alias glolar='git log --decorate --pretty=oneline --abbrev-commit --all --reverse'
alias gls='git ls-files'
alias gst='git status'
alias gsta='git status'
alias gtype='git cat-file -t'
alias gwc='git whatchanged -p --abbrev-commit --pretty=medium'
alias incoming='git log orgin/master ^master'
alias outgoing='git log master ^orgin/master'

# vim:nospell:ft=sh:
