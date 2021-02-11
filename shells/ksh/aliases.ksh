#!/usr/bin/env bash
#
# User defined aliases

[[ "$(uname -s)" == "FreeBSD" ]] && OS="BSD"
[[ "$OSRV" == "OpenBSD" ]] && OS="BSD"
export OS

bsd() {
  alias cv='command -v'
  alias declare='0xMF-declare'
  alias eg='egrep -i'
  alias h='fc -l'
  alias iotop='top -mio -ototal'
  alias j=jobs
  alias l='ls -l'
  alias links='elinks'
  alias ll='ls -laFo'
  alias lld='ls -laFod'
  alias ls='/bin/ls -F'
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
alias ple='perl -wnle'
alias please='perl -wnle'
alias rehash='hash -r'
alias reset='reset -e ^?'
alias vi='vim'

# git based aliases
type hub >/dev/null
[[ $? -eq 0 ]] && alias git='hub'
alias g='git'
alias gbr='g branch'
alias gc='g commit -v'
alias gca='g commit -v -a'
alias gci='g commit'
alias gcm='g checkout master'
alias gcmsg='g commit -m'
alias gco='g checkout'
alias gco='g checkout'
alias gd='g diff'
alias gdiff='g diff'
alias gdump='g cat-file -p'
alias glgp='g log -p'
alias glgs='g log --stat'
alias glogs='g log --stat'
alias glol='g log --graph --decorate --pretty=oneline --abbrev-commit'
alias glola='g log --graph --decorate --pretty=oneline --abbrev-commit --all'
alias glolar='g log --decorate --pretty=oneline --abbrev-commit --all --reverse'
alias gls='g ls-files'
alias gst='git status'
alias gsta='g status'
alias gsts='g status -s'
alias gtype='g cat-file -t'
alias gwc='g whatchanged -p --abbrev-commit --pretty=medium'
alias incoming='g log orgin/master ^master'
alias outgoing='g log master ^orgin/master'

# vim:nospell:ft=sh:
