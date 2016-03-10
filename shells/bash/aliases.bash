#!/usr/bin/env bash
#
# User defined aliases 

OS=$(uname -s)

if [ $OS == "FreeBSD" ]; then 

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
  alias stats="man -k '*' | grep -Ee 'stat\((1|8)\) '"

else

  alias ls='/bin/ls --color=always'
  alias lsn='ls -pF --color=none'
  alias la='ls -lah'
  alias lc='ls -Cp -w 120|less -FRMS'
  alias ll='ls -lh'
  alias lld='ls -lhd'
  alias stats="man -k ' ' | grep -Ee 'stat \((1|8)\) '"

fi

alias ..='cd ..'
alias ack='ack-grep'
alias cd..='cd ..'
alias cls='clear'
alias cpi='cp -i'
alias cpu='cp -u'
alias gnupg='gpg'
alias grep='grep --color=always -i'
alias mvi='mv -i'
alias pg='vi -R'
alias rehash='hash -r'
alias reset='reset -e ^?'
alias vi='vim'
alias gv='gvim --remote-tab-silent'

# git based aliases 
alias git='hub'
alias g='git'
alias gbr='g branch'
alias gc='g commit -v'
alias gca='g commit -v -a'
alias gcf='git add .;git commit --fixup'
alias gci='g commit'
alias gcm='g checkout master'
alias gcmsg='g commit -m'
alias gco='g checkout'
alias gco='g checkout'
alias gcs='git add .;git commit --squash'
alias gd='g diff'
alias gdiff='g diff'
alias gdump='g cat-file -p'
alias gf='g fetch'
alias gfr='g fetch;g rebase remotes/origin/master'
#alias gh='glola -10'
alias gl='glgp -1'
alias glgp='g log -p'
alias glgs='g log --stat'
alias glogs='g log --stat'
alias glol='g log --graph --decorate --pretty=oneline --abbrev-commit'
alias glola='g log --graph --decorate --pretty=oneline --abbrev-commit --all'
alias glolar='g log --decorate --pretty=oneline --abbrev-commit --all --reverse'
alias gls='g ls-files'
alias gpr='g pull --rebase'
alias gri='g rebase --interactive'
alias gst='git status' 
alias gsta='g status'
alias gsts='g status -s'
alias gtype='g cat-file -t'
alias gwc='g whatchanged -p --abbrev-commit --pretty=medium'
alias incoming='g log orgin/master ^master'
alias outgoing='g log master ^orgin/master'

# vim:nospell:ft=sh:
