#!/usr/bin/env bash
#
# User defined aliases 

OS=$(uname -s)

if [ $OS == "FreeBSD" ]; then 

  alias eg='egrep -i'
  alias h='fc -l'
  alias iotop='top -mio -ototal'
  alias j=jobs
  alias l='ls -lG'
  alias links='elinks'
  alias ll='ls -laFGo'
  alias lld='ls -laFodG'
  alias ls='/bin/ls -FG'
  alias lsof='fstat'
  alias lynx='elinks'
  alias m=more
  alias pgr='vi -R'
  alias pkg_check='pkg_libchk'
  alias pkg_locate="echo /usr/ports/*/*|tr ' ' '\n'"
  alias portaudit='/usr/sbin/pkg audit -F'
  alias su='su -l'
  alias stats="man -k '*' | grep -Ee 'stat\((1|8)\) '"

else

  #alias ls='/bin/ls --color=always'
  alias lsn='ls -pF --color=none'
  alias ls='ls --color=always --ignore="NTUSER.*" --ignore="ntuser.*"'
  alias la='ls -lah'
  alias lc='ls -Cp -w 120|less -FRMS'
  alias ll='ls -lh'
  alias lld='ls -lhd'
  alias stats="man -k ' ' | grep -Ee 'stat \((1|8)\) '"

fi

alias ..='cd ..'
alias ...='cd ../..'
alias ack='ack-grep'
alias gv='gvim --remote-tab-silent'
alias cd..='cd ..'
alias cls='clear'
alias cpi='cp -i'
alias cpu='cp -u'
alias gnupg='gpg'
alias grep='grep --color=always -i'
alias gv='gvim --remote-tab-silent'
alias mvi='mv -i'
alias pg='less -Feq'
alias rehash='hash -r'
alias reset='reset -e ^?'
alias vi='vim'
alias tmux='tmux -u'

# git based aliases 
[ ! -z "`which hub 2>/dev/null`" ] && alias git='hub'
alias g='git'
alias gbr='g branch'
alias gbrn='g rev-parse --abbrev-ref HEAD'
alias gc='g commit -v'
alias gca='g commit -v -a -t ~/.git/commit.txt'
alias gcb='g checkout'
alias gcf='g add .;g commit --fixup'
#alias gci='g commit'
alias gcm='g checkout master'
alias gcmsg='g commit -m'
#alias gco='g checkout'
alias gco='g checkout'
alias gcs='g add .;g commit --squash'
alias gd='g diff HEAD'
alias gdump='g cat-file -p'
alias gfr='g fetch;g rebase remotes/origin/master'
alias ghe='ghelp'
alias ghista='ghist --all'
alias ghumans="ghuman|sort -t'=' -k2 |cut -c1-120"
alias gl='g log -p -1'
alias gla='g log -p --all'
alias glar='g log -p --all --reverse'
alias glf='g log -p --follow'
alias glog='g log -p'
alias glogs='g log --stat'
#alias gld='g log --decorate --abbrev-commit --date=short --pretty=format:"%C(red bold)%h%Creset %C(dim green)%ad%Creset %C(cyan bold)|%Creset %s"'
alias gld='g log --decorate --abbrev-commit --date=short --pretty=format:"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset%s"'
alias glol='gld --graph'
alias glola='gld --all --graph'
alias glolar='gld --all --reverse'
alias glp='g log -p'
alias gls='g ls-files'
alias gpr='g pull --rebase'
alias gredo='gundo; gca -c ORIG_HEAD'
alias gri='g rebase --interactive'
alias gsh='gshow'
alias gst='g status'
alias gsts='g status -s'
alias gtype='g cat-file -t'
alias gundo='g reset --soft HEAD~1'
alias gwc='g whatchanged -p --abbrev-commit --pretty=medium'

alias incoming='g log orgin/master ^master'
alias outgoing='g log master ^orgin/master'

# vim:nospell:ft=sh:
