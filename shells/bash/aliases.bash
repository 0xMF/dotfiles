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
  alias ll='ls -lAFGo'
  alias lld='ls -lAFodG'
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
  alias ls='ls --color=always --ignore="NTUSER.*" --ignore="ntuser.*" -F --group-directories-first'
  alias la='ls -lAhFt --time-style=+"%Y-%b-%d %H:%S"'
  alias lc='ls -FCp -w 120|less -FRMS'
  alias ll='ls -lhFt --time-style=+"%Y-%b-%d %H:%S"'
  alias lld='ls -lhdFt --time-style=+"%Y-%b-%d %H:%S"'
  alias stats="man -k ' ' | grep -Ee 'stat \((1|8)\) '"

fi

alias ..='cd ..'
alias ...='cd ../..'
alias ack='ack-grep'
alias alldoc="cat $HOME/repos/dotfiles/doc/*-tips"
alias cd..='cd ..'
alias cls='clear'
alias cpi='cp -i'
alias cpu='cp -u'
alias dfh="df -h|egrep '^(Filesystem|/)'| sort -hk5"
alias e='emacs'
alias em='emacs'
alias emcas='emacs'
alias ft='tree -I .git -I "*.png" --charset UTF-8 --dirsfirst -F'
alias gnupg='gpg'
alias grep='grep --color=always -i'
alias gv='gvim --remote-tab-silent'
#alias lsc='ls | cut -c1-50 | column -c "${COLUMNS:-80}"'
alias make='make -j `grep -c "^processor" /proc/cpuinfo`'
alias mvi='mv -i'
alias pacman='sudo pacman'
alias pacdoc="cat $HOME/repos/dotfiles/doc/pacman-tips"
alias perldoc='perldoc -MPod::Text::Color::Delight'
alias pg='less -Feq'
alias python='/usr/bin/python3'
alias re='recent'
alias rehash='hash -r'
alias reset='reset -e ^?'
alias sau='sudo apt update'
alias sysdoc="cat $HOME/repos/dotfiles/doc/systemd-tips"
#alias sadu='sudo apt update; sudo apt dist-upgrade'
#alias saru='sudo apt autoremove'
alias sbcl='rlwrap sbcl'
#alias tree='tree -I .git --charset UTF-8 -push'
alias tree='tree -I .git --charset UTF-8 --dirsfirst -F'
alias trdd='trd 2 -d'
alias trddd='trd 3 -d'
alias ftree='tree -I .git -I "*.png" --charset UTF-8 --dirsfirst -F'
alias tmux='TERM=tmux-256color tmux -u'
alias vi='vim'
alias vb='/usr/local/bin/vimb 2>/dev/null &'
alias vimb='/usr/local/bin/vimb 2>/dev/null &'

# git based aliases
[ ! -z "`which hub 2>/dev/null`" ] && alias git='hub'
#alias g='git' # now g() is a function that calls git
alias gbr='git branch'
alias gbrn='git rev-parse --abbrev-ref HEAD'
alias gc='git commit -v'
alias gca='git commit -v -a -t ~/.git/commit.txt'
alias gcb='git checkout'
alias gcf='git add .;git commit --fixup'
#alias gci='git commit'
alias gcm='git checkout master'
alias gcmsg='git commit -m'
#alias gco='git checkout'
alias gco='git checkout'
alias gcs='git add .;git commit --squash'
alias gdump='git cat-file -p'
alias gfr='git fetch;git rebase remotes/origin/master'
alias ghe='ghelp'
alias ghista='ghist --all'
alias ghumans="ghuman|sort -t'=' -k2 |cut -c1-120"
alias gl='git log --color=always -p -1'
alias gla='git log --color=always -p --all'
alias glar='git log --color=always -p --all --reverse'
alias glf='git log --color=always -p --follow'
alias glog='git log --color=always -p'
alias glogs='git log --color=always --stat'
#alias gld='git log --color=always --decorate --abbrev-commit --date=short --pretty=format:"%C(red bold)%h%Creset %C(dim green)%ad%Creset %C(cyan bold)|%Creset %s"'
alias gld='git log --color=always --decorate --abbrev-commit --date=short --pretty=format:"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset%s"'
alias glol='gld --graph'
alias glola='gld --all --graph'
alias glolar='gld --all --reverse'
alias glp='git log --color=always -p'
alias gls='git ls-files'
alias gpot='git push origin --tags'
alias gpr='git pull --rebase'
alias gredo='gundo; gca -c ORIG_HEAD'
alias gri='git rebase --interactive'
alias gsh='gshow'
alias gtype='git cat-file -t'
alias gua='git update-index --assume-unchanged'
alias guna='git update-index --no-assume-unchanged'
alias gundo='git reset --soft HEAD~1'
alias gwc='git whatchanged -p --abbrev-commit --pretty=medium'

alias incoming='git log --color=always orgin/master ^master'
alias outgoing='git log --color=always master ^orgin/master'

# vim:nospell:ft=sh:
