[ -z "$PS1" ] && return

THIS_SHELL=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
case "${THIS_SHELL##/**/}" in
  bash|ksh|zsh) ;;
  *) >&2 echo "This script probably wont work with $THIS_SHELL, so bailing out now...bye!";
     exit 1;;
esac

shell=$(basename $SHELL)
[[ "$BASH" = "/usr/bin/bash" || "$BASH" = "/bin/bash" ]] && shell="bash"

# required for hub (cli tool for github management)
#export BROWSER='links2 -no-g'
export BROWSER='w3m -v -no-mouse -s -cookie -no-proxy'
export PROMPT_COMMAND="green"
export SHELL_PROMPT=$( [ "$shell" = "bash" ] && echo $ || echo %)

if [ "$shell" = "zsh" ]; then
  BLACK="%{$fg[black]%}"
  GREY="%{$fg_bold[grey]%}"
  RED="%{$fg_bold[red]%}"
  GREEN="%{$fg_bold[green]%}"
  YELLOW="%{$fg_bold[yellow]%}"
  BLUE="%{$fg_bold[blue]%}"
  MAGENTA="%{$fg_bold[magenta]%}"
  CYAN="%{$fg_bold[cyan]%}"
  WHITE="%{$fg_bold[white]%}"
  NOCOLOR="%{$reset_color%}"
else
  BLACK="\[\033[30m\]"
  GREY="\[\033[1;30m\]"
  RED="\[\033[1;31m\]"
  GREEN="\[\033[1;32m\]"
  YELLOW="\[\033[1;33m\]"
  BLUE="\[\033[1;34m\]"
  PURPLE="\[\033[1;35m"
  CYAN="\[\033[1;36m\]"
  WHITE="\[\033[1;37m\]"
  NOCOLOR="\[\033[00m\]"
fi

function green {
  LS_COLORS="`echo $LS_COLORS|sed 's/di=0[01];3[0-9]/di=01;33/'`"
  LS_COLORS="`echo $LS_COLORS|sed 's/ln=[01][01];3[0-9]/ln=00;32/'`"
  LS_COLORS="`echo $LS_COLORS|sed 's/ex=0[01];3[0-9]/ex=01;32/'`"
  export LS_COLORS
  [ "$shell" = "zsh" ] &&  { 0xMF-zsh-prompt; return; }
  if [ `id -u` -eq 0 ]; then
    PS1="$RED\W $(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="green"
  else
    if [ "$SHELL_PROMPT" = "$" ]; then
      [ "$shell" = "bash" ] && {
        PS1="$GREEN\W $(parse_git_repo)"
        if _is_git_repo; then
           PS1="$PS1$NOCOLOR $SHELL_PROMPT "
        else
           PS1="$PS1$NOCOLOR$SHELL_PROMPT "
        fi
        PROMPT_COMMAND="green" ;
      }
    else
      [ "$shell" = "ksh" ] && {
        prompt="$GREEN\W $(parse_git_repo)" ;
        if _is_git_repo; then
          print "$prompt$NOCOLOR $SHELL_PROMPT "
        else
          print "$prompt$NOCOLOR$SHELL_PROMPT "
        fi
        PS1='$(green)' ;
      }
    fi
  fi
}

function ps1 {
  if [ `id -u` -eq 0 ]; then
    PS1="$RED${OSRV}$BLUE:\w $(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="ps1"
  else
    if [ $(/usr/bin/id -u) -eq 1000 ]; then
      PS1="$GREEN${OSRV}$BLUE:\W$(parse_git_repo 2>/dev/null)$NOCOLOR$SHELL_PROMPT "
    else
      if [[ "$SHELL_PROMPT" = "$" ]]; then
        PS1="$PURPLE${OSRV}$BLUE:\W$(parse_git_repo 2>/dev/null)$NOCOLOR$SHELL_PROMPT "
        PROMPT_COMMAND="ps1"
      else
        print "$PURPLE${OSRV}$BLUE:\W$(parse_git_repo 2>/dev/null)$NOCOLOR$SHELL_PROMPT "
        PS1='$(ps1)'
      fi
    fi
  fi
}

function pscs {
  if [ `id -u` -eq 0 ]; then
    PS1="$RED\h:\W $(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="pscs"
  else
    if [[ "$SHELL_PROMPT" = "$" ]]; then
      PS1="$GREEN\u$YELLOW@$CYAN\h$YELLOW:$GREEN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PROMPT_COMMAND="pscs"
    else
      print "$GREEN\u$YELLOW@$CYAN\h$YELLOW:$GREEN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PS1='$(pscs)'
    fi
  fi
}

function psc {
  if [ `id -u` -eq 0 ]; then
    PS1="$RED\h:\W $(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="psc"
  else
    if [[ "$SHELL_PROMPT" = "$" ]]; then
      PS1="$(parse_git_repo)$CYAN$SHELL_PROMPT$NOCOLOR "
      PROMPT_COMMAND="psc"
    else
      print "$(parse_git_repo)$CYAN$SHELL_PROMPT$NOCOLOR "
      PS1='$(pscs)'
    fi
  fi
}

function pssc {
  if [ `id -u` -eq 0 ]; then
    PS1="$RED\W $(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="pssc"
  else
    if [[ "$SHELL_PROMPT" = "$" ]]; then
      PS1="$CYAN\h$YELLOW:\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PROMPT_COMMAND="pssc"
    else
      print "$CYAN\h$YELLOW:\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PS1='$(pssc)'
    fi
  fi
}

function psh {
  if [ `id -u` -eq 0 ]; then
    PS1="$RED\h:\W $(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="psh"
  else
    if [[ "$SHELL_PROMPT" = "$" ]]; then
      PS1="$PURPLE\h:$CYAN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PROMPT_COMMAND="psh"
    else
      print "$PURPLE\h:$CYAN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PS1='$(psh)'
    fi
  fi
}

function psept {
  if [ `id -u` -eq 0 ]; then
    PS1="$RED\h:\W $(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="psept"
  else
    if [[ "$SHELL_PROMPT" = "$" ]]; then
      PS1="$YELLOW\h:$CYAN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PROMPT_COMMAND="psept"
    else
      #print "$YELLOW\h:$CYAN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      prompt="$YELLOW\h:$CYAN\W $(parse_git_repo)" ;
      if _is_git_repo; then
        print "$prompt$NOCOLOR $SHELL_PROMPT "
      else
        print "$prompt$NOCOLOR$SHELL_PROMPT "
      fi
      PS1='$(psept)'
    fi
  fi
}

function pss {
  if [ `id -u` -eq 0 ]; then
    PS1="$RED\W $(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="pss"
  else
    if [[ "$SHELL_PROMPT" = "$" ]]; then
      PS1="$BLUE\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PROMPT_COMMAND="pss"
    else
      print "$BLUE\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PS1='$(pss)'
    fi
  fi
}

function psl {
  if [ `id -u` -eq 0 ]; then
    PS1="$PURPLE\W $(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="psl"
  else
    if [[ "$SHELL_PROMPT" = "$" ]]; then
      PS1="$CYAN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PROMPT_COMMAND="psl"
    else
      print "$CYAN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PS1='$(psl)'
    fi
  fi
  pslight
}

function psd {
  if [ `id -u` -eq 0 ]; then
    PS1="$PURPLE\W $(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="psd"
  else
    if [[ "$SHELL_PROMPT" = "$" ]]; then
      PS1="$GREEN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PROMPT_COMMAND="psd"
    else
      print "$GREEN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PS1='$(psd)'
    fi
  fi
  psdark
}

function psm {
  [ "$shell" = "zsh" ] && { eval "function precmd { PROMPT='$MAGENTA%%$NOCOLOR ' }" ; return ; }
  WHITE="%{$fg_bold[white]%}"

  if [ `id -u` -eq 0 ]; then
    PS1="$(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="psm"
  else
    if [[ "$SHELL_PROMPT" = "$" ]]; then
      PS1="$(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PROMPT_COMMAND="psm"
    else
      print "$(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PS1='$(psm)'
    fi
  fi
}

function gbruh {
  if [ -z "$1" ]; then
    echo "Usage: git push -u origin branch-name" >&2
    return
  fi
  if [ `git branch --list "$1" | wc -c` -eq 0 ]; then
    echo "branch: $1 not found!" >&2
    return 1
  else
    git push -u origin $1
  fi
}

function parse_git_repo {
  [ -z "$(parse_git_branch)" ] && return

  is_bare=$(git rev-parse --is-bare-repository)
  [ "$is_bare" = "true" ] \
  && echo "$BLUE($CYAN"bare $YELLOW⚙"$BLUE)" \
  || parse_git_branch_colour
}

function parse_git_branch {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}

function parse_git_branch_colour {
  br=$(parse_git_branch)

  if [ -z "$br" ]; then
    return
  elif [ "$br" = "master" ]; then
    echo "$BLUE($RED"$br"$BLUE $(parse_git_dirty)$BLUE)"
  else
    echo "$BLUE($YELLOW"$br"$BLUE $(parse_git_dirty)$BLUE)"
  fi
}

function parse_git_dirty {
  local unchanged="$GREEN✔$NOCOLOR"
  local changed="$RED✗$NOCOLOR"
      sts=$(git status --porcelain 2>&1)
      if [ -z "$sts" ]; then
        echo $unchanged
      else
        echo $changed
      fi
}

function old_parse_git_dirty {

  local large_repos=~/.bash_git_large_repos
  local unchanged="$GREEN✔$NOCOLOR"
  local changed="$RED✗$NOCOLOR"
  local sts_skip="$RED❔$YELLOW❓$GREEN❓$NOCOLOR"

  # check for large repo (fastest)
  #[ -n "$GIT_LARGE_REPO" ] && echo $sts_skip && return
  [ ! -e /usr/bin/time ] && {
      sts=$(git status --porcelain 2>&1)
      if [ -z "$sts" ]; then
        echo $unchanged
      else
        echo $changed
      fi
      return
  }

  # (slower) check for large repo in filenames of all large repos
  $GREP -qw "$PWD$" $large_repos 2>/dev/null
  if [ $? -eq 0 ]; then
    echo $sts_skip
  else
    # not in list of large repos, run a one time check for this being a large repo
    if [ "$OS" = "FreeBSD" ]; then
      sts=$(/usr/bin/time -p git status --porcelain 2>&1)
      echo $sts
    else
      sts=$(/usr/bin/time -f "%E" git status --porcelain 2>&1)
      secs=$(echo "$sts"|tail -1 |cut -d: -f2|cut -d. -f1)
      secs=${secs:-0}

      # do we need to add it to the list of large repos?
      if [ $secs -gt 1 ]; then
          echo "$PWD" >> $large_repos
          export GIT_LARGE_REPO="$PWD"
          echo $sts_skip && return
      fi
    fi

    lns=$(echo "$sts"|wc -l)
    if [ $lns -eq 1 ]; then
      echo $unchanged
    else
      echo $changed
    fi
  fi
}

# show all git aliases
function gits {
  # get all git-aliases and git-functions; filter out non-git; sort
  {
    alias|sed 's/alias //' | $GREP '^g[a-zA-Z]'
  } | sed -r '/(gpg|gvim|grep)/d'| sort | awk -F"'" '{printf("%8s %s\n",$1,$2)}'|less -E
  echo
  echo To see any expansion of the functions below use ghelp, example: ghelp amend
  echo
  {
    d=$(declare -F | $GREP ' -f g[a-zA-Z]' |cut -d" " -f3)
    a=$(sed -r '/#/d;/^$/d;/^\[/d;s/ *=.*//' $HOME/.git/aliases.gitconfig)
    f="$d $a"
    echo $f|tr ' ' '\n'
  } |sort |fmt -w `tput cols`
  echo
}

function ghelp {
  if [ -z "$1" ]; then
    ghuman
    echo
    echo -e "\nUsage: ghelp my_git_alias\n\n"\
            " where\n\n my_git_alias is any of the following (some aliases are shown above):\n"
    sed -r '/#/d;/^$/d;/^\[/d;s/ *=.*//;s/ *--[a-z].*//;/^\s*$/d' $HOME/.git/aliases.gitconfig | sort | fmt
    echo
  else
    expand=$(alias "$1" 2>/dev/null)
    [ $? -eq 0 ] && echo "$expand" && return
    expand=$($GREP -w "^ *$1" $HOME/.git/aliases.gitconfig 2>/dev/null)
    [ $? -eq 0 ] && echo "$expand" && return
    expand=$(declare -f "$1")
    [ $? -eq 0 ] && echo "$expand" && return

    echo "$1 isn't a bash alias,git alias, or a bash function"
  fi
}

function ghuman {
   sed -n '/BEGIN HUMAN/,/END HUMAN/p' $HOME/.git/aliases.gitconfig
}

# git most-recently-used aliases and bash-functions
function gmru {
   history|awk '{$1="";print $0}'|sort|uniq -c|sort -n|$GREP  '[0-9] *g[a-zA-Z]'
}

function _is_git_repo {
  git status >/dev/null 2>&1
  return $?
}

# show all git commits history
function ghist {
  if _is_git_repo -eq 0
  then
    [ "$1" = "--all" ] \
      && { shift; ghist-all "$@"; } \
      || git log --color=always --graph --date=short --pretty=format:"%C(red bold)%h%Creset %C(blue bold)%ad%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s" \
            "$@"
  fi
}

function ghist-all {
  if _is_git_repo -eq 0
  then
    [[ -n "$1" && -s "$1" ]] \
      && git log --color=always --pretty=format:'%C(cyan bold)%h%Creset | %C(red bold)%ad%Creset %d %Creset%s%Cgreen [%cn]' --date=short --follow -- "$1" \
      || { >&2 echo "Usage ghist-all filename"; return; }
  fi
}

# completes the triad of:
#   * alias gcf for git commit --fixup
#   * alias gcs for git commit --squash
#   * function gcr for git commit --reword
function gcr {
  if _is_git_repo -eq 0
  then
    case $# in
      1)  git add . ;  git commit -vac $1  ;;
      *) echo "Usage: gcr <commit>" >&2 ;;
    esac
  fi
}

# formerly aliases, now rewritten as functions
function gd    { if _is_git_repo -eq 0; then git diff --color=always -w; fi }
function gdc   { if _is_git_repo -eq 0; then git diff --color=always -w --cached; fi }
function gdh   { if _is_git_repo -eq 0; then git diff --color=always -w HEAD; fi }
function gds   { if _is_git_repo -eq 0; then git diff --color=always -w --staged; fi }
function gdiff { if _is_git_repo -eq 0; then git diff --color=always -w HEAD | grep -v binary; fi }
function gst   { if _is_git_repo -eq 0; then git status ; fi }
function gss   { if _is_git_repo -eq 0; then git status -s; fi }

function __gdh {
  if _is_git_repo -eq 0
  then
    n=${1:--10}
    git log --color=always -$n --graph --date=short \
      --pretty=format:"%C(red bold)%h%Creset %C(blue bold)%ad%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s"
  fi
}

function __gh {
  if _is_git_repo -eq 0
  then
    n=${1:--10}
    if [[ "$n" = "--all" ]]; then
      git log --color=always $n --graph --pretty=format:"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s"
    else
      git --no-pager log --color=always $n --graph --pretty=format:"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s"
      echo
    fi
    # default pager must handle less than one screen correctly (bash/Linux with less, ksh/OpenBSD with less -c are ok)
    # else uncomment line below when only on bash/Linux
    #if [[ "$SHELL_PROMPT" = "$" ]]; then
      eval "[ `git log --color=always --pretty=oneline | wc -l` -gt 10 ] && git diff --color=always --stat HEAD~$((0 - $n)) HEAD"
    #fi
  fi
}

# queries git log based on arguments
#   no args : show last 10 one-line commit messages
#   1 arg   : show that (relative to HEAD~) commit message details
#   2 args  : show summary of commit messages within the given numbers
function gh {
  #`echo "$@"|sed -r 's/(,|-|  )/ /g'`

  if _is_git_repo -eq 0
  then
    e=$([ "$1" = "d" ] && echo __gdh || echo __gh)

    [ "$1" = "d" ] && shift
    case $# in
      1) [ $1 -eq 1 ] && git log --color=always -p --stat HEAD...HEAD~1 \
                      || git log --color=always -p --stat HEAD~`expr $1 - 1`...HEAD~$1 ;;
      2) [ $1 -eq 1 ] && git log --color=always --stat  HEAD...HEAD~$2 \
                      || git log --color=always --stat  HEAD~$1...HEAD~$2 ;;
      *) [[ `$e | wc -l` -gt 50 ]] \
                      && { $e | head -30; echo -e "\n   ...[snip]...\nremoved listings! use gha if you want it all\n"; }\
                      || $e ;;
         #br_name=`git rev-parse --abbrev-ref HEAD`
         #br_all=`git branch -a|$GREP HEAD|cut -d'>' -f2`
         #case "origin/$br_name" in
         #  "$br_all") $e HEAD...origin/$br_name;;
         #  *)
         #esac
    esac
  fi
}

function gha {
  if _is_git_repo -eq 0
  then
   eval ` [ -z "$1" ] && echo __gh || echo __gdh ` --all
  fi
}

function ghb {
  if _is_git_repo -eq 0
  then
    for c in `seq $(gh|wc -l)`
    do
        git log --color=always --pretty=format:"%C(bold red)%h%Creset %C(bold cyan)%s%Creset %C(bold green)%b%Creset" \
            HEAD~$c..HEAD~`expr $c - 1`
    done
  fi
}

function ghd {
  # lines=$(git log --color=always --oneline|wc -l)
  # pager=`[ $lines -gt 78 ] && echo "less -R"  || echo "cat"  `
  if _is_git_repo -eq 0
  then
    git log --color=always --decorate --abbrev-commit --date=short --all --graph\
            --pretty=format:"%C(red bold)%h%Creset %C(green bold)%s%Creset" #|\
    #cut -c1-64 | less #eval "$pager"
  fi
}

# if $1 is 1 'git show HEAD' otherwise 'git show $1'
function _gshow {
  if _is_git_repo -eq 0
  then
    case $# in
      0) git show --color=always HEAD~11..HEAD --minimal 2>/dev/null ;;
      1) [ $1 -eq 1 ] \
             && git show --color=always HEAD~2..HEAD 2>/dev/null \
             || git show --color=always HEAD~$1      2>/dev/null ;;
      *) n=$(( `echo $2` + 1 )); git show --color=always HEAD~$n..HEAD~$1 --minimal;;
    esac

    # unknown error, probably a SHA1, so git show $1
    [ $? -ne 0 ] && git show --color=always $1
  fi
}

# if $1 contains alphabets (HEAD,SHA1 commits) 'git show $1' otherwise call _gshow
function gshow {
  if _is_git_repo -eq 0
  then
    _gshow "$@"
  fi
}

# goto any repo which is below pwd and show commits from there; pop back when done
function rshow {
  if _is_git_repo -eq 0
  then
    if [ "$1" = "" ]; then
      gshow
    else
      repo=$(find . -type d -name "$1")
      pushd $repo
      shift

      n=$(git rev-list HEAD --count)
      [ $n -gt 11 ] && n=11 || n=$(($n-1))

      #git show HEAD~11..HEAD --pretty=short --abbrev-commit
      git show --color=always HEAD~$n..HEAD --abbrev-commit
      popd
    fi
  fi
}

# help learning git, without arguments shows a list, with arguments works like grep
function glearn {

  TFILE=/tmp/glearn

  if [ ! -z "$1" ]; then
    cat /dev/null > /tmp/glearn
    man -k git|$GREP --color=none -iw git|$GREP "(7)" | $GREP "$@" | tee -a $TFILE
    man -k git|$GREP --color=none -iw git|$GREP "(5)" | $GREP "$@" | tee -a $TFILE
    man -k git|$GREP --color=none -iw git|$GREP "([^157])" | $GREP "$@" | tee -a $TFILE

    if [[ `wc -l $TFILE|awk '{print $1}'` -eq 1 ]]; then
      echo
      read -p 'Show man page? (Y/y/q): ' key
      [[ "$key" = "q" || "$key" = "Q" ]] && return
      man `awk '{print $1}' $TFILE|sed 's/([0-9])//'`
    fi
  else
    man -k git|$GREP --color "(7)"
    echo
    man -k git|$GREP --color "(5)"
    echo
    man -k git|$GREP --color=none -iw git|$GREP --color "^[^:]*([^157])"
    # check if tty
    if [ -t 0 ]; then
      echo
      read -p 'Any key to continue or q to quit: ' key
      [[ "$key" = "q" || "$key" = "Q" ]] && return
    fi
  fi

  if [ ! -z "$1" ]; then
      man -k git|$GREP  -w git|$GREP "(1)"|sed 's/\(.*\) - \(.*\)/\2 : \1/'|sort| \
          awk -F':' '{printf ("%-80s %s\n", $1,$2)}'|$GREP "$@"
  else
      man -k git|$GREP  -w git|$GREP "(1)"|sed 's/\(.*\) - \(.*\)/\2 : \1/'|sort| \
          awk -F':' '{printf ("%-80s %s\n", $1,$2)}'|less
  fi
  rm -f $TFILE
}

# serve up git man-pages
function gman {
  old_pwd=`pwd`
  which adsf > /dev/null
  [ $? -eq 0 ] && {
    cd /usr/share/doc/git/html
    adsf 2> /dev/null &
  }
  cd $old_pwd
}

function ghw {
  echo -e "my git workflow\n\
    git branch wip\n\
    git commit ...\n\
    git rebase master\n\
    git checkout master\n\
    git merge wip"
}

function gsearch {
  [ -z "$1" ] && echo Usage: gsearch search_term && return

  for commit in $(git log --color=always --oneline|$GREP "$1"|awk '{print $1}')
  do
    git show --color=always --pretty="%h %s %b" --stat $commit
    git show --color=always --pretty="%n %Cred===%C(yellow)**%Cgreenx%C(yellow)**%Cred===%Creset%n" -s $commit
  done
}

function glast {
  mru_repo=$(cat $HOME/repos/mru_repo)
  cd $HOME/repos/$mru_repo
  [ -z "$1" ] && echo "Usage: glast my_git_command/alias" >&2 || "$1"
}

function g {
  aliases=$(git config --get-regexp alias.*|cut -d'.' -f2-|awk '{f=$1; $1=""; printf("%-15s %s\n", f, $0)}')
  case "$1" in
    "alias"|"a")  echo "$aliases"|sort -bk2,2|less ;;
    "ar")         echo "$aliases"|sort -rbk2,2|less ;;
    *)            git "$@" ;;
  esac
}


function gsts {
  if _is_git_repo; then
    git stash show --text
    git status -s
  fi
}

function gt {
  git status 2&>/dev/null && [ $? -eq 0 ] && git tags | sort -n | fmt -w 110
}

function contributors {
  git shortlog --color=always -s -n | sort -b -k1,1nr -k2
}

# Credit: oh-my-zsh/lib/git.zsh and oh-my-zsh/themes/robbyrussell.zsh-theme
function precmd {
  [ "$shell" = "zsh" ] && 0xMF-zsh-prompt
}

function 0xMF-zsh-prompt {
  [ "$shell" = "zsh" ] && {
    PROMPT="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ )"
    PROMPT+=' %{$fg[cyan]%}%c%{$reset_color%} $(parse_git_repo 2>/dev/null)'
    PROMPT+=" %{$fg_bold[green]%}%%$NOCOLOR "
    eval "function precmd { 0xMF-zsh-prompt }"
  }
}

function git-use-0xMF {
  [[ -d ~/.oh-my-zsh || -f ~/.oh-my-zsh ]] && {
    unalias gsts
  } 2>/dev/null
}

# git based aliases
#hub >/dev/null 2>&1
#[ $? -eq  0 ] && alias git='hub'
alias ga='git add'
alias gaa='git add --all'
alias gam='git am'
alias game='git amend'
alias gamen='git amend'
alias gamend='git amend'
alias gbr='git branch'
alias gbrn='git rev-parse --abbrev-ref HEAD'
alias gc='git commit -v'
alias gca='git commit -v -a -t ~/.git/commit.txt'
alias gcam='git commit -am'
alias gcb='git checkout'
alias gcf='git add .;git commit --fixup'
#alias gci='git commit'
alias gcm='git checkout master'
alias gcmsg='git commit -m'
alias gco='git checkout'
alias gcs='git add .;git commit --squash'
alias gdump='git cat-file -p'
alias gfr='git fetch;git rebase remotes/origin/master'
alias ghe='ghelp'
alias ghista='ghist-all'
alias ghumans="ghuman|sort -t'=' -k2 |cut -c1-120"
alias g1='git log --color=always -p -1'
alias gl='git pull'
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
alias glum='git pull upstream master'
alias gp='git push'
alias gpd='git push --dry-run'
alias gpf='git push --force-with-lease'
alias gpu='git push upstream'
alias gpush='git push'
alias gpot='git push origin --tags'
alias gpr='git pull --rebase'
alias gredo='gundo; gca -c ORIG_HEAD'
alias gri='git rebase --interactive'
alias grbi='git rebase --interactive'
alias gsh='gshow'
alias gtype='git cat-file -t'
alias gua='git update-index --assume-unchanged'
alias guna='git update-index --no-assume-unchanged'
alias gundo='git reset --soft HEAD~1'
alias gwc='git whatchanged -p --abbrev-commit --pretty=medium'

alias incoming='git log --color=always origin/master ^master'
alias gin='git log --color=always origin/master ^master'
alias outgoing='git log --color=always master ^origin/master'
alias gout='git log --color=always master ^origin/master'


git-use-0xMF

# vim:nospell:ft=sh:
