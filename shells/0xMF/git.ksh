# Exit if not running interactively
[ -z "$PS1" ] && return

# Determine the current shell using reliable, built-in variables
if [ -n "$BASH_VERSION" ]; then
  shell="bash"
elif [ -n "$ZSH_VERSION" ]; then
  shell="zsh"
elif [ -n "$KSH_VERSION" ] || [ "$(basename "$SHELL")" = "ksh" ]; then
  shell="ksh"
else
  # Fallback to examining process info if built-in variables aren't available
  THIS_SHELL=$(ps -p $$ -o comm= 2>/dev/null || basename "$SHELL")
  THIS_SHELL=$(basename "$THIS_SHELL" | sed 's/^-//')

  case "$THIS_SHELL" in
    bash|ksh|zsh)
      shell="$THIS_SHELL"
      ;;
    *)
      >&2 echo "Warning: Unknown shell '$THIS_SHELL'. This script is designed for bash, ksh, or zsh."
      >&2 echo "Some features may not work correctly, so bailing out now...bye!"
      ;;
  esac
fi

# required for hub (cli tool for github management)
#export BROWSER='links2 -no-g'
export BROWSER='w3m -v -no-mouse -s -cookie -no-proxy'
export PROMPT_COMMAND="green"
export SHELL_PROMPT=$( [ "$shell" = "bash" ] && echo $ || echo %)

if [ "$shell" = "zsh" ]; then
  BLACK="%{$fg[black]%}"; export BLACK
  GREY="%{$fg_bold[grey]%}"; export GREY
  RED="%{$fg_bold[red]%}"; export RED
  GREEN="%{$fg_bold[green]%}"; export GREEN
  YELLOW="%{$fg_bold[yellow]%}"; export YELLOW
  BLUE="%{$fg_bold[blue]%}"; export BLUE
  MAGENTA="%{$fg_bold[magenta]%}"; export MAGENTA
  CYAN="%{$fg_bold[cyan]%}"; export CYAN
  WHITE="%{$fg_bold[white]%}"; export WHITE
  NOCOLOR="%{$reset_color%}"; export NOCOLOR
else
  BLACK="\[\033[30m\]"; export BLACK
  GREY="\[\033[1;30m\]"; export GREY
  RED="\[\033[1;31m\]"; export RED
  GREEN="\[\033[1;32m\]"; export GREEN
  YELLOW="\[\033[1;33m\]"; export YELLOW
  BLUE="\[\033[1;34m\]"; export BLUE
  PURPLE="\[\033[1;35m"; export PURPLE
  CYAN="\[\033[1;36m\]"; export CYAN
  WHITE="\[\033[1;37m\]"; export WHITE
  NOCOLOR="\[\033[00m\]"; export NOCOLOR
fi

GREP=$(if [ -s /bin/grep ]; then echo /bin/grep; else echo /usr/bin/grep; fi)

function ls-dark-theme {
  LS_COLORS="`echo $LS_COLORS|sed 's/di=0[01];3[0-9]/di=01;33/'`"
  LS_COLORS="`echo $LS_COLORS|sed 's/ln=[01][01];3[0-9]/ln=01;36/'`"
  LS_COLORS="`echo $LS_COLORS|sed 's/ex=0[01];3[0-9]/ex=01;32/'`"
  export LS_COLORS
}

function green {
  ls-dark-theme
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

function psuh {
  if [ `id -u` -eq 0 ]; then
    PS1="$RED\h:\W $(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="psuh"
  else
    if [[ "$SHELL_PROMPT" = "$" ]]; then
      PS1="$CYAN\u$YELLOW@$PURPLE\h$RED:$GREEN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PROMPT_COMMAND="psuh"
    else
      if [ "$shell" = "zsh" ]; then
        if [ `id -u` -eq 0 ]; then
          eval "function precmd { PROMPT='$RED#$NOCOLOR ' }"
        else
          { 0xMF-zsh-psuh; return;  }
        fi
        WHITE="%{$fg_bold[white]%}"
      else
        print "$CYAN\u$YELLOW@$PURPLE\h$RED:$GREEN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
        PS1='$(psuh)'
      fi
    fi
  fi
}

function psu {
  if [ `id -u` -eq 0 ]; then
    PS1="$RED\u@\h:\W $(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="psu"
  else
    if [[ "$SHELL_PROMPT" = "$" ]]; then
      PS1="$CYAN\u$RED:$GREEN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PROMPT_COMMAND="psu"
    else
      if [ "$shell" = "zsh" ]; then
        if [ `id -u` -eq 0 ]; then
          eval "function precmd { PROMPT='$RED#$NOCOLOR ' }"
        else
          { 0xMF-zsh-psu; return;  }
        fi
        WHITE="%{$fg_bold[white]%}"
      else
        print "$CYAN\u$RED:$GREEN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
        PS1='$(psu)'
      fi
    fi
  fi
}

function psg {
  ls-dark-theme
  if [ `id -u` -eq 0 ]; then
    PS1="$RED$(parse_git_repo)$RED#$NOCOLOR "
    PROMPT_COMMAND="psg"
  else
    [ "$shell" = "zsh" ] &&  { 0xMF-zsh-psg; return; }
    if [[ "$SHELL_PROMPT" = "$" ]]; then
      PS1="$(parse_git_repo)$CYAN$SHELL_PROMPT$NOCOLOR "
      PROMPT_COMMAND="psg"
    else
      print "$(parse_git_repo)$CYAN$SHELL_PROMPT$NOCOLOR "
      PS1='$(psg)'
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
      PS1="$PURPLE\h$RED:$GREEN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
      PROMPT_COMMAND="psh"
    else
      if [ "$shell" = "zsh" ]; then
        if [ `id -u` -eq 0 ]; then
          eval "function precmd { PROMPT='$RED#$NOCOLOR ' }"
        else
          { 0xMF-zsh-psh; return;  }
        fi
        WHITE="%{$fg_bold[white]%}"
      else
        print "$PURPLE\h$RED:$GREEN\W $(parse_git_repo)$NOCOLOR$SHELL_PROMPT "
        PS1='$(psh)'
      fi
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
  if [ "$shell" = "zsh" ]; then
    if [ `id -u` -eq 0 ]; then
      eval "function precmd { PROMPT='$RED#$NOCOLOR ' }"
    else
      eval "function precmd { PROMPT='$MAGENTA%%$NOCOLOR ' }"
    fi
    WHITE="%{$fg_bold[white]%}"
  else
    unset PROMPT_COMMAND
    if [ `id -u` -eq 0 ]; then
      PS1="# "
    else
      PS1="$ "
    fi
  fi
}

function psmo {
  if [ "$shell" = "zsh" ]; then
    eval "function precmd { PROMPT='$MAGENTA%%$NOCOLOR ' }"
    WHITE="%{$fg_bold[white]%}"
  else
    if [ `id -u` -eq 0 ]; then
      PS1="$(parse_git_repo)$RED#$NOCOLOR "
      PROMPT_COMMAND="psmo"
    else
      if [[ "$SHELL_PROMPT" = "$" ]]; then
        PS1="$GREEN\W $(parse_git_repo)$WHITE$SHELL_PROMPT$NOCOLOR "
        PROMPT_COMMAND="psmo"
      else
        print "$GREEN\W $(parse_git_repo)$WHITE$SHELL_PROMPT$NOCOLOR "
        PS1='$(psmo)'
      fi
    fi
  fi
}

function psm-no-git {
  PS1="$YELLOW\h$WHITE:$GREEN\W$WHITE\$$NOCOLOR "
  PROMPT_COMMAND=""
}

function ps-remote {
  if [[ "$SHELL_PROMPT" = "$" ]]; then
    if [ -n "${PURPLE}" ]; then
      PS1="$CYAN\u$PURPLE@\h$GREEN:\W$NOCOLOR$SHELL_PROMPT ";
      PROMPT_COMMAND="ps-remote"
    fi
  else
    if [ -n "${MAGENTA}" ]; then
      if [ `id -u` -eq 0 ]; then
        eval "function precmd { PROMPT='$RED#$NOCOLOR ' }"
      else
        { 0xMF-zsh-psremote; return;  }
      fi
      WHITE="%{$fg_bold[white]%}"
    fi
  fi
}

function psmm {
  if [ "$shell" = "zsh" ]; then
    eval "function precmd { PROMPT='$MAGENTA%%$NOCOLOR ' }"
    WHITE="%{$fg_bold[white]%}"
  else
    if [ `id -u` -eq 0 ]; then
      PS1="$RED#$NOCOLOR "
      PROMPT_COMMAND="psmm"
    else
      if [[ "$SHELL_PROMPT" = "$" ]]; then
        PS1="$NOCOLOR$SHELL_PROMPT "
        PROMPT_COMMAND="psmm"
      else
        print "$NOCOLOR$SHELL_PROMPT "
        PS1='$(psmm)'
      fi
    fi
  fi
}

function psmw {
  if [ "$shell" = "zsh" ]; then
    eval "function precmd { PROMPT='$MAGENTA%%$NOCOLOR ' }"
    WHITE="%{$fg_bold[white]%}"
  else
    if [ `id -u` -eq 0 ]; then
      PS1="$RED#$NOCOLOR "
      PROMPT_COMMAND="psmw"
    else
      if [[ "$SHELL_PROMPT" = "$" ]]; then
        PS1="$GREEN\W $NOCOLOR$SHELL_PROMPT "
        PROMPT_COMMAND="psmw"
      else
        print "$GREEN\W $NOCOLOR$SHELL_PROMPT "
        PS1='$(psmw)'
      fi
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
  elif [ "$br" = "master" ] || [ "$br" = "dev" ] || [ "$br" = "main" ]; then
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
  unset local
  unset changed
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

  unset large_repos
  unset changed
  unset unchanged
  unset sts_skip
}

unset opts
unset optsDiff
unset optsPretty1 optsPretty2
opts="--color=always --first-parent --date=short --decorate=no"
optsDiff="--color=always"
optsDiffExclude=":(exclude)*.html"
optsPretty1="%C(red bold)%h%Creset %C(cyan bold)| %ad %C(green bold)*%Creset %C(auto)%d%Creset %s"
optsPretty2="%C(red bold)%h%Creset %C(cyan bold)| %ad %C(green bold)*%Creset %C(auto)%d%Creset %s"
optsPretty3="%C(red bold)%h%Creset %C(cyan bold)| %Creset %C(blue bold)%ad%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s"

# show all git aliases
function galias {
  # get all git-aliases and git-functions; filter out non-git; sort
  {
    alias|sed 's/alias //' | $GREP '^g[a-zA-Z]'
  } | sed -r '/(gpg|gvim|grep)/d'| sort | awk -F"'" '{printf("%8s %s\n",$1,$2)}' |less -E
}

# show all git functions
function gfunctions {
  {
    d=$(declare -F | $GREP ' -f g[a-zA-Z]' |cut -d" " -f3)
    a=$(sed -r '/#/d;/^$/d;/^\[/d;s/ *=.*//' $HOME/.git/aliases)
    f="$d $a"
    echo $f|tr ' ' '\n'
  } |sort |fmt -w `tput cols`
  echo
  echo To see the full expansion use ghelp, example: ghelp amend
}

function ghelp {
  if [ -z "$1" ]; then
    ghuman
    echo
    echo -e "\nUsage: ghelp my_git_alias\n\n"\
            " where\n\n my_git_alias is any of the following (some aliases are shown above):\n"
    sed -r '/#/d;/^$/d;/^\[/d;s/ *=.*//;s/ *--[a-z].*//;/^\s*$/d' $HOME/.git/aliases | sort | fmt
    echo
    ghw
  else
    expand=$(alias "$1" 2>/dev/null)
    [ $? -eq 0 ] && echo "$expand" && return
    expand=$($GREP -w "^ *$1" $HOME/.git/aliases 2>/dev/null)
    [ $? -eq 0 ] && echo "$expand" && return
    expand=$(declare -f "$1")
    [ $? -eq 0 ] && echo "$expand" && return

    echo "$1 isn't a bash alias,git alias, or a bash function"
  fi
}

function ghuman {
   sed -n '/BEGIN HUMAN/,/END HUMAN/p' $HOME/.git/aliases
   ghw
}

function grow {
  if [ -z "$1" ]; then
    >&2 echo -e "Usage: to rebase commits onto <branch-name>: grow <branch-name>, for example: grow beta\n\
       does the following when alpha has latest changeset\n\
        \tgit checkout alpha\n\
        \tgit rebase --onto beta"
    return
  fi
  git rebase --onto $1
}

# git most-recently-used aliases and bash-functions
function gmru {
   history|awk '{$1="";print $0}'|sort|uniq -c|sort -n|$GREP  '[0-9] *g[a-zA-Z]'
}

function _is_git_repo {
  git status >/dev/null 2>&1
  return $?
}

function 0xMF-git-log-pretty {
  eval "git log ${opts} \
          --pretty=format:\"%C(red bold)%h%Creset %C(blue bold)%ad%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s\" $@"
}

function 0xMF-git-log-pretty-reverse {
  eval "git log --reverse ${opts} \
          --pretty=format:\"%C(red bold)%h%Creset %C(blue bold)%ad%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s\" $@"
}

function 0xMF-git-lost-found {
  if _is_git_repo -eq 0
  then
    git gc
    for o in $(git fsck --lost-found | awk '{ print $NF}')
    do
      git show $o
    done
  fi
}
alias glost-found='0xMF-git-lost-found'

# show all git commits history
function ghist {
  if _is_git_repo -eq 0
  then
    if [[ -z "$1" ]]; then
      >&2 echo "Usage ghist filename"
      return 1
    fi
    if [ "$1" = "--all" ]; then
      shift
      ghist-all "$@"
    else
      if echo "$1" | grep -q "^[0-9][0-9]*$"; then
        local c=$(git log --oneline --follow $(realpath $2) | __pager-counter | \grep "^ *$1 " | awk '{ print $2 }')
        git show $(echo "${opts}") "$c" --minimal 2>/dev/null
      else
        for f in "$@"
        do
          echo "$f"
          if  [[ $(0xMF-git-log-pretty --follow "$(realpath $f)" | wc -l) -le 5 ]]; then
            0xMF-git-log-pretty --follow "$(realpath $f)" | __pager-no-counter
          else
            0xMF-git-log-pretty --follow "$(realpath $f)" | __pager-counter
          fi
          echo
        done | less -F
      fi
    fi
  fi
}

# show all git commits history with rel-to-HEAD (works only with linear commit histories)
function ghist-rel-to-HEAD {
  if _is_git_repo -eq 0
  then
    if [ "$1" = "--all" ]; then
      shift
      ghist-all "$@"
    else
      0xMF-git-log-pretty "$@" | __pager-counter
    fi
  fi
}

function ghist-all {
  if _is_git_repo -eq 0
  then
    if [[ -n "$1" && -s "$1" ]]; then
      for f in "$@"
      do
        echo $f
        git log --all $(echo "${opts}") --follow --pretty=format:'%C(cyan bold)%h%Creset | %C(red bold)%ad%Creset %d %Creset%s%Cgreen [%cn]' -- "$1"
      done
    else
      >&2 echo "Usage ghist-all filename"
    fi
  fi
}

# shows history in reverse order
function ghist-reverse {
  0xMF-git-log-pretty-reverse "$@"   #| sed '$s/$/\n/' | tac | less
}

function ghist-reverse-rel-to-HEAD {
  ghist-rel-to-HEAD "$@" | {
    if [ `uname` != "Linux" ]; then
      tail -r
    else
      tac
    fi
  } | less
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
function gdff  { if _is_git_repo -eq 0; then git diff --color=always -w HEAD | grep -v binary; fi }
function gst   { if _is_git_repo -eq 0; then git status ; fi }
function gss   { if _is_git_repo -eq 0; then git status -s; fi }

function gdiff-files-changed {
  if _is_git_repo -eq 0; then
    if [[ -z "$1" || -s "$1" ]]; then
      lines=$(git diff --color=always -w HEAD "$@" |wc -l)
      [ $lines -eq 0 ] && return
      git diff --color=always --stat -w HEAD "$@"
      if [ $lines -le 30 ]; then
        echo
        git diff --color=always -w HEAD "$@"
      else
        echo -ne "\nShow diff details from HEAD? (y/N) "; read key
        if [[ "$key" = "y" || "$key" = "Y" ]]; then
          git diff --color=always -w HEAD "$@"
        fi
      fi
    else
      if [[ -z "$2" && "${1}" -gt 0 ]]; then
        echo -e "Top (max) 10 files changed...from HEAD~${1} HEAD...were "
        git diff --color=always -w --stat HEAD~"${1}" HEAD
        echo -ne "\nShow diff details from HEAD~$1 HEAD? (y/N) "; read key
          if [[ "$key" = "y" || "$key" = "y" ]]; then
            git diff --color=always -w HEAD~"${1}" HEAD | grep -v binary | less -FeqRSX
          fi
      else
        if [[ ! -s "${2}" && "${2}" -gt 0 ]] ; then
          echo -e "Top (max) 10 files changed...from HEAD~${1} HEAD~${2}...were "
          git diff --color=always -w --stat HEAD~"${1}" HEAD~"${2}"
          echo -ne "\nShow diff details from HEAD~${1} HEAD~${2}? (y/N) "; read key
          if [[ "$key" = "y" || "$key" = "Y" ]]; then
            git diff --color=always -w HEAD~"${1}" HEAD~"${2}" | grep -v binary | less -FeqRSX
          fi
        else
          echo "did not understand...$2"
        fi
      fi
    fi
  fi
}

function __gdiff-list-files-and-show-diffs {
  if [ -z "$3" ]; then
    echo -e "list of (max) 10 files changed...from HEAD~${1} to HEAD~${2}...were "
    git diff --color=always -w --stat HEAD~"${1}" HEAD~"${2}" | head
    echo -ne "\nShow diff details from HEAD~$1 HEAD~$2? (Y/n) "; read key
    [[ "$key" = "n" || "$key" = "N" ]] && return
    git diff --color=always -w HEAD~"${1}" HEAD~"${2}" | grep -v binary | less -FeqRSX
  else
    git diff --color=always -w --stat HEAD~"${1}" HEAD~"${2}" -- "$3" | head
    echo -ne "\nShow diff details from HEAD~$1 HEAD~$2? (Y/n) "; read key
    [[ "$key" = "n" || "$key" = "N" ]] && return
    git diff --color=always -w HEAD~"${1}" HEAD~"${2}" -- "$3" | grep -v binary | less -FeqRSX
  fi
}

function gdiff {
  if _is_git_repo -eq 0; then
    if [[ -z "$1" || -s "$1" ]]; then
      lines=$(git diff --color=always -w HEAD "$@" "${optsDiffExclude}" |wc -l)
      [ $lines -eq 0 ] && return
      git diff --color=always --stat -w HEAD "$@" "${optsDiffExclude}"
      if [ $lines -le 30 ]; then
        echo
        git diff --color=always -w HEAD "$@" "${optsDiffExclude}"
      else
        echo -ne "\nShow diff details from HEAD? (Y/n) "; read key
        [[ "$key" = "n" || "$key" = "N" ]] && return
        git diff --color=always -w HEAD "$@" "${optsDiffExclude}"
      fi
    else
      # $1 is a file
      if [[ -s "$1" ]]; then
        local f
        f=$(realpath "$1")
        if [[ -z "$3" && "${2}" -gt 0 ]]; then
          __gdiff-list-files-and-show-diffs $2 0 $f
        else
          git diff --color=always "${optsDiffExclude}"  | grep -v binary | less -FeqRSX
        fi
        unset f
        return
      fi
      # $1 and (if given) $2 are integers greater than zero
      if [[ -z "$2" && "${1}" -gt 0 ]]; then
        __gdiff-list-files-and-show-diffs $1 0
        return
      fi
      if [[ -s "${2}" ]]; then
        __gdiff-list-files-and-show-diffs 0 $1 $2
        return
      fi
      if [[ "${2}" -gt 0 ]] ; then
        echo "git-diff for...$@"
        __gdiff-list-files-and-show-diffs $1 $2 $3
        return
      fi
      git diff --color=always "$@" "${optsDiffExclude}"
    fi
  fi
}

function gdiff-pager {
  [ $# -eq 0 ] && gdff
  if _is_git_repo -eq 0; then
    for f in "$@"
    do
      git diff --color=always -w $f "${optsDiffExclude}"  | grep -v binary
    done | less -FeqRSX
  fi
}

function __gdh {
  if _is_git_repo -eq 0
  then
    n=${1:--10}
    git log --all $n $(echo "${opts}") \
      --pretty=format:"%C(red bold)%h%Creset %C(blue bold)%ad%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s"
  fi
}

function __pager-counter {
  awk '{ printf "%5d %s\n", NR-1, $0 }' | less -FeQRSX
}

function __no-pager-counter {
  awk '{ printf "%5d %s\n", NR-1, $0 }'
}

function __pager-no-counter {
  less -FeQRSX
}


function __ghh {
  if _is_git_repo -eq 0
  then
    c=$(git log --oneline | wc -l)
    n=${1:--10}
    if [[ "$n" != "--no-pager" ]]; then
      if [ $c -le 9 ]; then
        eval "git log -$c $(echo ${opts}) --pretty=format:\"${optsPretty1}\"" | __no-pager-counter
      else
        eval "git log $n $(echo ${opts}) --pretty=format:\"${optsPretty1}\"" | __no-pager-counter
      fi
    else
      eval "git --no-pager log $(echo ${opts}) --pretty=format:\"${optsPretty1}\"" | __pager-counter
    fi
  fi
}

function ghaf {
  if _is_git_repo -eq 0
  then
    n=${1:--10}
    if [[ "$n" = "--all" ]]; then
      git log --all $n $(echo "${opts}") --pretty=format:"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s"
    else
      git --no-pager log --all $n $(echo "${opts}") --pretty=format:"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s"
      echo
    fi
    # default pager must handle less than one screen correctly (bash/Linux with less, ksh/OpenBSD with less -c are ok)
    # else uncomment line below when only on bash/Linux
    #if [[ "$SHELL_PROMPT" = "$" ]]; then
      eval "[ $(git log --all $(echo "${opts}") --pretty=oneline | wc -l) -gt 10 ] && git diff --color=always --stat HEAD~$((0 - $n)) HEAD"
    #fi
  fi
}

# queries git log based on arguments
#   no args : show last 10 one-line commit messages
#   1 arg   : show that (relative to HEAD~) commit message details
#   2 args  : show summary of commit messages within the given numbers
function ghh {
  #`echo "$@"|sed -r 's/(,|-|  )/ /g'`

  if _is_git_repo -eq 0
  then
    if [[ $1 -le 0 ]]; then
      gshow $1
    else
      e=$([ "$1" = "d" ] && echo __gdh || echo __ghh)

      [ "$1" = "d" ] && shift
      case $# in
        1) [ $1 -eq 1 ] && git log --all $(echo "${opts}") -p --stat HEAD...HEAD~1 \
                        || git log --all $(echo "${opts}") -p --stat HEAD~`expr $1 - 1`...HEAD~$1 ;;
        2) [ $1 -eq 1 ] && git log --all $(echo "${opts}") --stat  HEAD...HEAD~$2 \
                        || git log --all $(echo "${opts}") --stat  HEAD~$1...HEAD~$2 ;;
        *) [[ `$e | wc -l` -gt 50 ]] \
                        && { $e | head -30; echo -e "\n   ...[snip]...\nremoved listings! use gha if you want it all\n"; }\
                        || $e;;
           #br_name=`git rev-parse --abbrev-ref HEAD`
           #br_all=`git branch -a|$GREP HEAD|cut -d'>' -f2`
           #case "origin/$br_name" in
           #  "$br_all") $e HEAD...origin/$br_name;;
           #  *)
           #esac
      esac
    fi
  fi
}

function gh-rel-to-HEAD {
  ghh "$@" | __pager-counter
}

function gha {
  if _is_git_repo -eq 0
  then
   eval $([ -z "$1" ] && echo __ghh || echo __gdh) --no-pager
  fi
}

function gha-rel-to-HEAD {
  gha "$@" | __pager-counter
}

function ghb {
  if _is_git_repo -eq 0
  then
    for c in `seq $(__ghh|wc -l)`
    do
        git log --all $(echo "${opts}") --pretty=format:"%C(bold red)%h%Creset %C(bold cyan)%s%Creset %C(bold green)%b%Creset" \
            HEAD~$c..HEAD~`expr $c - 1`
    done
  fi
}

function ghd {
  # lines=$(git log --color=always --oneline|wc -l)
  # pager=`[ $lines -gt 78 ] && echo "less -R"  || echo "cat"  `
  if _is_git_repo -eq 0
  then
    git log $(echo "${opts}") --abbrev-commit --all \
            --pretty=format:"%C(red bold)%h%Creset %C(green bold)%s%Creset" #| #cut -c1-64 | less #eval "$pager"
  fi
}

function ghg {
  if _is_git_repo -eq 0
  then
    n=${1:--10}
    if [[ "$n" = "--all" ]]; then
      git log --all $(echo "${opts}") $n --graph --pretty=format:"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s"
    else
      git --no-pager log --all --graph $(echo "${opts}") $n --pretty=format:"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s"
      echo
    fi
  fi
}

# date strict ISO
function ght {
  if _is_git_repo -eq 0
  then
    git log $(echo "${opts}") --all --date=format:"%F %T"\
            --pretty=format:"%C(red bold)%h%Creset %C(blue bold)%ad%Creset %C(cyan bold)%D%Creset %C(green bold)%s%Creset"
  fi
}

# user supplied arguments
function ghtA {
  if _is_git_repo -eq 0
  then
    git log "$@" $(echo "${opts}") --all --date=format:"%F %T"\
            --pretty=format:"%C(red bold)%h%Creset %C(blue bold)%ad%Creset %C(cyan bold)%D%Creset %C(green bold)%s%Creset"
  fi
}

function ghl {
  if _is_git_repo -eq 0
  then
    git log $(echo "${opts}") --all \
            --pretty=format:"%C(red bold)%h%Creset %C(blue bold)%ah%Creset %C(green bold)%s%Creset"
  fi
}

function ghr {
  if _is_git_repo -eq 0
  then
    git log $(echo "${opts}") --all \
            --pretty=format:"%C(red bold)%h%Creset %C(blue bold)%ar%Creset %C(green bold)%s%Creset"
  fi
}


# worker function for gshow:
#     no arguments  = show last 10 commits
#    one argument   =
#        argument given was a file in git then show last 3 changes to that file
#        argument given was a git object (like tag|commit|tree|blob), then show it
#        argument given was a number then show HEAD if argument was 1 otherwise show HEAD~n
#    otherwise consider two arguments given as two commit hashes and show commits between them
function _gshow {
  local old_opts="${opts}"
  if _is_git_repo -eq 0
  then
    for a in "$@"
    do
      if [[ "$a" = --* ]]; then
        if [[ "$a" != "--help" ]]; then
         opts="$a $opts"
         shift
        else
          man git-show git-log git-diff
          return
        fi
      fi
    done
    c=$(git log --oneline | wc -l)
    p=$([ $c -ge 10 ] && echo 10 || echo $(( $c - 1 )))
    case $# in
      # no arguments means show last 10 commits
      0) echo -e "Showing last $p commit messages"
        git log $(echo "${opts}") -"${p}" --pretty=format:"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(blue bold)%ad%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s" --date=short | awk '{print NR-1,$0}'
        if [ $(git diff --stat HEAD~$p HEAD | wc -l) -gt 10 ]; then
          git diff $(echo ${optsDiff}) --stat HEAD~$p HEAD "${optsDiffExclude}"| head
        else
          git diff $(echo ${optsDiff}) --stat HEAD~$p HEAD "${optsDiffExclude}"
        fi
         echo -ne "\nShow details of last $p commits? (y/N) "; read key
         if [[ "$key" = "y" || "$key" = "Y" ]]; then
          git show $(echo "${opts}") HEAD~$p...HEAD --minimal 2>/dev/null
         fi
        ;;

      # we got one argument, show last 3 commits if that is a file
      1) if [ -s "$1" ]; then
            git log $(echo "${opts}") --pretty=format:"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(blue bold)%ad%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s" --date=short "$1"
            echo -ne "\nShow last $p commit details? (y/N) "; read key
            if [[ "$key" = "y" || "$key" = "Y" ]]; then
              git st $(echo "${opts}") $(git log --all -n 3 --oneline "$1" | cut -d' ' -f1)
              git show $(echo "${opts}") $(git log --all -n 3 --oneline "$1" | cut -d' ' -f1)
            fi
         else
            # not a file, is it a git object type
            local type=$(git cat-file -t $1 2>/dev/null)
            if [[ "$type" = "commit" || "$type" = "tag" || "$type" = "tree" || "$type" = "blob" ]]; then
              eval "git show $(echo "${opts}") $1"
            else
                if [[ "$1" = -[0-9]* || "$1" = [0-9]* ]]; then  # alternatively, if [[ "$1" != "[[:alpha:]]*" ]]; then
                  if [[ $1 -lt 0 ]]; then
                    eval "git log ${opts} HEAD~$((0 - $1))...HEAD --pretty=format:\"${optsPretty3}\"" | __no-pager-counter
                  else
                    git show $(echo "${opts}") HEAD~$1 2>/dev/null
                  fi
                else
                  echo "did not understand... $1"
                fi
              fi
            unset type
         fi ;;

      2) if echo "$1" | grep -q "^[0-9][0-9]*$"; then
            local c=$(git log --oneline --follow $(realpath $2) | __pager-counter | \grep "^ *$1 " | awk '{ print $2 }')
            git show $(echo "${opts}") "$c" --minimal 2>/dev/null
         else
            # two args shows commits, files-changed and diffs between them if first arg was a commit or tag
            # assumes second arg is either a commit or a tag too.
            type=$(git cat-file -t $1 2>/dev/null)
            if [[ "$type" = "commit" || "$type" = "tag" ]]; then
              echo "showing one line summaries of commits in chronological order...";
              local parent=$(git rev-list --parents -n 1 $1|cut -d' ' -f2)
              eval "git log ${opts} --reverse $parent...$2 --pretty=format:\"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(blue bold)%ad%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s\" --date=short"

              echo -n "show files changed between $1 and $2? (Y/n) ";  read key
              [[ "$key" = "n" || "$key" = "N" ]] && return
              local files=$(git diff $(echo ${optsDiff}) --stat $1 $2 "${optsDiffExclude}" 2>/dev/null)
              if [[ $(echo "${files}" | wc -l) -le 1 ]]; then
                echo "no files found; next time try with more hex digits from commit hash for one or both commits"
              else
                eval "git diff $(echo ${optsDiff}) --stat $1 $2 "${optsDiffExclude}" 2>/dev/null"
              fi

              echo -n "show diffs between $1 and $2? (Y/n) ";  read key
              [[ "$key" = "n" || "$key" = "N" ]] && return
              eval "git diff $(echo ${optsDiff}) ${optsDiffExclude} $@"
            else
              if [[ ! -s $2 ]]; then
                if  [[ $1 -ge 0 && $2 -ge 0 ]]; then
                  if [[ $2 -gt $1 ]]; then
                    git log $(echo "${opts}") HEAD~$(($2 + 1))...HEAD~$1 --pretty=format:"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(blue bold)%ad%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s" --date=short
                    git diff $(echo ${optsDiff}) --stat HEAD~$(($2 + 1))..HEAD~$1 "${optsDiffExclude}"
                  else
                    if [[ $1 -gt $2 ]]; then
                      git log $(echo "${opts}") HEAD~$(($1 + 1))...HEAD~$2 --pretty=format:"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(blue bold)%ad%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset %s" --date=short
                      git diff $(echo ${optsDiff}) --stat HEAD~$(($1 + 1))..HEAD~$2 "${optsDiffExclude}"
                    else
                      echo "did not understand... $*"
                    fi
                  fi
                fi
              else
                0xMF-git-log-pretty $(echo "${opts}") HEAD~$1 $2
              fi
            fi
          fi ;;
      *) echo "did not understand... $*"
         ;;
    esac

    # unknown error, probably a SHA1s, so git show $1
    [ $? -ne 0 ] && eval "git show ${opts} $1"
  fi
  opts="${old_opts}"
}
if [ -n "$ZSH_VERSION" ]; then
  compdef _git gshow=git-log
fi

# if $1 contains alphabets (HEAD,SHA1 commits) 'git show $1' otherwise call _gshow
function gshow {
  if _is_git_repo -eq 0
  then
    _gshow "$@"
  fi
}

function gshow--stat {
  if _is_git_repo -eq 0
  then
    _gshow --stat "$@"
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
    cat /dev/null > ${TFILE}
    man -k git|$GREP --color=none -iw git|$GREP "(7)" | $GREP "$@" | tee -a $TFILE
    man -k git|$GREP --color=none -iw git|$GREP "(5)" | $GREP "$@" | tee -a $TFILE
    man -k git|$GREP --color=none -iw git|$GREP "([^157])" | $GREP "$@" | tee -a $TFILE

    if [[ `wc -l $TFILE|awk '{print $1}'` -eq 1 ]]; then
      echo -n "Show man page? (Y/q) ";  read key
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
      echo -n "Any key to continue or q to quit: "; read key
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
  rm -f ${TFILE}
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
    # make branch wip and checkout wip\n\t\
      gbr wip =>  git branch wip\n\t\
      gco wip => git checkout wip\n\n\
    # add commits and rebase as much as needed\n\t\
      git add ...;\n\t\
      gcam; gha; gshow...\n\t\
      gri ...\n\t\
      git commit ...\n\n\
    # ready to merge \n\t\
      git rebase (master|dev|main)\n\t\
      git checkout (master|dev|main)\n\t\
      git merge wip\n\n\
    # push to origin\n\t\
      git pull\n\t\
      git push\n\n\
    # prepare a tagged release\n\t\
      git tag -a x.y.z(-beta) -m 'message'\n\t\
      git push --tags"
}

function gsearch {
  [ -z "$1" ] && echo Usage: gsearch search_term && return

  git log --all $(echo "${opts}") --oneline | $GREP "$1" | less -FeqRSX
}

function gsearch-list {
  [ -z "$1" ] && echo Usage: gsearch-list search_term && return

  for commit in $(git log --all --color=never --oneline|$GREP "$1"|awk '{print $1}')
  do
    git show --color=always --all --pretty="%C(auto)%h %s %b%Creset" --stat $commit
    git show --color=always --all --pretty="medium" --minimal $commit
    git show --color=always --all --pretty="%n %C(bold red)====%C(yellow)***%Cgreen|xxoxx|%C(yellow)***%Cred====%Creset%n" -s $commit
  done | less -FeqRSX
}

function glast {
  mru_repo=$(cat $HOME/repos/mru_repo)
  cd $HOME/repos/$mru_repo
  [ -z "$1" ] && echo "Usage: glast my_git_command/alias" >&2 || "$1"
}

function g {
  local aliases=$(git config --get-regexp "^alias.*"|cut -d'.' -f2-|awk '{f=$1; $1=""; printf("%-15s %s\n", f, $0)}')
  case "$1" in
    "alias"|"a")  echo "$aliases"|sort -bk2,2|less ;;
    "ar")         echo "$aliases"|sort -rbk2,2|less ;;
    *)            git "$@" ;;
  esac
  unset aliases
}


function gsts {
  if _is_git_repo; then
    git stash show --text
    case "$1" in
      "--all" ) git status -s -u --ignored --ignore-submodules ;;
      "--no-swap" ) git status -s -u --ignored --ignore-submodules  | grep -vE ".*~$" ;;
      * ) git status -s "$@" ;;
    esac
  fi
}

function gt {
  if git status 2&>/dev/null; then
    git tags --list -n
  fi
}

function contributors {
  git shortlog --color=always -s -n | sort -b -k1,1nr -k2
}

function gco {
  if [[ -e "$1" ]]; then
      git checkout "$@"
  else
    if [[ -n "$1" ]]; then
      if echo "$1" | grep -q '^[0-9][0-9]*$'; then
        git checkout HEAD~"$1"
      else
        git checkout "$1"
      fi
    else
      git checkout
    fi
  fi
}

function gcm {
  if ! git checkout master 2>/dev/null; then
    if ! git checkout dev 2>/dev/null; then
        git checkout main
    fi
  fi
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

function 0xMF-zsh-psg {
  [ "$shell" = "zsh" ] && {
    PROMPT='$(parse_git_repo 2>/dev/null)'
    PROMPT+=" %{$fg_bold[green]%}%%$NOCOLOR "
    eval "function precmd { 0xMF-zsh-psg }"
  }
}

function 0xMF-zsh-psu {
  [ "$shell" = "zsh" ] && {
    PROMPT="%(?:%{$fg_bold[cyan]%}%n:)"
    PROMPT+='%{$fg_bold[red]%}:%{$fg[green]%}%1d%{$reset_color%}'
    if _is_git_repo ; then
      PROMPT+=' $(parse_git_repo 2>/dev/null)'
    fi
    PROMPT+=" %{$fg_bold[green]%}%%$NOCOLOR "
    eval "function precmd { 0xMF-zsh-psu }"
  }
}

function 0xMF-zsh-psh {
  [ "$shell" = "zsh" ] && {
    PROMPT="%(?:%{$fg_bold[magenta]%}%m:)"
    PROMPT+='%{$fg_bold[red]%}:%{$fg[green]%}%1d%{$reset_color%}'
    if _is_git_repo ; then
      PROMPT+=' $(parse_git_repo 2>/dev/null)'
    fi
    PROMPT+=" %{$fg_bold[green]%}%%$NOCOLOR "
    eval "function precmd { 0xMF-zsh-psh }"
  }
}

function 0xMF-zsh-psuh {
  [ "$shell" = "zsh" ] && {
    PROMPT="%(?:%{$fg_bold[cyan]%}%n%{$fg_bold[yellow]%}@%{$fg_bold[magenta]%}%m:)"
    PROMPT+='%{$fg_bold[red]%}:%{$fg[green]%}%1d%{$reset_color%}'
    if _is_git_repo ; then
      PROMPT+=' $(parse_git_repo 2>/dev/null)'
    fi
    PROMPT+=" %{$fg_bold[green]%}%%$NOCOLOR "
    eval "function precmd { 0xMF-zsh-psuh }"
  }
}

function 0xMF-zsh-psremote {
  [ "$shell" = "zsh" ] && {
    PROMPT="%(?:%{$fg_bold[cyan]%}%n@%{$fg_bold[magenta]%}%m:)"
    PROMPT+='%{$fg[green]%}:%c%{$reset_color%}'
    if _is_git_repo ; then
      PROMPT+=' $(parse_git_repo 2>/dev/null)'
    fi
    PROMPT+=" %{$fg_bold[green]%}%%$NOCOLOR "
    eval "function precmd { 0xMF-zsh-psremote }"
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

alias g='git'

alias ga='git add'
alias gaa='git add --all'
alias gam='git am'
alias game='git amend'
alias gamen='git amend'
alias gamend='git amend'

alias gbr='git branch'
alias gbrn='git rev-parse --abbrev-ref HEAD'

alias gc='git commit -v'
alias gca='git commit -v -a'
alias gcac='git commit -v -a -t ~/.git/commit.txt'
alias gcam='git commit -am'
alias gcb='git switch -'
alias gcf='git add .;git commit --fixup'
alias gci='git commit'
alias gcmsg='git commit -m'
alias gcs='git add .;git commit --squash'

alias gd='git diff --color=always'
alias gdiff-minimal="gdiff-files-changed"
alias gdump='git cat-file -p'

alias ghe='ghelp'
alias ghista='ghist-all'
alias ghistg='ghist --graph'
alias ghumans="ghuman|sort -t'=' -k2 |cut -c1-120"

alias gl='git pull'
alias glr='git pull --rebase'
#alias g1='git log --color=always -p -1'
alias gld='git log --all --color=always --decorate --abbrev-commit --date=short --pretty=format:"%C(red bold)%h%Creset %C(dim green)%ad%Creset %C(cyan bold)|%Creset %s"'
alias glg='git log --all --color=always -p'
alias glgr='git log --all --color=always -p --reverse'
alias glgd='git log --all --color=always --decorate --abbrev-commit --date=short --pretty=format:"%C(red bold)%h%Creset %C(cyan bold)|%Creset %C(auto)%d%Creset%s"'
alias glgf='git log --all  --color=always -p --follow'
alias glgp='git log --all -p'
alias glgs='git log --all --stat'
alias glog='git log --all --stat'
alias glogp='git log --all --color=always -p'
#alias glogs='git log --stat'
alias glogs='git log --all --color=always --stat'
alias glol='git log --all --graph --decorate --pretty=oneline --abbrev-commit'
alias glola='gld --all --graph'
alias glolar='glola --reverse'
#alias glola='git log --graph --decorate --pretty=oneline --abbrev-commit --all'
#alias glolar='git log --decorate --pretty=oneline --abbrev-commit --all --reverse'
alias glp='git log --all --color=always -p'
alias gls='git ls-files'
alias glsi='git status -s --ignored'
alias glsv='git ls-files -v |\grep -v "^H"'

alias gnevermind='git reset --hard HEAD && git clean -d -f'

alias gp='git push'
alias gpd='git push --dry-run'
alias gpf='git push --force-with-lease'
alias gpot='git push origin --tags'
alias gpr='git pull --rebase'
alias gpu='git push upstream'
alias gpush='git push'

alias grbi='git rebase --interactive'
alias gredo='gundo; gca -c ORIG_HEAD'
alias gread-chrono='ghist-reverse-rel-to-HEAD'
alias gri='git rebase --interactive'
alias grr='grow'

alias gsb='git switch -'
alias gsh='gshow'
alias gst='git status'

alias gtags='git tag --list -n'
alias gtype='git cat-file -t'

alias gua='git update-index --assume-unchanged'
alias guna='git update-index --no-assume-unchanged'
alias gundo='git reset --soft HEAD~1'

alias gwc='git whatchanged -p --abbrev-commit --pretty=medium'

#alias incoming='git log orgin/master ^master'
#alias outgoing='git log master ^orgin/master'


## original aliases that are now replaced by functions
unalias gfr glum gin gout incoming outgoing 2>/dev/null

#alias gfr='git fetch;git rebase remotes/origin/master'
function gfr {
 git fetch
 if ! git rebase remotes/origin/master; then
  if ! git rebase remotes/origin/dev; then
    git rebase remotes/origin/main
  fi
 fi 2>/dev/null
}

#alias glum='git pull upstream master'
function glum {
 if ! git pull upstream master; then
  if ! git pull upstream dev; then
    git pull upstream main
  fi
 fi
}

#alias gin='git log --all --color=always origin/master ^master'
function gin {
 if ! git log --all --color=always origin/master ^master; then
  if ! git log --all --color=always origin/dev ^dev; then
    git log --all --color=always origin/main ^main
  fi
 fi 2>/dev/null
}

#alias gout='git log --all --color=always master ^origin/master'
function gout {
 if ! git log --all --color=always origin/master ^origin/master; then
  if ! git log --all --color=always origin/dev ^origin/dev; then
    git log --all --color=always origin/main ^origin/main
  fi
 fi 2>/dev/null
}

#alias incoming='git log --all --color=always origin/master ^master'
function incoming {
  gin
}
#alias outgoing='git log --all --color=always master ^origin/master'
function outgoing {
  gout
}

git-use-0xMF

# vim:nospell:ft=sh:
