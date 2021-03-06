echo -n loading prompts...
shell=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
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

function psdark {
  # blue dir, green link, light green executables
  LS_COLORS="`echo $LS_COLORS|sed 's/di=0[01];3[0-9]/di=01;34/'`"
  LS_COLORS="`echo $LS_COLORS|sed 's/ln=[01][01];3[0-9]/ln=00;32/'`"
  LS_COLORS="`echo $LS_COLORS|sed 's/ex=0[01];3[0-9]/ex=01;32/'`"
  export LS_COLORS
}

function pslight {
  # black dir   #LS_COLORS="`echo $LS_COLORS|sed 's/di=01;33/di=00;30/'`"
  # brown dir   #LS_COLORS="`echo $LS_COLORS|sed 's/di=0[01];3[0-9]/di=00;34/'`"
  # purple link #LS_COLORS="`echo $LS_COLORS|sed 's/ln=0[01];3[0-9]/ln=01;35/'`"

  # yellow dir, cyan link, light green executables
  LS_COLORS="`echo $LS_COLORS|sed 's/di=0[01];3[0-9]/di=01;33/'`"
  LS_COLORS="`echo $LS_COLORS|sed 's/ln=[01][01];3[0-9]/ln=01;36/'`"
  LS_COLORS="`echo $LS_COLORS|sed 's/ex=0[01];3[0-9]/ex=01;32/'`"
  export LS_COLORS
}

