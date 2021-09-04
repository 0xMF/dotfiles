# prompt.sh: couple of prompts I like having follow me around...

[ -z "$PS1" ] && return

# echo -n loading prompts...

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

# always ensure extended globbing and expansion is on
THIS_SHELL=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
if [  "${THIS_SHELL##/**/}" = "bash" ]; then
  shopt -s extglob      # shopt -u extglob to unset (don't ask why)
  shopt -s extquote
fi

