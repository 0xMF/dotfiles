# prompt.sh: couple of prompts I like having follow me around...

[ -z "$PS1" ] && return

## man dircolors; dircolors --print-database
## foreground: 30-37   # background: 40-47
#----
#  00:30 = black      #  01:30 = light grey
#  00:31 = red        #  01:31 = brick red
#  00:32 = green      #  01:32 = dark green
#  00:33 = brown      #  01:33 = yellow
#  00:34 = blue       #  01:34 = dark blue
#  00:35 = magenta    #  01:35 = purple
#  00:36 = cyan       #  01:36 = cyan bold
#  00:37 = dark grey  #  01:37 = white bold
#
# to quickly test different colours, test with setting directory colours using
# LS_COLORS=$(echo LS_COLORS|sed 's/di=01;33/di=00;31/') ls


if [ "$(uname -s)" = "FreeBSD" ]; then
  export LSCOLORS="ExGxcxdxCxegDxabagacad"
  CLICOLOR='no=00:fi=00:di=01;33:ln=00;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=00;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.swp=01;31';
  export $CLICOLOR
else
  if [ -n "${TMUX}" ]; then
    export TERM=tmux-256color
  else
    export TERM=xterm-256color
  fi
  export INFOPATH=~/.local/share/eless/info:$INFOPATH
  if [ ${TERM} != "dumb" ]; then
    LS_COLORS='no=00:fi=00:di=01;33:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=00;33:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.swp=01;31';
    export LS_COLORS
  fi
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

# always ensure extended globbing and expansion is on
THIS_SHELL=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
if [  "${THIS_SHELL##/**/}" = "bash" ]; then
  shopt -s extglob      # shopt -u extglob to unset (don't ask why)
  shopt -s extquote
fi

