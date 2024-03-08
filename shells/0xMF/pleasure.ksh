# pleasure.sh: perl like ruby for regular expression needs and more

[ -z "$PS1" ] && return

THIS_SHELL=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
case "${THIS_SHELL##/**/}" in
  bash|ksh|zsh) ;;
  *) >&2 echo "This script probably wont work with $THIS_SHELL, so bailing out now...bye!";
     exit 1;;
esac

function 0xMF-perldoc {
  local pd=
  if [ -d $PERLBREW_ROOT/perls ]; then
    pd=$(find $PERLBREW_ROOT/perls -name perldoc 2>/dev/null | sed '1q')
  fi
  if [[ -n ${pd} && -x ${pd} ]]; then
      eval "${pd} -MPod::Text::Color::Delight $@"
  else
    if [ -d /usr/bin/core_perl ]; then
      /usr/bin/core_perl/perldoc -MPod::Text::Color::Delight "$@"
    else
      if [ "$(uname)" = "OpenBSD" ]; then
        eval "$(whereis perldoc) -MPod::Text::Color::Delight $@"
      else
        eval "$(whereis perldoc | cut -d' ' -f2 | sed 's/.stub//') -MPod::Text::Color::Delight $@"
      fi
    fi
  fi
  unset pd
}

function 0xMF-sl {
  local _w
  _w=$(date "+%s"|cut -b10-)
  if [ -z "$1" ]; then
    >&2 echo "No fun, sorry. sl is not installed."
    return 1
  else
    "${1}" -cale"${_w}"
  fi
  unset _w
}

unalias sl 2>/dev/null
_sl="$(which sl 2>/dev/null)"
if echo "${_sl}" | grep -q "not found"; then
 _sl=""
fi
alias sl="0xMF-sl $(echo ${_sl})"


alias cv='0xMF-cv'

ogrep=$(alias grep|sed 's/-i//g')
ogrep=$(echo "${ogrep}" -i|sed s/\'//g)
unalias grep
alias grep="$(echo ${ogrep}|sed 's/^.*grep=//')"
unset ogrep

alias make-slides="0xMF-make-slides"
alias sbcl='rlwrap sbcl'
alias ple='perl -wnle'
alias pale='perl -awnle'
alias pd='0xMF-perldoc'
alias pdoc='0xMF-perldoc'
alias perldoc='0xMF-perldoc'
alias wiwin-refresh='sddoc-refresh'
alias wiwin-doc='sddoc'

alias tmux='tmux -u'
alias http-serve="serve"

alias ll='0xMF-ll'
alias llr='0xMF-llr'
alias lg='0xMF-lg'


function 0xMF-cal {
  local c
  c=$(whereis ncal|cut -d: -f2)
  [[ "${c}" = "" ]] && /usr/bin/cal "$@" || /usr/bin/ncal -b "$@"
  unset c
}
alias cal='0xMF-cal'

function 0xMF-lg {
 ls -g "$@" | awk '!/total/{$2=$3="\b"; print $0}'
}

function 0xMF-llr {
  if [ -z "$1" ]; then
    [ "$(uname)" = "OpenBSD" ] \
      && ls -lhFtr \
      || ls --colo=always -lhFtr --time-style=+"%Y-%b-%d %H:%M"
  else
    [ "$(uname)" = "OpenBSD" ] \
      && ls -lhFtr "$@" \
      || ls --color=always -lhFtr --time-style=+"%Y-%b-%d %H:%M" "$@"
  fi | sed '/^total /d'
}

function 0xMF-ll {
  if [ -z "$1" ]; then
    [ "$(uname)" = "OpenBSD" ] \
      && ls -lhFt \
      || ls --color=always -lhFt --time-style=+"%Y-%b-%d %H:%M"
  else
    [ "$(uname)" = "OpenBSD" ] \
      && ls -lhFt "$@" \
      || ls --color=always -lhFt --time-style=+"%Y-%b-%d %H:%M" "$@"
  fi | sed '/^total /d'
}

function 0xMF-reload {
  local shell=$(basename $SHELL)
  if [ -d ~/.${shell}/0xMF ]; then
    for f in ~/.${shell}/0xMF/*
    do
      case ${shell} in
        "ksh" ) [[ -s "$f" ]] && . $f ;;
        * )     [[ -s "$f" ]] && source $f
      esac
    done
  fi
  case ${shell} in
    "ksh" ) [[ -s "$f" ]] && . ~/.${shell}rc ;;
    * )     [[ -s "$f" ]] && source ~/.${shell}rc
  esac
  unset shell
}

function 0xMF-make-slides {
  local force options src

  if ! options=$(getopt -a -l "force" -o "f" -- "$@"); then
    echo "Usage: $(basename $0) ( -f | --force ) filename"
    return
  fi
  eval set -- "$options"
  while true; do
    case "$1" in
      -f | --force)
        force=yes
        shift ;;
      --) shift; break ;;
      *)
        echo "ERROR: $@"
        return ;;
    esac
  done

  src="${1/%.*/.md}"
  [ ! -s "$src" ] && { >&2 echo "Usage: $(basename $0) ( -f | --force ) filename"; return 1; }
  if [[ -n $(whereis pandoc|awk '{print $2}') ]]; then
    pandoc -t beamer "$src" -V theme:Pittsburgh -V colortheme:beaver -V fonttheme:professionalfonts -o "${1%md}pdf"
    mkdir -p ~/Documents/slides
    if [[ "$force" = "yes" && -s ~/Documents/slides/"${1%md}pdf" ]]; then
      mv "${1%md}pdf" ~/Documents/slides
    else
      mv -i "${1%md}pdf" ~/Documents/slides
    fi
    xset s off
    [[ -n $(whereis mupdf|awk '{print $2}') ]] && mupdf ~/Documents/slides/"${1%md}pdf"
    xset s on
  else
    >&2 echo "pandoc is required but not found"
  fi
  unset force options src
}

alias ss-on=0xMF-screensaver-on
function 0xMF-screensaver-on {
  [ "$1" -gt 0 ] && xset s $1 || xset s $1 on
  xset s blank
  xset -q | sed -n '/^Screen Saver:/,/^  timeout:/p'
}

alias ss-off=0xMF-screensaver-off
function 0xMF-screensaver-off {
  xset s noblank
  xset s off
  xset -q | sed -n '/^Screen Saver:/,/^  timeout:/p'
}

function serve {

  local port=2212
  dirs=`dirs | tr ' ' '\n' | wc -l`
  if [[ -d "$1" ]]; then
    if [[ -e /usr/bin/ruby ]]; then
      serverstr="/usr/bin/ruby -run -e httpd $1 -p $port"
    else
      pushd $1
      serverstr="/usr/bin/python -m http.server $port"
    fi
  else
    if [[ -e /usr/bin/ruby ]]; then
      serverstr="/usr/bin/ruby -run -e httpd . -p $port"
    else
      serverstr="/usr/bin/python -m http.server $port"
    fi
  fi

    echo "Starting: $serverstr on `ip -br -4 a|sed '/^lo/d'|cut -d' ' -f1,14- | tr -s ' ' | sed 's/ / at /'`"

    eval $serverstr

    d=`dirs | tr ' ' '\n' | wc -l`
    [[ $d -ne $dirs ]] && popd
  unset port
}

function _0xMF-cv-helper {
    case "$(basename $SHELL)" in
      "bash"  ) type -a "$1" ;;
      "ksh"   ) command -V "$1"; typeset -f "$1" ;;
      "zsh"   ) type -f "$1" ;;
      *       ) >&2 echo "not implemented in $SHELL" ;;
    esac
}

function 0xMF-cv {
  local _cv
  if [ -n "$1" ]; then
    _cv=$(_0xMF-cv-helper "$1")
    echo "$_cv"
    if echo "${_cv}" |  grep -qE "(alias for 0xMF|aliased to)"; then
      if [[ "$(basename $SHELL)" = "zsh" ]]; then
        if [[ "$1" != "0xMF-cv" ]]; then
          0xMF-cv $(echo "$_cv"|awk '{print $NF}')
        fi
      else
        if ! echo "$1" |  grep -q "0xMF"; then
          _0xMF-cv-helper $(echo "$_cv" | awk '{print $NF}' | sed "s/\`//;s/'//") 2>/dev/null
        fi
      fi
    fi
  fi
  unset _cv
}

function gdoc {
  # run go doc to get help on arguments passed, otherwise how to use go help doc
  if [ -z "$1" ]; then
    if go doc . 2> /dev/null; then
      _gdoc_helper .
    else
      { go help doc | perl -wnle '($. < 3) || (/^Examples/..\Z) and print'
        echo -e "\nSee also:\n\t0xMF-gdoc-list-cmd:\t\tGo command line utils and more." \
              "\n\t0xMF-gdoc-list-std:\t\tGo std lib." \
              "\n\t0xMF-gdoc-list-vendor:\tAdditional vendor stuff shipped with Go."
      } | less -FeqRSX
    fi
  else
    if [ "$1" = "spec" ]; then
      0xMF-gdoc-spec
      return
     fi
    _gdoc_helper "$@"
  fi
}

function _gdoc_helper {
  if [ "$1" = "help" ]; then
    shift
    go help "$@" | chroma -l go -f terminal256 -s paraiso-dark | less -FeqRSX
    return
  fi
  if which chroma > /dev/null 2>&1; then
    [ -z "$CHROMA_STYLE" ] \
      && { go doc "$@" | chroma -l go -f terminal256 -s paraiso-dark | less -FeqRSX ; }  \
      || { go doc "$@" | chroma -l go -f terminal256 -s "$CHROMA_STYLE" | less -FeqRSX ; }
  else
    go doc "$@"
  fi
}

function 0xMF-gdoc-spec {
  local spec=~/.cache/0xMF/go_spec.html
  if [ -s "$spec" ]; then
    if which chroma > /dev/null 2>&1; then
      [ -z "$CHROMA_STYLE" ] \
      && { w3m -dump "$spec" | chroma -l go -f terminal256 -s paraiso-dark | less -FeqRSX -z$LINES --mouse; }  \
      || { w3m -dump "$spec" | chroma -l go -f terminal256 -s "$CHROMA_STYLE" | less -FeqRSX -z$LINES --mouse; }
    fi
  else
   >&2 echo -e "ERROR: $spec was not found\n\n Try:\n" \
               "\t git clone -b master https://github.com/golang/go /path/to/go/repo\n" \
               "\t cp /path/to/go/repo/doc/*html ~/.cache/0xMF\n"

  fi
}

function hdoc {
  if ! which hoogle > /dev/null 2>&1 ; then
    >&2 echo "hoogle is not installed"
    return
  fi

  # chroma style manni over paraiso-dark seems more suited for hoogle
  (hoogle --info "$@" ; hoogle "$@" --count=50 ) \
  | if which chroma > /dev/null 2>&1; then \
      [ -z "$CHROMA_STYLE" ] \
        && chroma -l hs -f terminal256 -s paraiso-dark \
        || chroma -l hs -f terminal256 -s "$CHROMA_STYLE" ; \
    else : ; fi \
  | less -FeqRSX
}

function 0xMF-ghci-help {
  if [ -z "$1" ];
  then
    { echo ":browse! GHC.Base" | ghci -ignore-dot-ghci | chroma -l hs -f terminal256 -s paraiso-dark | less -FeqRSX; return ; }
  else
    { ( echo ":browse! $1"  | ghci -ignore-dot-ghci | sed 's/^Prelude> //;1d;$d' ;
        echo ":info! $1"    | ghci -ignore-dot-ghci | sed 's/^Prelude> //;1d;$d' \
      )  > /dev/null \
      |chroma -l hs -f terminal256 -s paraiso-dark | less -FeqRSX ; }
  fi
}

alias hgdoc="0xMF-ghci-help"
alias hdoc-ghci="0xMF-ghci-help"

function 0xMF-ri {
  [ -z "$1" ] && { ri --help; echo -e "\nSee also:\n\t0xMF-rdoc-list-all\t\tShow all classes ri knows about."; return; }

  if which chroma > /dev/null 2>&1; then
    (ri -f markdown "$@"; print; ri -a -l "$@") \
    | cat -s \
    | chroma --unbuffered -l TOML -f terminal256 -s rrt \
    | less -FeqRSX
  else
    ri -f ansi "$@"; print; ri -a -l "$@"
  fi
}

function 0xMF-rdoc-list-all {
  ri -l | cut -d: -f1-3 | uniq | pr -4 -T -w $COLUMNS |less -FeqRSX ;
}

function 0xMF-gdoc-list-packages {
  go list std cmd | cut -d/ -f1 | sort -u | column
}

function 0xMF-gdoc-list-std {
  go list std | sed '/^vendor/d' | pr -4 -T -w $COLUMNS | less -FeqRSX
}

function 0xMF-gdoc-list-std-row-wise {
  go list std | sed '/^vendor/d' | pr -a -4 -T -w $COLUMNS | less -FeqRSX
}

function 0xMF-gdoc-list-vendor {
  go list std | sed -n '/^vendor/p' | column -x
}

function 0xMF-gdoc-list-cmd {
  go list cmd | sed '/vendor/d' | pr -4 -T -w $COLUMNS
}

function 0xMF-pydoc {
    [ -z "$CHROMA_STYLE" ] \
      && { pydoc3 "$@" | cat -s | chroma --unbuffered -l go -f terminal256 -s paraiso-dark | less -FeqRSX ; }  \
      || { pydoc3 "$@" | cat -s | chroma --unbuffered -l python -f terminal256 -s "$CHROMA_STYLE" | less -FeqRSX ; }
}
alias pydoc='0xMF-pydoc'

function 0xMF-list-aliases {
  case "${THIS_SHELL}" in
    zsh) alias|cut -d= -f1 | sort -u|pr -4 -T -w $COLUMNS ;;
    *) ;;
  esac
}

function 0xMF-list-functions {
  case "${THIS_SHELL}" in
    bash) declare -F | awk '{ print $NF; }';;
    ksh)  typeset +f ;;
    zsh) print -l ${(ok)functions}\n ;;
    *) ;;
  esac | sed '/^_/d'
}

function 0xMF-list-functions-columnate {
  case "${THIS_SHELL}" in
    bash) declare -F | awk '{ print $NF; }';;
    ksh)  typeset +f ;;
    zsh) print -l ${(ok)functions}\n ;;
    *) ;;
  esac | sed '/^_/d' | pr -4 -T -w ${COLUMNS}
}

alias cpan-installed=0xMF-list-cpan-installed
function 0xMF-list-cpan-installed {
  cpan -l | sort
}

function 0xMF-help {
  local old found
  if [[ -z "$xMFDOC" || ! -d "$xMFDOC" ]]; then
    if [ "$OS" = "BSD" ]; then
      xMFDOC=$(find -L ~ -maxdepth 4 -type d -path "*shells*" -name "0xMF" 2>/dev/null | perl -wlne '/(.*)shells(.*)/ and print "$1doc"' | uniq)
    else
      xMFDOC=$(find -L ~/repos -maxdepth 4 -type d -wholename "*shells/0xMF" 2>/dev/null | sed 's/shells.*/doc/' | uniq)
    fi
    xMFDOC=$(echo $xMFDOC | find -L ~/repos -maxdepth 2 -type d -name "doc" 2>/dev/null | sort -u | tr '\n' ' ')
  fi
  old=$(pwd)

  if [ -z "$1" ]; then
    >&2 echo -e "Usage: 0xMF-help TOPIC, where TOPIC is one or more of:\n\n  $(ls `echo "$xMFDOC"` | sed '/^\/.*:$/d;/^$/d' | sort | fmt)"
  else
    found=$(eval "find -L $xMFDOC -name \"$@\"")
    if [ -n "$found" ]; then
      echo -ne "0xMF-help for $@\n\n"
      local lex=$(\grep -wE "(filetype|ft)" "$f" | perl -wlne '/(.*):(filetype|ft)+?=([^ :]*)?(.*)/ and print "$3"')
      if which chroma > /dev/null 2>&1; then
        if [[ "$lex" != "markdown" && "$lex" != "md"  ]]; then
          eval "chroma -f terminal256 -l $([ -z "${CHROMA_LEXER}" ] && echo sh || echo ${CHROMA_LEXER}) -s $([ -z "${CHROMA_STYLE}" ] && echo rrt || echo ${CHROMA_STYLE} ${found})" | less -FeqRSX
        else
          eval "chroma -f terminal -l $([ -z "${CHROMA_LEXER}" ] && echo md || echo ${CHROMA_LEXER}) -s $([ -z "${CHROMA_STYLE}" ] && echo friendly || echo ${CHROMA_STYLE} ${found})" | less -FeqRSX
        fi
      else
        cat ${found} | less -FeqRSX
      fi
      unset lex
    else
      >&2 echo -e "did not find any help doc for: "$@"\n in: $xMFDOC"
    fi
    cd $old
  fi
  unset old found
}

alias 0xMF-doc='0xMF-help'

# show directory tree
function 0xMF-tree {

  local _tree=$( whereis tree | awk '{print $2}' )
  local depth tpath onlydirs
  if [ -z "${_tree}" ]; then
    >&2 echo "tree command not found"
    return
  fi

  if [ -z "$1" ]; then
    depth="-L 1";
  else
    if [[ $# -eq 1 && ! -d "$1" ]]; then
      depth="-L $1";
      onlydirs=-d
      shift
    else
      for a in "$@"
      do # if $1 is a number use it for maxdepth
        if echo "$a" | grep "^[0-9][0-9]*$" > /dev/null; then
          depth="-L $a";
          shift
        fi
        if [ "$a" = "-d" ]; then
          onlydirs=-d
          shift
        fi
        if [ -d "$a" ] ; then
          tpath="$a"
          shift
        fi
      done
    fi
  fi

  eval "${_tree} $onlydirs $depth $tpath $@"
  unset depth tpath onlydirs

}

function _sddoc {
  wiw=$(find -L ~ -maxdepth 3 -type d -name "*wiwinwlh" 2>/dev/null)
  wiwf=~/.cache/0xMF/wiwinwlh-tutorial.md
  if [ ! -d "${wiw}" ]
  then
    echo -n 'Do you want to clone: "What I Wish I Knew When Learning Haskell"? (Y/n) '
    read REPLY
    if [[ "$REPLY" != "n" &&  "$REPLY" != "N" ]]
    then
      wiwdir=~/repos/z/wiwinwlh
      echo -n 'Location to clone to: (default ~/repos/z/wiwinwlh or Ctrl+C to exit) '
      read REPLY
      [ -n "$REPLY" ] && wiwdir="${REPLY}"
      if git clone https://github.com/sdiehl/wiwinwlh ${wiwdir}; then
        (
          if [ -z "${CHROMA_STYLE}" ]; then
            mkdir -p ~/.cache/0xMF/dotfiles
            chroma -l md -f terminal256 -s emacs "${wiw}/tutorial.md"
          else
            chroma -l md -f terminal256 -s "$CHROMA_STYLE" "${wiw}/tutorial.md"
          fi
        ) > "$wiwf"
        echo -e "\n...installed! Run sddoc again"
      fi
    fi
  fi
}

function sddoc {
  wiwf=~/.cache/0xMF/wiwinwlh-tutorial.md
  if [ -s "$wiwf" ]; then
    less $([ -n "$1" ] && echo "+$1") -FeqRSX "$wiwf"
  else
    _sddoc
  fi
}

function sddoc-refresh {
  unset wiw wiwf
  wiw=$(find -L ~ -maxdepth 3 -type d -name "*wiwinwlh" 2>/dev/null)
  wiwd=~/.cache/0xMF
  mkdir -p $wiwd
  wiwf=${wiwd}/wiwinwlh-tutorial.md
  touch $wiwf
  if [ -d  "$wiw" ]; then
    (
      cd $wiw || return
      git pull > /dev/null
      if [ -z "$CHROMA_STYLE" ]
      then
        chroma -l md -f terminal256 -s emacs "${wiw}/tutorial.md"
      else
        chroma -l md -f terminal256 -s "$CHROMA_STYLE" "${wiw}/tutorial.md"
      fi
    ) > "$wiwf"
  else
    echo "refreshing...What I Wish I Knew When Learning Haskell"
    _sddoc
  fi
}

function m4a2mp3 {
  if [ -z "$1" ]; then
    echo "Usage: m4a2mp3 filename.m4a"
    return
  fi

  local _ffmpeg=$( whereis ffmpeg | awk '{ print $2}' )

  [ -z "${_ffmpeg}" ] && { echo "ffmpeg not found!"; return; }
  if file "$1" | grep -q Audio$; then
    echo -n converting "$1"...
    ${_ffmpeg} -v 5 -y -i "$1" -acodec libmp3lame -ac 2 -ab 192k "${1%m4a}mp3"
    echo done!
  else
    echo "Not an m4a file"
  fi
}

function 0xMF-links {
  local shell=$(basename $SHELL)
  local path=$([ -z "$1" ] && echo "." || echo "$@")

  if [ "$shell" = "ksh" ]; then
    eval "/usr/bin/find ${path} -type l -exec /bin/ls -ld {} \; 2>/dev/null"
  else
    eval "/usr/bin/find ${path} -type l -exec /bin/ls -ld --color=always {} \; 2>/dev/null"
  fi
  unset shell
}

alias magnify=0xMF-magnify
function 0xMF-magnify {
  local w=512 h=128 m=4
  [ "$1" -gt 0 ] && w="$1"
  [ "$2" -gt 0 ] && h="$2"
  [ "$3" -gt 0 ] && m="$3"

  xmag -source "$w"x"$h" -mag "$m"
  unset w h m
}

function 0xMF-hogs {
  local files f nf sz
  if [[ "$1" = "--help" || "$1" = "-h" ]]; then
    >&2 echo -n "Usage: $0 [+-]nnn[M|G|K] [--sort-human]\n" \
                "      Input is -size option of find. Default shows files of 10M or more.\n"
    return
  fi

  if [[ -z "$1"  || "$1" = "--sort-human" ]]; then
    if [ "$(uname)" = "OpenBSD" ]; then
      sz="1950"
    else
      sz="+10M"
    fi
  else
    if [ "$1" != "--sort-human" ]; then
      if [ "$(uname)" = "OpenBSD" ]; then
        sz="$(echo $1|sed 's/[kK]/ * 2/;s/M/ * 4/;s/G/ * 8/'|bc)"
      else
        sz="$1"
      fi
      shift
    fi
  fi

  files=$(find -L "$PWD" -size "$sz" -exec ls -l {} \;)
  if [[ -n "$files" ]]; then
    nf=$(echo "$files" | wc -l)
    if [ $nf -gt 999 ]; then
      echo -e "$files\n$nf files of size $sz or more were under $PWD, far too many to calculate total size."
      return
    fi

    f=$(echo "$files" | awk '{print $5,"+"}' | sed 's/^ *+/0 +/g' | tr '\n' ' ' |sed 's/+ $//g')
    nf=$(whereis numfmt)
    if [ -n "$nf" ]; then
      f=$(echo $f | bc | numfmt --to=iec)
    else
      nf=$(whereis gnumfmt)
      if [ -n "$nf" ]; then
        f=$(echo $f | bc | gnumfmt --to=iec)
      else
        f=$(echo $f | bc)
      fi
    fi

    if [ -n "$f"  ]; then
      if [ "$1" = "--sort-human" ]; then
        files=$(find -L "$PWD" -size "$sz" -exec ls -lh {} \; | sort -hk5,5)
      fi
      nf=$(echo "$files" | wc -l)
        if [ $nf -lt 20 ]; then
          echo -e "$nf files of size $sz or more were under $PWD. Total size: $f.\n$files"
        else
          echo -e "$files\n$nf files of size $sz or more were under $PWD. Total size: $f."
        fi
    else
      echo -e "No files of size $sz or more were found under $PWD"
    fi
  fi

  unset files f nf sz
}
