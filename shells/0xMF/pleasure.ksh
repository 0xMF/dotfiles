# pleasure.sh: perl like ruby for regular expression needs and more

[ -z "$PS1" ] && return

THIS_SHELL=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
case "${THIS_SHELL##/**/}" in
  bash|ksh|zsh) ;;
  *) >&2 echo "This script probably wont work with $THIS_SHELL, so bailing out now...bye!";
     exit 1;;
esac

alias ple='perl -wnle'
alias pale='perl -awnle'
alias pd='perldoc -MPod::Text::Color::Delight'
alias pdoc='perldoc -MPod::Text::Color::Delight'
alias perldoc='perldoc -MPod::Text::Color::Delight'

alias tmux='tmux -u'
alias http-serve="serve"

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

function hdoc {
  if ! which hoogle > /dev/null 2>&1 ; then
    >&2 echo "hoogle is not installed"
    return
  fi

  # chroma style manni over paraiso-dark seems more suited for hoogle
  hoogle "$@" --count=50 \
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
    { echo ':h' | ghci -ignore-dot-ghci | chroma -f terminal256 -s paraiso-dark | less -FeqRSX; return }
  else
    { echo ":browse! $1" | ghci -ignore-dot-ghci | sed 's/^Prelude> //;1d;$d'|chroma -l hs -f terminal256 -s paraiso-dark | less -FeqRSX }
  fi
}

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

function 0xMF-doc {

  DOC=$HOME/repos/dotfiles/doc
  if [ ! -d $DOC ]; then
    DOC=$(find -L $HOME/repos -type d -name 0xMF|sed 's/shells.*/doc/'|uniq)
  fi
  old=$(pwd)

  if [ -z "$1" ]; then
    >&2 echo "Usage: 0xMF-help TOPIC, where TOPIC is one or more of: `ls $DOC|fmt`"
  else
    if [ ! -d "$DOC" ]; then
      >&2 echo "Not found: $DOC"
    else
      cd $DOC
      for f in "`echo $@`"
      do
        if [ -f $f ]; then
          echo -n "Displaying help for:\t ${f}\n--------------------\n"
          if which chroma > /dev/null 2>&1; then
            chroma -l sh -f terminal256 -s `[ -z $CHROMA_STYLE ] && echo rrt || echo $CHROMA_STYLE` ${f}
          else
            cat ${f}
          fi
          echo
        fi
      done
      cd $old
    fi
  fi
}

alias 0xMF-help='0xMF-doc'

# show directory tree
function 0xMF-tree {

  if ! which tree > /dev/null 2>&1; then
    >&2 echo "tree command not found"
    return
  fi

  if [ -z "$1" ]; then
    depth="-L 1";
  else
    # if $1 is a number use it for maxdepth
    if echo "$1" | grep "^[0-9][0-9]*$" > /dev/null; then
      depth="-L $1";
      shift
      [ -d "$1" ] && tpath="$1"
    else
      tpath="$1"
      depth="-L 1"
    fi
  fi

  [[ -n "$tpath" && -n "$1" ]] && shift

  eval "tree $depth $tpath $@"
  unset depth tpath

}

function _sddoc {
  wiw=$(find -L ~ -maxdepth 3 -type d -name "*wiwinwlh")
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
          if [ -z "$CHROMA_STYLE" ]; then
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
  wiw=$(find -L ~ -maxdepth 3 -type d -name "*wiwinwlh")
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
