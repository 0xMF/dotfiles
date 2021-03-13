# pleasure.sh: perl like ruby for regular expression needs and more

THIS_SHELL=`ps o command -p $$ | grep -v "^COMMAND$" | tr -d '-' | cut -d' ' -f1`
case "${THIS_SHELL}" in
  bash|ksh|zsh) ;;
  *) >&2 echo "This script probably wont work with your shell, so bailing out now...bye!";
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
   go help doc | perl -wnle '($. < 3) || (/^Examples/..\Z) and print'
   echo -e "\nSee also:\n\tgdoc-list-cmd:\t\tGo command line utils and more" \
           "\n\tgdoc-list-std:\t\tGo std lib" \
           "\n\tgdoc-list-vendor:\tAdditional vendor stuff shipped with Go"
  else
   [ -z "$CHROMA_STYLE" ] \
    && { go doc "$1" | chroma -l go -f terminal256 -s paraiso-dark | less -FeqRSX ; }  \
    || { go doc "$1" | chroma -l go -f terminal256 -s "$CHROMA_STYLE" | less -FeqRSX ; }
  fi
}

function 0xMF-ri {
  [ -z "$1" ] && { ri --help; return; }

   (ri -f markdown "$@"; print; ri -a -l "$@") \
   | cat -s \
   | chroma --unbuffered -l yaml -f terminal256 -s rrt \
   | less -FeqRSX
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
    zsh) print -l ${(ok)functions}\n | sed '/^_/d' | pr -4 -T -w ${COLUMNS} ;;
    *) ;;
  esac
}

function 0xMF-help {

  DOC=$HOME/repos/dotfiles/doc
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
