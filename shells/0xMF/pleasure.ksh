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
   go doc "$1" | chroma -l go -f terminal256 -s paraiso-dark | less -FeqRSX
  fi
}

function gdoc-list-std {
  go list std | sed '/^vendor/d' | pr -4 -T -w $COLUMNS | less -FeqRSX
}

function gdoc-list-std-row-wise {
  go list std | sed '/^vendor/d' | pr -a -4 -T -w $COLUMNS | less -FeqRSX
}

function gdoc-list-vendor {
  go list std | sed -n '/^vendor/p' | column -x
}

function gdoc-list-cmd {
  go list cmd | sed '/vendor/d' | pr -4 -T -w $COLUMNS
}

