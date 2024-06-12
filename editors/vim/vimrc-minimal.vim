" absolute minimal vim settings I cannot live without, no matter what
:if filereadable(expand("$VIMRUNTIME/colors/pablo.vim"))
: colorscheme pablo
:endif

let loaded_matchparen = 1

set nohlsearch
set textwidth=78
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab

au BufNewFile,BufRead *.py  setlocal tabstop=4
