" absolute minimal vim settings I cannot live without, no matter what
:if filereadable(expand("$VIMRUNTIME/colors/pablo.vim"))
: colorscheme pablo
:endif

:if filereadable(expand("$VIMRUNTIME/colors/darkblue.vim"))
: colorscheme darkblue
:endif

let loaded_matchparen = 1

set nohlsearch
set textwidth=78
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab

au BufNewFile,BufRead *.py  setlocal tabstop=4

map b <C-B>
map f <PageDown>
map ;h <Esc>:set hltsearch!<CR>
map ;l <Esc>:set list!<CR>
map ;n <Esc>:set number!<CR>
map ;w <Esc>:set wrap!<CR>
map <Space> <PageDown>
map <S-Space> <PageUp>
