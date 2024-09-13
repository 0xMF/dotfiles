" absolute minimal vim settings I cannot live without, no matter what
:if filereadable(expand("$VIMRUNTIME/colors/pablo.vim"))
: colorscheme pablo
:endif

:if filereadable(expand("$VIMRUNTIME/colors/darkblue.vim"))
: colorscheme darkblue
:endif

let loaded_matchparen = 1

set expandtab
set mousemodel=popup keymodel=startsel
set nohlsearch
set selectmode=mouse,key selection=inclusive
set shiftwidth=2
set softtabstop=2
set textwidth=78
set tabstop=2

au BufNewFile,BufRead *.py  setlocal tabstop=4
au BufWritePost * call DeleteTrailingSpaces()

syntax on

imap ;n <ESC>:set number!a<CR>
imap ;l <ESC>:set list!a<CR>
imap ;w <ESC>:set wrap!a<CR>

map b <PageUp>
map f <PageDown>
map <BS> <PageUp>
map <Space> <PageDown>
map <S-Space> <PageUp>
map <Space> <C-f>
map ;<Space> <PageUp>

map ;h <Esc>:set hlsearch!<CR>
map ;l :set list!<CR>
map ;n :set number!<CR>
map ;w :set wrap!<CR>
map <C-n> <Esc>:call BufferNext()<CR>
map <C-p> <Esc>:call BufferPrevious()<CR>

function! DeleteTrailingSpaces()
  let l:savecursor = winsaveview()
  :silent :%g/ *$/:s/ *$//
  call winrestview(l:savecursor)
endfunction

" get previous buffer for console vim
function! BufferPrevious()
  :set showtabline=0   " =0 never show tabs, =1 show if 2 or more tabs, =2 always show
  let mybuflist = getbufinfo()
  :if len(mybuflist) < 2
  : echo "no previous buffer"
  :else
  : bprevious
  : if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
  :   exe "normal! g`\""
  : endif
  :endif
endfunction

" get next buffer function! BufferNext()
function! BufferNext()
  :set showtabline=0   " =0 never show tabs, =1 show if 2 or more tabs, =2 always show
  let mybuflist = getbufinfo()
  :if len(mybuflist) < 2
  : echo "no next buffer"
  :else
  : bnext
  " see restore-cursor
  : if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
  :   exe "normal! g`\""
  : endif
  :endif
endfunction
