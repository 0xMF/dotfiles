" vimrc

"** Custom Settings
set nocompatible        " Use VIM not vi
syntax on               " start syntax highlighting
set number              " display linenumbers in text
set history=50          " keep track of last 50 chars
set textwidth=70        " no of chars/line
set formatoptions=tcqnl " include numbered lists when formatting with gq
set autoindent          " set automatic indenting
set wrap                " force word wrapping on (does not put hard return)
set linebreak           " does not breakup words
set nojoinspaces        " avoids inserting two spaces when joining lines

"** Tab settings
set tabstop=2           " change tabstop from 8 (default) to 2
set shiftwidth=2        " no of spaces for each indent (previous default = 8)
set softtabstop=2       " set softtabstop
set expandtab           " expand tabs to spaces
set smarttab            " convert spaces to tabs for quicker editing
set visualbell t_vb=    " no bell and no visual bell (flashing)
set whichwrap=b,s,<,>,[,]   " whichwrap for Windows style arrow keys

"* status line gives information about the file, the character under the cursor and its position
"* adjust for root user (shows status line in red)
:if  $USER == 0
: hi User1 cterm=NONE    ctermfg=red    ctermbg=white  guifg=red    guibg=white
: set statusline=%1*%f%m%r%h%w\ [%{&ff}]\ [%Y]\ [ASCII=\%3.3b]\ [HEX=\%02.2B]\ [%l,%v][%p%%]\ [TOTAL=%L]
:else
: set statusline=%f%m%r%h%w\ [%{&ff}]\ [%Y]\ [ASCII=\%3.3b]\ [HEX=\%02.2B]\ [%l,%v][%p%%]\ [TOTAL=%L]
:endif
set laststatus=2        " always show the statusline
