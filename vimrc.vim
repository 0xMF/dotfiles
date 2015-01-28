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

"** Simple scripts for autocommands on file type detectiong
:if !exists("autocommands_loaded")
    : let autocommands_loaded =1
    :filetype on        " enables file type detection.

    "* BibTeX  and LaTeX
    : au BufNewFile,BufRead *.bib set nospell
    : au BufNewFile,BufRead *.bib set tw=0
    : au BufRead            *.dbj set ft=tex

    "* C/C++
    "* Programmer settings for smart C/C++ style indents and commenting
    au FileType c,cpp :set cindent
    au BufRead,BufNewFile *.c,*.cpp :set comments-=://
    au BufRead,BufNewFile *.c,*.cpp :set comments+=slO://,mbO://,ebOx:--

     " CMake settings
    :autocmd BufRead,BufNewFile *.cmake,CMakeLists.txt,*.cmake.in runtime! indent/cmake.vim
    :autocmd BufRead,BufNewFile *.cmake,CMakeLists.txt,*.cmake.in setf cmake
    :autocmd BufRead,BufNewFile *.ctest,*.ctest.in setf cmake
    :autocmd BufRead,BufNewFile   CMakeLists.txt :set syntax=cmake

    "* conf files
    "* options for conf file, turn autowrapping back on and remove numbered lists
    "au FileType conf :set comments+=fb:-
    au FileType conf :set formatoptions+=ntc
    au FileType conf :set formatlistpat=^\\s*-[\\t\ ]\\s*
    au FileType conf :set comments+=fb:-
    au FileType conf :set tw=84

    "* dae files
    "* options for COLLADA files
    au BufRead,BufNewFile *.dae  :set filetype=COLLADA
    au FileType COLLADA          :set syntax=xml

     "* Go
    :au BufRead,BufNewFile *.go set filetype=go
    ":au BufRead,BufNewFile *.go setlocal bomb
    :au BufRead,BufNewFile *.go setlocal fileencoding=utf-8

    "* Markdown
    :au BufRead,BufNewFile *.md set filetype=markdown

    "* MAXScript
    au BufRead,BufNewFile *.ms  :set filetype=maxscript
    au FileType maxscript       :set syntax=maxscript
    au FileType maxscript       :colorscheme nice-gui

    "* SCALA files
    au BufRead,BufNewFile *.scala :set filetype=scala
    au FileType SCALA             :set syntax=scala

    "* SH files
    au FileType sh    :set fileformat=unix

    "*  Squirrel
    : au BufNewFile,BufRead *.nut set ft=squirrel

    " tmux autodetection
    : au BufNewFile,BufRead *.tmux.conf*,tmux.conf* setf tmux

    "* Text file
    au BufRead,BufNewFile   *.txt :set textwidth=100
    au BufRead,BufNewFile   *.txt :set syntax=asciidoc
    au BufRead,BufNewFile   *.txt :set spell
    au BufRead,BufNewFile   *.txt :set formatoptions+=n
    au BufRead,BufNewFile   *.txt :set comments+=n:>
    au BufRead,BufNewFile   *.txt :set comments=b:>
    au BufRead,BufNewFile   *.txt :set comments+=b:>>
    au BufRead,BufNewFile   *.txt :set comments+=b:>>>
    au BufRead,BufNewFile   *.txt :set comments+=b:#
    au BufRead,BufNewFile   *.txt :set comments+=fb:-
    au BufRead,BufNewFile   *.txt :set comments+=fb:*
    au BufRead,BufNewFile   *.txt :set comments+=fb:.
    au BufRead,BufNewFile   *.txt :set comments+=fb:\|
    au BufRead,BufNewFile   *.txt :set nocindent
    au BufRead,BufNewFile   *.txt :set autoindent

    "* options for PowerShell files
    au BufNewFile,BufRead   *.ps1   set ft=ps1
    au BufNewFile,BufRead   *.psd1  set ft=ps1
    au BufNewFile,BufRead   *.psm1  set ft=ps1
    au BufRead,BufNewFile   *.ps1   :set filetype=ps1
    au FileType ps1                 :set syntax=ps1
    au FileType ps1                 :set fileformat=dos

    "*  Python
    : au BufNewFile,BufRead *.py set ts=4
    : au BufRead,BufNewFile *.py  :set noexpandtab

    "* VIM files (.vim) settings
    au FileType vim     :set fileformat=unix
    au BufRead,BufNewFile *.vim :set fileformat=unix

    "* vim help files
    "* switch off the listing of unprintable characters
    au FileType help :set nolist
    au FileType help :set nospell

    "* wikipedia files
    au BufRead,BufNewFile *.wikipedia.org* :if &ft == 'flexwiki' | set filetype=Wikipedia | endif
    au BufRead,BufNewFile *.wiki   :if &ft == 'flexwiki' |  set filetype=Wikipedia | endif
    au FileType Wikipedia :so $MYVIM/.vim/ftdetect/Wikipedia.vim
    au BufRead,BufNewFile *.wiki :setlocal linebreak
    au BufRead,BufNewFile *.wiki :setlocal textwidth=0
    au BufRead,BufNewFile *.wiki :setlocal formatoptions=rol
    au BufRead,BufNewFile *.wiki :noremap <buffer> k gk
    au BufRead,BufNewFile *.wiki :noremap <buffer> j gj
    au BufRead,BufNewFile *.wiki :noremap <buffer> <Up> gk
    au BufRead,BufNewFile *.wiki :noremap <buffer> <Down> gj
    au BufRead,BufNewFile *.wiki :noremap <buffer> 0 g0
    au BufRead,BufNewFile *.wiki :noremap <buffer> ^ g^
    au BufRead,BufNewFile *.wiki :noremap <buffer> $ g$
    au BufRead,BufNewFile *.wiki :inoremap <buffer> <Up> <C-O>gk
    au BufRead,BufNewFile *.wiki :inoremap <buffer> <Down> <C-O>gj

    " utf-8 should be set if not already done globally
    au BufRead,BufNewFile *.wiki :setlocal fileencoding=utf-8
    au BufRead,BufNewFile *.wiki :setlocal matchpairs+=<:>
    au BufRead,BufNewFile *.wiki :setlocal comments=n:#,n:*,n:\:,s:{\|,m:\|,ex:\|}

:endif
