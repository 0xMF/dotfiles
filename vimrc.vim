" vimrc
"

"  MYVIM variable used to handle different OS repos/conf locations
" use expand() because "$HOME", "~" or "$MYVIM" do not work
" correctly with the let command. vim's function expand() tries to
" correctly substitute environment variables from the shell.
:if has ("gui_win32")
  :let $MYVIM=expand("$HOME")
:else
  :let $MYVIM=expand("$HOME")
  :let $USER=substitute(system("/usr/bin/id -u"),"\n","","g")
:endif

"** Global variables for this script
" my_settings_path has location of .vim/my_settings directory in
" user's home directory. We use expand() because "$HOME" or "~" do not
" work correctly with the let command. expand(), tries to expand the
" environment variables from the shell, which works if your shell
" supports that expanded environment.
:let my_settings_path= expand("$HOME/.vim/my_settings")

"** check if gvim is running and set its options accordingly
:if has ("gui_running")
    "** only initialize window size if has not been initialized yet
    :if !exists ("s:my_windowInitialized_variable")
    :   let s:my_windowInitialized_variable=1
    :   set guiheadroom=20  "room for window decorations
    :   set guioptions-=T   "hide the toolbar
    :   set guioptions-=m   "hide the menu
    :   set columns=114     "width in number of cols
    :   set lines=30        "height in number of lines
    :   set runtimepath=$VIMRUNTIME,$MYVIM/.vim
    :endif

    "** check os version where gvim is running
    :if has ("gui_win32")
    :   winpos 0 0          "make window stick to the top left corner
    :   set guifont=Rod:h10:cHEBREW "set font and its size
    :   set bs=2            "backspace over indent,eol,start
    :elseif has ("gui_gtk")
    :   set guifont=Nimbus\ Mono\ L\ Bold\ 13 "set font and its size
    :   set guioptions+=a   " visual selection can copy to clipboard
    :   set guioptions+=i   " show gvim color icon, instead of default
    :   let &guicursor = &guicursor . ",a:blinkon0"
    :endif
:endif


"** Custom Settings
set nocompatible        " Use VIM not vi
syntax on               " start syntax highlighting
set number              " display linenumbers in text
set history=50          " keep track of last 50 chars
set textwidth=80        " no of chars/line
set formatprg=par\ -reqw80  " use external par instead of Vim fmt, still avail with gw
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
set selectmode=mouse,key selection=inclusive  " when does select mode begin
set mousemodel=popup keymodel=startsel        " type of mouse/keyboard behaviour

"* status line gives information about the file, the character under the cursor and its position
"* adjust for root user (shows status line in red)
"* status line gives information about the file, the character under the cursor and its position
:if  $USER == 0
: hi User1 cterm=NONE    ctermfg=red    ctermbg=white  guifg=red    guibg=white
: set statusline=%1*%f%m%r%h%w\ [%{&ff}]\ [%Y]\ [ASCII=\%3.3b]\ [HEX=\%02.2B]\ [%l,%v]\ [%{CharCount()}\ CHARS][%{WordCount()}\ WORDS][%p%%:%L\ LINES]
:else
: set statusline=%f%m%r%h%w\ [%{&ff}]\ [%Y]\ [ASCII=\%3.3b]\ [HEX=\%02.2B]\ [%l,%v]\ [%{CharCount()}\ CHARS][%{WordCount()}\ WORDS][%p%%:%L\ LINES]
:endif
set laststatus=2        " always show the statusline

"** locations of backup directories
set backup
set backupdir=$MYVIM/.vim/backup " all the *~ files go here
set directory=$MYVIM/.vim/backup " all the *.swp files go here

"** Keyboard Mappings
"   sourced from ~/.vim/my_settings/mappings.vim
"   Note:   filereadable() does not expand ~ or $HOME  correctly so we
"   use variable my_settings defined above and '.' to join two strings
:if filereadable(my_settings_path . "/keymap.vim")
:    so ~/.vim/my_settings/keymap.vim
:else
:   echoerr "no keyboard mappings found"
:endif

"** My Scripts
:if filereadable(my_settings_path . "/scripts.vim")
:   so $MYVIM/.vim/my_settings/scripts.vim
:else
:   echoerr "none of my scripts found"
:endif


"
"** Vim70 features
:if (version >= 700) && (has("gui_running"))
:   set nospell
:   set spelllang=en_ca,en_us       " Canadian and US spelling, ca words
:   set spellfile=$MYVIM/.vim/spell/latin1.add   " my dictionary
:   set formatlistpat=^\\s*\\d\\+[\\]:.)}\*\\t\ ]\\s* "also called set flp
:   set showtabline=1   " =0 never show tabs, =1 show if 2 or more tabs, =2 always show
:endif


"** Simple scripts for autocommands on file type detectiong
:if !exists("autocommands_loaded")
    : let autocommands_loaded =1
    :filetype on        " enables file type detection.

    "* BibTeX  and LaTeX
    : au BufNewFile,BufRead *.bib setlocal nospell
    : au BufNewFile,BufRead *.bib setlocal tw=0
    : au BufRead            *.dbj setlocal ft=tex

    "* C/C++
    "* Programmer settings for smart C/C++ style indents and commenting
    au FileType c,cpp :setlocal cindent
    au BufRead,BufNewFile *.c,*.cpp :setlocal comments-=://
    au BufRead,BufNewFile *.c,*.cpp :setlocal comments+=slO://,mbO://,ebOx:--

     " CMake settings
    :autocmd BufRead,BufNewFile *.cmake,CMakeLists.txt,*.cmake.in runtime! indent/cmake.vim
    :autocmd BufRead,BufNewFile *.cmake,CMakeLists.txt,*.cmake.in setf cmake
    :autocmd BufRead,BufNewFile *.ctest,*.ctest.in setf cmake
    :autocmd BufRead,BufNewFile   CMakeLists.txt :setlocal syntax=cmake

    "* conf files
    "* options for conf file, turn autowrapping back on and remove numbered lists
    "au FileType conf :setlocal comments+=fb:-
    au FileType conf :setlocal formatoptions+=ntc
    au FileType conf :setlocal formatlistpat=^\\s*-[\\t\ ]\\s*
    au FileType conf :setlocal comments+=fb:-
    au FileType conf :setlocal tw=84

    "* dae files
    "* options for COLLADA files
    au BufRead,BufNewFile *.dae  :setlocal filetype=COLLADA
    au FileType COLLADA          :setlocal syntax=xml

     "* Go
    :au BufRead,BufNewFile *.go setlocal filetype=go
    ":au BufRead,BufNewFile *.go setlocal bomb
    :au BufRead,BufNewFile *.go setlocal fileencoding=utf-8
    :au BufRead,BufNewFile *.go setlocal spell
    :au BufRead,BufNewFile *.go setlocal textwidth=85
    :au BufRead,BufNewFile *.go setlocal colorcolumn=85
    :au BufRead,BufNewFile *.go setlocal formatprg=par\ -reqw85
    :au BufRead,BufNewFile *.go setlocal noexpandtab

    "* Markdown
    :au BufRead,BufNewFile *.md setlocal filetype=markdown

    "*  Squirrel
    : au BufNewFile,BufRead *.nut setlocal ft=squirrel

    " tmux autodetection
    : au BufNewFile,BufRead *.tmux.conf*,tmux.conf* setf tmux

    "*  Python
    : au BufNewFile,BufRead *.py setlocal ts=4
    : au BufRead,BufNewFile *.py  :setlocal expandtab
    ": au BufNewFile,BufRead *.py so <sfile>:h\vim70\ftplugin\python.vim


    "* BibTeX  and LaTeX
    : au BufNewFile,BufRead *.bib setlocal nospell
    : au BufNewFile,BufRead *.bib setlocal tw=0
    : au BufRead            *.dbj setlocal ft=tex

    "* C/C++
    "* Programmer settings for smart C/C++ style indents and commenting
    au FileType c,cpp :setlocal cindent
    au BufRead,BufNewFile *.c,*.cpp :setlocal comments-=://
    au BufRead,BufNewFile *.c,*.cpp :setlocal comments+=mb:*
    au BufRead,BufNewFile *.c,*.cpp :setlocal comments+=slO://,mbO://,ebOx:--

    "* conf files
    "* options for conf file, turn autowrapping back on and remove numbered lists
    au FileType conf :setlocal tw=84
    au FileType conf :setlocal formatoptions+=ntc
    au FileType conf :setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s* "also called set flp
    au FileType conf :setlocal comments+=fb:-

    "* dae files
    "* options for COLLADA files
    au BufRead,BufNewFile *.dae  :setlocal filetype=COLLADA
    au FileType COLLADA          :setlocal syntax=xml

    "* Markdown
    :au BufRead,BufNewFile *.md setlocal filetype=markdown

    "* MAXScript
    au BufRead,BufNewFile *.ms  :setlocal filetype=maxscript
    au FileType maxscript       :setlocal syntax=maxscript
    au FileType maxscript       :colorscheme nice-gui

    "* SCALA files
    au BufRead,BufNewFile *.scala :setlocal filetype=scala
    au FileType SCALA             :setlocal syntax=scala

    "* SH files
    au FileType sh    :setlocal fileformat=unix

    "*  Squirrel
    : au BufNewFile,BufRead *.nut setlocal ft=squirrel

    " tmux autodetection
    : au BufNewFile,BufRead *.tmux.conf*,tmux.conf* setf tmux

    "* Text file
    au BufRead,BufNewFile   *.txt :setlocal textwidth=100
    au BufRead,BufNewFile   *.txt :setlocal formatprg=par\ -reqw100
    au BufRead,BufNewFile   *.txt :setlocal syntax=asciidoc
    au BufRead,BufNewFile   *.txt :setlocal spell
    au BufRead,BufNewFile   *.txt :setlocal formatoptions+=n
    au BufRead,BufNewFile   *.txt :setlocal comments+=n:>
    au BufRead,BufNewFile   *.txt :setlocal comments=b:>
    au BufRead,BufNewFile   *.txt :setlocal comments+=b:>>
    au BufRead,BufNewFile   *.txt :setlocal comments+=b:>>>
    au BufRead,BufNewFile   *.txt :setlocal comments+=b:#
    au BufRead,BufNewFile   *.txt :setlocal comments+=fb:-
    au BufRead,BufNewFile   *.txt :setlocal comments+=fb:*
    au BufRead,BufNewFile   *.txt :setlocal comments+=fb:.
    au BufRead,BufNewFile   *.txt :setlocal comments+=fb:\|
    au BufRead,BufNewFile   *.txt :setlocal nocindent
    au BufRead,BufNewFile   *.txt :setlocal autoindent

    "* Texapp files
    au BufRead,BufNewFile   texapp-*.txt :setlocal nonumber
    au BufRead,BufNewFile   texapp-*.txt :setlocal wrap
    au BufRead,BufNewFile   texapp-*.txt :setlocal textwidth=0
    au BufRead,BufNewFile   texapp-*.txt :setlocal formatprg=par\ -req
    au BufRead,BufNewFile   texapp-*.txt :setlocal spell
    au BufRead,BufNewFile   texapp-*.txt :setlocal syntax=asciidoc
    au BufRead,BufNewFile   texapp-*.txt :setlocal filetype=asciidoc
    au BufRead,BufNewFile   texapp-*.txt :setlocal expandtab

    "* options for PowerShell files
    au BufNewFile,BufRead   *.ps1   setlocal ft=ps1
    au BufNewFile,BufRead   *.psd1  setlocal ft=ps1
    au BufNewFile,BufRead   *.psm1  setlocal ft=ps1
    au BufRead,BufNewFile   *.ps1   :setlocal filetype=ps1
    au FileType ps1                 :setlocal syntax=ps1
    au FileType ps1                 :setlocal fileformat=dos

    "*  Python
    : au BufNewFile,BufRead *.py setlocal ts=4
    : au BufRead,BufNewFile *.py  :setlocal noexpandtab

    "* VIM files (.vim) settings
    au FileType vim     :setlocal fileformat=unix
    au BufRead,BufNewFile *.vim :setlocal fileformat=unix

    "* vim help files
    "* switch off the listing of unprintable characters
    au FileType help :setlocal nolist
    au FileType help :setlocal nospell

    "* wikipedia files
    au BufRead,BufNewFile *.wikipedia.org* :if &ft == 'flexwiki' | set filetype=Wikipedia | endif
    au BufRead,BufNewFile *.wiki   :if &ft == 'flexwiki' |  set filetype=Wikipedia | endif
    au FileType Wikipedia :so $MYVIM/.vim/ftdetect/Wikipedia.vim
    au BufRead,BufNewFile *.wiki :setlocal linebreak
    au BufRead,BufNewFile *.wiki :setlocal textwidth=0
    au BufRead,BufNewFile *.wiki :setlocal formatprg=par\ -req
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
