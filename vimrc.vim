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
set selectmode=mouse,key selection=inclusive  " when does select mode begin
set mousemodel=popup keymodel=startsel        " type of mouse/keyboard behaviour

"* status line gives information about the file, the character under the cursor and its position
"* adjust for root user (shows status line in red)
:if  $USER == 0
: hi User1 cterm=NONE    ctermfg=red    ctermbg=white  guifg=red    guibg=white
: set statusline=%1*%f%m%r%h%w\ [%{&ff}]\ [%Y]\ [ASCII=\%3.3b]\ [HEX=\%02.2B]\ [%l,%v][%p%%]\ [TOTAL=%L]
:else
: set statusline=%f%m%r%h%w\ [%{&ff}]\ [%Y]\ [ASCII=\%3.3b]\ [HEX=\%02.2B]\ [%l,%v][%p%%]\ [TOTAL=%L]
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
    :au BufRead,BufNewFile *.go set spell
    :au BufRead,BufNewFile *.go set textwidth=85
    :au BufRead,BufNewFile *.go set colorcolumn=85
    :au BufRead,BufNewFile *.go set noexpandtab

    "* Markdown
    :au BufRead,BufNewFile *.md set filetype=markdown

    "*  Squirrel
    : au BufNewFile,BufRead *.nut set ft=squirrel

    " tmux autodetection
    : au BufNewFile,BufRead *.tmux.conf*,tmux.conf* setf tmux

    "*  Python
    : au BufNewFile,BufRead *.py set ts=4
    : au BufRead,BufNewFile *.py  :set expandtab
    ": au BufNewFile,BufRead *.py so <sfile>:h\vim70\ftplugin\python.vim


    "* BibTeX  and LaTeX
    : au BufNewFile,BufRead *.bib set nospell
    : au BufNewFile,BufRead *.bib set tw=0
    : au BufRead            *.dbj set ft=tex

    "* C/C++
    "* Programmer settings for smart C/C++ style indents and commenting
    au FileType c,cpp :set cindent
    au BufRead,BufNewFile *.c,*.cpp :set comments-=://
    au BufRead,BufNewFile *.c,*.cpp :set comments+=mb:*
    au BufRead,BufNewFile *.c,*.cpp :set comments+=slO://,mbO://,ebOx:--

    "* conf files
    "* options for conf file, turn autowrapping back on and remove numbered lists
    au FileType conf :set tw=84
    au FileType conf :set formatoptions+=ntc
    au FileType conf :set formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s* "also called set flp
    au FileType conf :set comments+=fb:-

    "* dae files
    "* options for COLLADA files
    au BufRead,BufNewFile *.dae  :set filetype=COLLADA
    au FileType COLLADA          :set syntax=xml

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

    "* Texapp files
    au BufRead,BufNewFile   texapp-*.txt :set nowrap
    au BufRead,BufNewFile   texapp-*.txt :set textwidth=0
    au BufRead,BufNewFile   texapp-*.txt :set spell
    au BufRead,BufNewFile   texapp-*.txt :set syntax=asciidoc
    au BufRead,BufNewFile   texapp-*.txt :set filetype=asciidoc
    au BufRead,BufNewFile   texapp-*.txt :set expandtab

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
