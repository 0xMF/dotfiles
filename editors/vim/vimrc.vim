"** Global variables for this script

" MYVIM variable used to handle different OS repos/conf locations
"   use expand() because "$HOME", "~" or "$MYVIM" do not work
"   correctly with the let command. vim's function expand() tries to
"   correctly substitute environment variables from the shell, which
"   works if your shell supports that expanded environment.
" Note:   filereadable() does not expand ~ or $HOME  correctly so we
"   use variable my_settings defined above and '.' to join two strings
"   :echoerr expand("$HOME")
"   :echoerr expand("$MYVIM")

:if has ("win32")
:   let $MYVIM=expand("$USERPROFILE/repos/dotfiles/editors/vim")
:   if filereadable(expand("$MYVIM"))
:     echoerr $MYVIM
:   endif
:   if filereadable($VIMRUNTIME . "/mswin.vim")
:     source $VIMRUNTIME/mswin.vim
:   endif
:else
:   let $USER=substitute(system("/usr/bin/id -u"),"\n","","g")
:   let $MYVIM=expand("$HOME/.vim")
:end

set runtimepath=$VIMRUNTIME,$MYVIM,$MYVIM/dependencies,$VIM/vimfiles

"** Keyboard Mappings
:if filereadable($MYVIM . "/my_settings/keymap.vim")
:   so $MYVIM/my_settings/keymap.vim
:else
:   echoerr "no keyboard mappings found"
:endif

"** My Scripts
:if filereadable($MYVIM . "/my_settings/scripts.vim")
:   so $MYVIM/my_settings/scripts.vim
:else
:   echoerr "none of my scripts found"
:endif

"** My Abbreviations
:if filereadable($MYVIM . "/my_settings/abbr.vim")
:    so $MYVIM/my_settings/abbr.vim
:else
:   echoerr "no abbreviations found"
:endif

"** locations of backup directories
:if isdirectory($MYVIM . "/backup")
: set backup
: set backupdir=$MYVIM/backup " all the *~ files go here
: set directory=$MYVIM/backup " all the *.swp files go here
:else
:   echoerr "cannot create backups"
:endif

"** hopefully we are running a fairly modern version of vim (Vim7). If so, use its features
:if (version >= 700) && (has("gui_running"))
    :set nospell
    :set spelllang=en_ca,en_us                " Canadian and US spelling, ca words
    :set spellfile=$MYVIM/spell/latin1.add    " my dictionary
    :set formatlistpat=^\\s*\\d*[\\]:.)}*\\t\ ]\\s*
    " use Ctrl+N Ctrl+P to cycle between open tabs (maximize view on smaller screens)
    :set showtabline=0   " =0 never show tabs, =1 show if 2 or more tabs, =2 always show
    :if (version > 740)                       " possible features for better than  Vim74
    " Wow!! Some elegant colorschemes come shipped with vim. Thanks to:
    :   colorscheme darkblue                  " Bohdan Vlasyuk
    ":  colorscheme industry                  " Shian Lee
    :endif
:endif

"** check if gvim is running and set its options accordingly
:if has ("gui_running")
    :syntax on               " start syntax highlighting
    "** only initialize window size if has not been initialized yet
    :if !exists ("s:my_windowInitialized_variable")
    :   let s:my_windowInitialized_variable=1
    :   let g:fonts_small=0
    :   set guiheadroom=20  "room for window decorations
    :   set guioptions-=T   "hide the toolbar
    :   set guioptions-=m   "hide the menu
    :   set guioptions-=r   "hide the right scrollbar
    :   set guioptions-=L   "hide the left scrollbar
    :   set columns=114     "width in number of cols
    :   set lines=30        "height in number of lines
    :endif

    "** check os version where gvim is running
    :if has ("gui_win32")
    :   winpos 0 0          "make window stick to the top left corner
    :     set guifont=Source_Code_Pro_Semibold:h10:cANSI
    ":   set guifont=Rod:h10:cHEBREW "set font and its size
    :   set bs=2            "backspace over indent,eol,start
    :elseif has ("gui_gtk")
    ":   let hostname = substitute(system("uname -n"),"\n","","g")
    ":   if (hostname == 'name')
    " set font and its size
    :   silent let scp_detected=system("fc-list|grep 'Source Code Pro Medium'|wc -l")[0]
    :   if scp_detected == "2"
    ":     set guifont=Source\ Code\ Pro\ Medium\ 12
    :     set guifont=Source\ Code\ Pro\ Semibold\ 11
    :   else
    :     set guifont=Monospace\ 12
    :   endif
    ":  set guifont=Nimbus\ Mono\ L\ Bold\ 13 "set font and its size
    :   set guioptions+=a   " visual selection can copy to clipboard
    :   set guioptions+=i   " show gvim color icon, instead of default
    :   let &guicursor = &guicursor . ",a:blinkon0"
    "** tags
    :   set tags=./tags,tags,TAGS,
    :   let tagfiles = glob("`/bin/ls -1 $HOME/src/tags/*tags`")
    :   let tagfiles = substitute(tagfiles, "\n", ",", "g")
    :   let &tags.= tagfiles
    :endif

    "** use my colorscheme (nice) if both industry and midnight are not available (win/gtk)
    :if expand(g:colors_name) != "industry"
    :   if filereadable(expand("$MYVIM/colors/midnight.vim"))
    :     colorscheme midnight
    :   elseif filereadable(expand("$MYVIM/colors/nice.vim"))
    :     colorscheme nice
    :   else
    :     colorscheme ron
    :   endif
    : endif
:else
    "** prefer my colorscheme (nice-term) for conosole
    : if filereadable(expand("$MYVIM/colors/nice-term.vim"))
    :   colorscheme nice-term
    :   syntax on               " start syntax highlighting
    : endif
:endif

"** Custom Settings
set nocompatible        " Use VIM not vi
set nonumber            " hides linenumbers by default
set history=50          " keep track of last 50 chars
set textwidth=99        " force text width off (prev at 80 chars/line)
set colorcolumn=100     " show the last column in colour
"set formatprg=par\ -reqw100 " use external par instead of Vim fmt, still avail with gw
set formatoptions=tcqnl " include numbered lists when formatting with gq
set comments-=s1:/*,mb:*,ex:*/  " removes C-style formatting from default
set comments+=fb:*      " let comments begin with *; meaning lists can now have *
set autoindent          " set automatic indenting
set nowrap              " do not force word wrapping on (does not put hard return)
set linebreak           " does not breakup words
set nojoinspaces        " avoids inserting two spaces when joining lines

set encoding=utf-8      " set encoding to Unicode by default

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
": set statusline=%1*%f%m%r%h%w\ [%{&ff}]\ [%Y]\ [ASCII=%b]\ [HEX=0x%B]\ [%l,%v]\ [%{CharCount()}\ CHARS][%{WordCount()}\ WORDS][%p%%:%L\ LINES]\ TIME:\ %{strftime('%c')}
: set statusline=%1*%t%m%r%h%w\ %Y\ [%l,%v]\ [CHARS:%{CharCount()},\ WORDS:%{WordCount()}][%p%%:%L\ LINES]\ %{strftime('%c')}
:else
": set statusline=%f%m%r%h%w\ [%{&ff}]\ [%Y]\ [ASCII=%b]\ [HEX=0x\%B]\ [%l,%v]\ [%{CharCount()}\ CHARS][%{WordCount()}\ WORDS][%p%%:%L\ LINES]\ TIME:\ %{strftime('%c')}
: set statusline=%t%m%r%h%w\ %Y\ [%l,%v]\ [CHARS:%{CharCount()},\ WORDS:%{WordCount()}][%p%%:%L\ LINES]\ %{strftime('%c')}
:endif
set laststatus=2        " always show the statusline

"** Search related options
set incsearch           " make search incremental
set ignorecase          " makes searches case insensitive...
set smartcase           " ...if it didn't have a capital letter

"** Make unnamed buffer the default for clipboard (see line below)
set clipboard=unnamed   " all yanking goes to clipboard


"** Simple scripts for autocommands on file type detectiong
:if !exists("autocommands_loaded")
    : let autocommands_loaded =1
    :filetype on        " enables file type detection.

    " remove trailing spaces on save
    au BufWritePost * call DeleteTrailingSpaces()

    " enable completion, so in insert mode: CTRL+X CTRL+O
    :filetype plugin on
    :set omnifunc=syntaxcomplete#Complete
    :set wildmode=list

    "* AsciiDoc
    au BufRead,BufNewFile   *.asc  setlocal syntax=asciidoc
    au BufRead,BufNewFile   *.ad   setlocal syntax=asciidoc
    au BufRead,BufNewFile   *.ad   setlocal filetype=asciidoc
    au BufRead,BufNewFile   *.adoc setlocal syntax=asciidoc

    "* BibTeX  and LaTeX
    au BufNewFile,BufRead *.bib setlocal nospell
    au BufNewFile,BufRead *.bib setlocal tw=0
    au BufRead            *.dbj setlocal ft=tex

    "* C/C++
    "* Programmer settings for smart C/C++ style indents and commenting
    au FileType c,cpp setlocal cindent
    au BufRead,BufNewFile *.c,*.cpp setlocal comments-=://
    au BufRead,BufNewFile *.c,*.cpp setlocal comments+=mb:*
    au BufRead,BufNewFile *.c,*.cpp setlocal comments+=slO://,mbO://,ebOx:--

    " CMake settings
    autocmd BufRead,BufNewFile *.cmake,CMakeLists.txt,*.cmake.in runtime! indent/cmake.vim
    autocmd BufRead,BufNewFile *.cmake,CMakeLists.txt,*.cmake.in setf cmake
    autocmd BufRead,BufNewFile *.ctest,*.ctest.in setf cmake
    autocmd BufRead,BufNewFile   CMakeLists.txt setlocal syntax=cmake

    "* conf files
    "* options for conf file, turn autowrapping back on and remove numbered lists
    au FileType conf setlocal tw=84
    au FileType conf setlocal formatoptions+=ntc
    au FileType conf setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s* "also called set flp
    au FileType conf setlocal comments+=fb:-

    "* dae files
    "* options for COLLADA files
    au BufRead,BufNewFile *.dae  setlocal filetype=COLLADA
    au FileType COLLADA          setlocal syntax=xml

    "* Elixir files
    au FileType ex setlocal syntax=elixir
    au FileType eex setlocal syntax=eelixir
    au BufEnter,BufRead,BufNewFile *.ex setlocal filetype=elixir
    au BufEnter,BufRead,BufNewFile *.eex setlocal filetype=eelixir

    "* git commit messages
    au FileType gitcommit setlocal spell textwidth=72
    au BufEnter,BufRead,BufNewFile */COMMIT_EDITMSG setlocal filetype=gitcommit
    au BufEnter,BufRead,BufNewFile */MERGE_MSG      setlocal filetype=gitcommit

    "* Vim help files
    au FileType help setlocal nospell spelllang=
    au BufEnter,BufRead,BufNewFile *.vim setlocal nospell spelllang=

    "* Go
    au BufRead,BufNewFile *.go setlocal filetype=go
    "au BufRead,BufNewFile *.go setlocal bomb
    au BufRead,BufNewFile *.go setlocal fileencoding=utf-8
    au BufRead,BufNewFile *.go setlocal nospell
    au BufRead,BufNewFile *.go setlocal textwidth=99
    "au BufRead,BufNewFile *.go setlocal formatprg=par\ -reqw85
    au BufRead,BufNewFile *.go setlocal colorcolumn=100
    au BufRead,BufNewFile *.go setlocal noexpandtab nowrap

    "* Mail
    au FileType mail setlocal textwidth=0 spell

    "* Markdown
    au BufRead,BufNewFile *.md setlocal filetype=markdown
    au BufRead,BufNewFile *.md setlocal spell
    au BufRead,BufNewFile *.md setlocal colorcolumn=100
    au BufRead,BufNewFile *.md setlocal textwidth=99
    au BufRead,BufNewFile *.md setlocal nowrap
    au BufRead,BufNewFile *.md setlocal nonumber
    au BufRead,BufNewFile *.md setlocal guioptions-=r
    au BufRead,BufNewFile *.md setlocal formatoptions-=q
    au BufRead,BufNewFile *.md setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*-\\s*\\[[\ xX]\\]\\s\\+\\\|^\\s*[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:

    "* MAXScript
    au BufRead,BufNewFile *.ms  setlocal filetype=maxscript
    au FileType maxscript       setlocal syntax=maxscript
    au FileType maxscript       colorscheme nice-gui

    "* OKSH files
    au BufNewFile,BufRead .okshrc*,*.oksh :call SetFileTypeSH("sh")

    "* options for PowerShell files
    au BufNewFile,BufRead   *.ps1   setlocal ft=ps1
    au BufNewFile,BufRead   *.psd1  setlocal ft=ps1
    au BufNewFile,BufRead   *.psm1  setlocal ft=ps1
    au BufRead,BufNewFile   *.ps1   setlocal filetype=ps1
    au FileType ps1                 setlocal syntax=ps1
    au FileType ps1                 setlocal fileformat=dos

    "*  Python
    au BufNewFile,BufRead *.py  setlocal tabstop=4
    au BufRead,BufNewFile *.py  setlocal expandtab
    "au BufNewFile,BufRead *.py so <sfile>:h\vim70\ftplugin\python.vim

    "* Ruby
    au BufRead,BufNewFile *.rb setlocal filetype=ruby
    au BufRead,BufNewFile *.rb setlocal fileencoding=utf-8
    au BufRead,BufNewFile *.rb setlocal textwidth=99
    au BufRead,BufNewFile *.rb setlocal colorcolumn=100
    au BufRead,BufNewFile *.rb setlocal noexpandtab nonumber nospell nowrap

    "* options for SCALA files
    au BufRead,BufNewFile *.scala setlocal filetype=scala
    au FileType SCALA             setlocal syntax=scala

    "* SH files
    au FileType sh    setlocal fileformat=unix

    "*  Squirrel
    au BufNewFile,BufRead *.nut setlocal ft=squirrel

    " tmux autodetection
    au BufNewFile,BufRead *.tmux.conf*,tmux.conf* setf tmux

    "* Text file
    "au BufRead,BufNewFile   *.txt setlocal formatprg=par\ -reqw100
    au BufRead,BufNewFile   *.txt setlocal syntax=asciidoc
    au BufRead,BufNewFile   *.txt setlocal colorcolumn=100
    au BufRead,BufNewFile   *.txt setlocal textwidth=99
    au BufRead,BufNewFile   *.txt setlocal nowrap
    au BufRead,BufNewFile   *.txt setlocal nonumber
    au BufRead,BufNewFile   *.txt setlocal guioptions-=r
    au BufRead,BufNewFile   *.txt setlocal spell
    au BufRead,BufNewFile   *.txt setlocal formatoptions+=n
    au BufRead,BufNewFile   *.txt setlocal comments+=n:>
    au BufRead,BufNewFile   *.txt setlocal comments=b:>
    au BufRead,BufNewFile   *.txt setlocal comments+=b:>>
    au BufRead,BufNewFile   *.txt setlocal comments+=b:>>>
    au BufRead,BufNewFile   *.txt setlocal comments+=b:#
    au BufRead,BufNewFile   *.txt setlocal comments+=fb:-
    au BufRead,BufNewFile   *.txt setlocal comments+=fb:*
    au BufRead,BufNewFile   *.txt setlocal comments+=fb:.
    au BufRead,BufNewFile   *.txt setlocal comments+=fb:\|
    au BufRead,BufNewFile   *.txt setlocal nocindent
    au BufRead,BufNewFile   *.txt setlocal autoindent
    "au BufRead,BufNewFile   *.txt setlocal fileencoding=utf-8
    "au FileType asciidoc    :call My_FileType()

    "* Texapp files
    au BufRead,BufNewFile   texapp-*.txt setlocal nonumber
    au BufRead,BufNewFile   texapp-*.txt setlocal wrap
    au BufRead,BufNewFile   texapp-*.txt setlocal textwidth=0
    "au BufRead,BufNewFile   texapp-*.txt setlocal formatprg=par\ -reqw0
    au BufRead,BufNewFile   texapp-*.txt setlocal spell
    au BufRead,BufNewFile   texapp-*.txt setlocal syntax=asciidoc
    au BufRead,BufNewFile   texapp-*.txt setlocal filetype=asciidoc
    au BufRead,BufNewFile   texapp-*.txt setlocal expandtab

    "* VIM files (.vim) settings
    au FileType vim     setlocal fileformat=unix
    au BufRead,BufNewFile *.vim setlocal fileformat=unix

    "* vim help files
    "* switch off the listing of unprintable characters
    au FileType help setlocal nolist
    au FileType help setlocal nospell

    "* wikipedia files
    au BufRead,BufNewFile *.wikipedia.org* :if &ft == 'flexwiki' | setlocal filetype=Wikipedia | endif
    au BufRead,BufNewFile *.wiki   :if &ft == 'flexwiki' |  setlocal filetype=Wikipedia | endif
    au FileType Wikipedia :so $MYVIM/.vim/ftdetect/Wikipedia.vim
    au BufRead,BufNewFile *.wiki setlocal linebreak
    au BufRead,BufNewFile *.wiki setlocal textwidth=0
    "au BufRead,BufNewFile *.wiki setlocal formatprg=par\ -reqw0
    au BufRead,BufNewFile *.wiki setlocal formatoptions=rol
    au BufRead,BufNewFile *.wiki noremap <buffer> k gk
    au BufRead,BufNewFile *.wiki noremap <buffer> j gj
    au BufRead,BufNewFile *.wiki noremap <buffer> <Up> gk
    au BufRead,BufNewFile *.wiki noremap <buffer> <Down> gj
    au BufRead,BufNewFile *.wiki noremap <buffer> 0 g0
    au BufRead,BufNewFile *.wiki noremap <buffer> ^ g^
    au BufRead,BufNewFile *.wiki noremap <buffer> $ g$
    au BufRead,BufNewFile *.wiki inoremap <buffer> <Up> <C-O>gk
    au BufRead,BufNewFile *.wiki inoremap <buffer> <Down> <C-O>gj
    " utf-8 should be setlocal if not already done globally
    au BufRead,BufNewFile *.wiki setlocal fileencoding=utf-8
    au BufRead,BufNewFile *.wiki setlocal matchpairs+=<:>
    au BufRead,BufNewFile *.wiki setlocal comments=n:#,n:*,n:\:,s:{\|,m:\|,ex:\|}

:endif

"* Plugin management (via Vundle)
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
"

" settings for a local setup
:if filereadable(expand("$MYVIM/local.vim"))
:  source $MYVIM/local.vim
:endif

