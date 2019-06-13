"--------------------------------------------------------------------
" Filename:     nice-term.vim
" Purpose:      Vim color file
" Maintainer:   Mark Fernandes
" Last Change: 23 Feb 2008 
"   - added detection of xterm for termcap/terminfo
"   - changed none to none to get it to work correctly on gnone-terminal
"--------------------------------------------------------------------

" Read help on the following topics
" cool help screens
" :he group-name
" :he highlight-groups
" :he cterm-colors

" termcap/terminfo detection
:if &term =~ "xterm"
:  if has("terminfo")
:	set t_Co=8
:	set t_Sf=<Esc>[3%p1%dm
:	set t_Sb=<Esc>[4%p1%dm
":   echo "terminfo"
:  else
:	set t_Co=8
:	set t_Sf=<Esc>[3%dm
:	set t_Sb=<Esc>[4%dm
":   echo "termcap"
:  endif
:endif
":set t_ti= t_te=

" your pick:
"set background=dark	" or light
:set background=dark
:highlight  clear 
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="alt-term"

"hi Normal

" OR

" highlight clear Normal
" set background&
" highlight clear
" if &background == "light"
"   highlight Error ...
"   ...
" else
"   highlight Error ...
"   ...
" endif

" A good way to see what your colorscheme does is to follow this procedure:
" :w 
" :so % 
"
" Then to see what the current setting is use the highlight command.  
" For example,
" 	:hi Cursor
" gives
"	Cursor         xxx guifg=bg guibg=fg 
 	
" Uncomment and complete the commands you want to change from the default.


hi Cursor       cterm=none  ctermbg=none   ctermfg=Red
hi CursorIM     cterm=none  ctermbg=none   ctermfg=Red
hi Directory    cterm=none  ctermbg=none   ctermfg=Cyan
hi DiffAdd      cterm=none  ctermbg=none   ctermfg=Blue
hi DiffChange   cterm=none  ctermbg=none   ctermfg=Blue
hi DiffDelete   cterm=none  ctermbg=none   ctermfg=Red
hi DiffText     cterm=none  ctermbg=none   ctermfg=Yellow
hi ErrorMsg     cterm=none  ctermbg=none   ctermfg=Red
hi VertSplit    cterm=none  ctermbg=none   ctermfg=Cyan
hi Folded       cterm=none  ctermbg=none   ctermfg=Yellow
hi FoldColumn   cterm=none  ctermbg=none   ctermfg=Cyan
hi IncSearch    cterm=none  ctermbg=none   ctermfg=Cyan
hi LineNr       cterm=none  ctermbg=none   ctermfg=DarkCyan
hi ModeMsg      cterm=none  ctermbg=none   ctermfg=DarkRed
hi MoreMsg      cterm=none  ctermbg=none   ctermfg=Cyan
hi NonText      cterm=none  ctermbg=none   ctermfg=Yellow
hi Question     cterm=none  ctermbg=none   ctermfg=Cyan
hi Search       cterm=none  ctermbg=White   ctermfg=none
hi SpecialKey   cterm=none  ctermbg=none   ctermfg=Cyan
hi StatusLine   cterm=none  ctermbg=none   ctermfg=DarkYellow
hi StatusLineNC cterm=none  ctermbg=none   ctermfg=Cyan
hi Title        cterm=none  ctermbg=none   ctermfg=Cyan
hi Visual       cterm=none  ctermbg=White   ctermfg=none
hi VisualNOS    cterm=none  ctermbg=White   ctermfg=none
hi WarningMsg   cterm=none  ctermbg=none   ctermfg=DarkRed
hi WildMenu     cterm=none  ctermbg=none   ctermfg=Cyan
hi Menu         cterm=none  ctermbg=none   ctermfg=DarkBlue
hi Scrollbar    cterm=none  ctermbg=none   ctermfg=Cyan
hi Tooltip      cterm=none  ctermbg=none   ctermfg=Brown

" syntax highlighting groups
hi Comment      cterm=none  ctermbg=none   ctermfg=DarkMagenta
hi Constant     cterm=none  ctermbg=none   ctermfg=DarkYellow
hi Identifier   cterm=none  ctermbg=none   ctermfg=DarkCyan
hi Statement    cterm=none  ctermbg=none   ctermfg=Blue
hi PreProc      cterm=none  ctermbg=none   ctermfg=Red
hi Type         cterm=none  ctermbg=none   ctermfg=Blue
hi Special      cterm=none  ctermbg=none   ctermfg=Yellow
hi Underlined   cterm=none  ctermbg=none   ctermfg=Blue
hi Ignore       cterm=none  ctermbg=none   ctermfg=DarkGray
hi Error        cterm=none  ctermbg=none   ctermfg=White
hi Todo         cterm=none  ctermbg=none   ctermfg=Brown
