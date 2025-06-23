"------------------------------------------------------------------------------
" Filename:     nice-term.vim
" Purpose:      Vim color file
" Maintainer:   Mark Fernandes
" Last Change:  2023 Aug 07
"   - added detection of xterm for termcap/terminfo
"   - changed none to none to get it to work correctly on gnone-terminal
"   - $VIMRUNTIME/syntax/hitest.vim corrected many unreadable bg/fg combos
"   - tested on dark (none) background only, light background still pending
"------------------------------------------------------------------------------

" First, read help on the following topics
"   :help group-name
"   :help highlight-groups
"   :help cterm-colors
"
" Then to see existing setup, run
"   :highlight
"

" termcap/terminfo detection
":if &term =~ "xterm"
":  if has("terminfo")
":  set t_Co=8
":  set t_Sf=<Esc>[3%p1%dm
":  set t_Sb=<Esc>[4%p1%dm
":   echo "terminfo"
":  else
":  set t_Co=8
":  set t_Sf=<Esc>[3%dm
":  set t_Sb=<Esc>[4%dm
":   echo "termcap"
":  endif
":endif
":set t_ti= t_te=

" your pick:
"set background=dark  " or light
:set background=dark
:highlight  clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="nice-term"

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
" :hi Cursor
" gives
" Cursor         xxx guifg=bg guibg=fg

" Uncomment and complete the commands you want to change from the default.

hi Normal      cterm=none   ctermbg=none   ctermfg=none cterm=none   guibg=Black   guifg=White

hi Cursor       cterm=none  ctermbg=none   ctermfg=Red
hi CursorLine   cterm=reverse ctermfg=white
hi CursorIM     cterm=none  ctermbg=none   ctermfg=Red
hi DiffAdd      cterm=none  ctermbg=none   ctermfg=LightBlue
hi DiffChange   cterm=none  ctermbg=none   ctermfg=LightBlue
hi DiffDelete   cterm=none  ctermbg=none   ctermfg=Red
hi DiffText     cterm=none  ctermbg=none   ctermfg=Red
hi Directory    cterm=none  ctermbg=none   ctermfg=Cyan
hi ErrorMsg     cterm=none  ctermbg=none   ctermfg=Red
hi FoldColumn   cterm=none  ctermbg=none   ctermfg=Cyan
hi Folded       cterm=none  ctermbg=none   ctermfg=DarkRed
hi IncSearch    cterm=none  ctermbg=none   ctermfg=Cyan
hi LineNr       cterm=none  ctermbg=none   ctermfg=DarkCyan
hi Menu         cterm=none  ctermbg=none   ctermfg=DarkBlue
hi ModeMsg      cterm=none  ctermbg=none   ctermfg=DarkRed
hi MoreMsg      cterm=none  ctermbg=none   ctermfg=Cyan
hi NonText      cterm=none  ctermbg=none   ctermfg=DarkCyan
hi Question     cterm=none  ctermbg=none   ctermfg=Cyan
hi Scrollbar    cterm=none  ctermbg=none   ctermfg=Cyan
hi Search       cterm=bold  ctermbg=White  ctermfg=Black
hi SpecialKey   cterm=none  ctermbg=none   ctermfg=Cyan
hi StatusLine   cterm=bold  ctermbg=none   ctermfg=DarkCyan
hi StatusLineNC cterm=none  ctermbg=none   ctermfg=Cyan
hi Title        cterm=none  ctermbg=none   ctermfg=Cyan
hi Tooltip      cterm=none  ctermbg=none   ctermfg=Brown
hi VertSplit    cterm=none  ctermbg=none   ctermfg=Cyan
hi Visual       cterm=bold  ctermbg=White  ctermfg=Black
hi VisualNOS    cterm=bold  ctermbg=White  ctermfg=Black
hi WarningMsg   cterm=bold  ctermbg=none   ctermfg=Red
hi WildMenu     cterm=none  ctermbg=none   ctermfg=Cyan

" syntax highlighting groups
:if $USER == "root" || $USER == 0
  hi Comment      cterm=none  ctermbg=none   ctermfg=Red
:else
  hi Comment      cterm=none  ctermbg=none   ctermfg=Green
:endif
hi Constant     cterm=none  ctermbg=none   ctermfg=Green
hi Error        cterm=bold  ctermbg=none   ctermfg=Red
hi Identifier   cterm=bold  ctermbg=none   ctermfg=LightCyan
hi Ignore       cterm=none  ctermbg=none   ctermfg=DarkGray
hi PreProc      cterm=bold  ctermbg=none   ctermfg=Magenta
hi Special      cterm=bold  ctermbg=none   ctermfg=Green
hi Statement    cterm=bold  ctermbg=none   ctermfg=Magenta
hi Todo         cterm=bold  ctermbg=none   ctermfg=Red
hi Type         cterm=bold  ctermbg=none   ctermfg=Magenta
hi Underlined   cterm=none  ctermbg=none   ctermfg=Blue

" make even rare occurrences readable
hi SpellBad    cterm=reverse ctermbg=Black  ctermfg=Red
hi SpellCap    cterm=reverse ctermbg=Yellow  ctermfg=Red
hi SpellLocal  cterm=reverse ctermbg=Cyan  ctermfg=Red
hi PmenuThumb  cterm=none   ctermbg=none   ctermfg=White
hi CursorLineNr  cterm=bold ctermfg=red
hi Italic      cterm=italic ctermbg=none   ctermfg=Blue
hi Bold        cterm=bold ctermbg=none   ctermfg=Red
hi goPackageComment      cterm=none  ctermbg=none   ctermfg=LightBlue
hi asciidocQuotedEmphasized2 cterm=italic ctermbg=none   ctermfg=Blue
hi asciidocQuotedUnconstrainedEmphasized cterm=italic ctermbg=none   ctermfg=Blue
hi asciidocQuotedBold cterm=bold ctermbg=none   ctermfg=Red
hi asciidocQuotedUnconstrainedBold cterm=bold ctermbg=none   ctermfg=Red
hi link Bold markdownBold
