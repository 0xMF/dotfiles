"--------------------------------------------------------------------
" Filename:     nice.vim
" Purpose:      Vim color file
" Based On:     color highlighting found in zellner.vim
" Modified By:  Mark Fernandes
" Last Change:  2023 Apr 08
"--------------------------------------------------------------------

" First, read help on the following topics
"   :help group-name
"   :help highlight-groups
"   :help cterm-colors
"
" Then to see existing setup, run
"   :highlight
"

set background=light
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "nice"

" Possible color names: Brown, Blue, Purple,
" Alternate nice colors (gray: #888999) (purple:AA0BBB)

" Mode and Document settings
hi Normal     gui=none    term=bold                                     guifg=Black     guibg=Beige
hi Visual     gui=none    term=reverse    ctermfg=Yellow  ctermbg=Red   guifg=Black     guibg=Yellow
hi Search     gui=none    term=reverse    ctermfg=Black   ctermbg=Cyan  guifg=Black     guibg=Cyan
hi StatusLine gui=bold    term=reverse    ctermfg=Gray    ctermbg=Black guifg=DarkBlue  guibg=DarkGray
hi Title      gui=bold    term=underline  ctermfg=Blue                  guifg=Black

" syntax highlighting groups
" hi Constant     cterm=none        ctermbg=none   ctermfg=Yellow
" hi Error        cterm=reverse     ctermbg=9      ctermfg=15
" hi Identifier   cterm=bold  ctermbg=none   ctermfg=LightCyan
" hi PreProc      cterm=bold  ctermbg=none   ctermfg=Magenta
" hi Special      cterm=bold  ctermbg=none   ctermfg=DarkYellow
" hi Statement    cterm=bold  ctermbg=none   ctermfg=Magenta
" hi Todo         cterm=bold  ctermbg=none   ctermfg=Red
" hi Type         cterm=bold  ctermbg=none   ctermfg=Magenta
hi Comment      gui=italic      guifg=DarkGreen                 cterm=none        ctermbg=none      ctermfg=Green
hi Constant     gui=italic,bold guifg=#888999                   cterm=none                          ctermfg=Cyan
hi Error                        guifg=White     guibg=Red       cterm=bold        ctermbg=White     ctermfg=Red
hi Identifier   gui=bold        guifg=Red                       cterm=bold                          ctermfg=Magenta
hi Ignore                                                       cterm=none        ctermbg=none      ctermfg=DarkGray
hi PreProc      gui=bold        guifg=#FF3311                   cterm=bold                          ctermfg=Magenta   " guifg=Reddish
hi Special      gui=bold        guifg=OrangeRed                 cterm=bold                          ctermfg=Yellow
hi Statement    gui=bold        guifg=Brown                     cterm=bold                          ctermfg=Magenta
hi Todo         gui=bold        guifg=Red       guibg=Yellow    cterm=standout    ctermbg=Yellow    ctermfg=Black
hi Type         gui=bold        guifg=Blue                      cterm=bold                          ctermfg=Magenta
hi Underlined                                                   cterm=none        ctermbg=none      ctermfg=DarkRed

" Colour for crucial elements
hi Cursor                                                       cterm=none        ctermbg=none      ctermfg=Red
hi CursorIM                                                     cterm=none        ctermbg=none      ctermfg=Red
hi DiffAdd                                                      cterm=none        ctermbg=none      ctermfg=Blue
hi DiffChange                                                   cterm=none        ctermbg=none      ctermfg=Blue
hi DiffDelete                                                   cterm=none        ctermbg=none      ctermfg=Red
hi DiffText                                                     cterm=none        ctermbg=none      ctermfg=DarkRed
hi Directory                                                    cterm=none        ctermbg=none      ctermfg=Cyan
hi ErrorMsg                                                     cterm=none        ctermbg=none      ctermfg=Red
hi FoldColumn                                                   cterm=none        ctermbg=none      ctermfg=Cyan
hi Folded                                                       cterm=none        ctermbg=none      ctermfg=DarkRed
hi IncSearch                                                    cterm=none        ctermbg=none      ctermfg=Cyan
hi LineNr                                                       cterm=none        ctermbg=none      ctermfg=DarkCyan
hi Menu                                                         cterm=none        ctermbg=none      ctermfg=DarkBlue
hi ModeMsg                                                      cterm=none        ctermbg=none      ctermfg=DarkRed
hi MoreMsg                                                      cterm=none        ctermbg=none      ctermfg=Cyan
hi NonText                                                      cterm=none        ctermbg=none      ctermfg=DarkRed
hi Question                                                     cterm=none        ctermbg=none      ctermfg=Cyan
hi Scrollbar                                                    cterm=none        ctermbg=none      ctermfg=Cyan
hi Search                                                       cterm=bold        ctermbg=White     ctermfg=Black
hi SpecialKey                                                   cterm=none        ctermbg=none      ctermfg=Cyan
hi StatusLine                                                   cterm=bold        ctermbg=none      ctermfg=DarkCyan
hi StatusLineNC                                                 cterm=none        ctermbg=none      ctermfg=Cyan
hi Tag                          guifg=DarkGreen                 cterm=bold                          ctermfg=DarkGreen
hi Title                                                        cterm=none        ctermbg=none      ctermfg=Cyan
hi Tooltip                                                      cterm=none        ctermbg=none      ctermfg=Brown
hi VertSplit                                                    cterm=none        ctermbg=none      ctermfg=Cyan
hi Visual                                                       cterm=bold        ctermbg=White     ctermfg=Black
hi VisualNOS                                                    cterm=bold        ctermbg=White     ctermfg=Black
hi WarningMsg                                                   cterm=bold        ctermbg=none      ctermfg=Red
hi WildMenu                                                     cterm=none        ctermbg=none      ctermfg=Cyan

hi! link ErrorMsg       Visual
hi! link MoreMsg        Comment
hi! link Question       Comment
hi! link WarningMsg     ErrorMsg

hi  link Conditional    Statement
hi  link Exception      Statement
hi  link Keyword        Statement
hi  link Operator       Statement
hi  link Repeat         Statement

hi  link Function       Type
hi  link StorageClass   Type
hi  link Structure      Type
hi  link Typedef        Type

hi  link Debug          Special
hi  link Delimiter      Special
hi  link Label          Special
hi  link SpecialChar    Special
hi  link SpecialComment Special

hi  link Define         PreProc
hi  link Include        PreProc
hi  link Macro          PreProc
hi  link PreCondit      PreProc

hi  link Boolean        Constant
hi  link Character      Constant
hi  link Float          Number
hi  link Number         Constant
hi  link String         Constant

hi goPackageComment      cterm=none  ctermbg=none   ctermfg=LightBlue
hi asciidocQuotedEmphasized2 cterm=italic ctermbg=none   ctermfg=Blue
hi asciidocQuotedUnconstrainedEmphasized cterm=italic ctermbg=none   ctermfg=Blue
hi asciidocQuotedBold cterm=bold ctermbg=none   ctermfg=Red
hi asciidocQuotedUnconstrainedBold cterm=bold ctermbg=none   ctermfg=Red

hi  DiffText  guibg=Yellow

" local syntax file - set colors on a per-machine basis:
" vim: tw=0 ts=4 sw=4
