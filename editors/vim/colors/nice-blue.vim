"--------------------------------------------------------------------
" Filename:     nice-blue.vim
" Purpose:      Vim color file
" Based On:     color highlighting found in zellner.vim
" Modified By:  Mark Fernandes
" Last Change:  2025 Feb 14
"--------------------------------------------------------------------

" First, read help on the following topics
"   :help group-name
"   :help highlight-groups
"   :help cterm-colors
"
" Then to see existing setup, run
"   :highlight
"

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "nice-blue"

" Possible color names: Brown, Blue, Purple,
" Alternate nice colors (gray: #888999) (purple:AA0BBB)

" Mode and Document settings
hi Normal     gui=none    term=bold                                     guifg=white guibg=#180248
hi Visual     gui=none    term=reverse    ctermfg=Yellow  ctermbg=Red   guifg=Black guibg=Yellow
hi Search     gui=none    term=reverse    ctermfg=Black   ctermbg=Cyan  guifg=Black guibg=White
hi StatusLine gui=bold    term=reverse    ctermfg=Gray    ctermbg=Black guifg=DarkBlue guibg=DarkGray
hi Title      gui=bold    term=underline  ctermfg=Blue                  guifg=Yellow


" Colour for crucial elements
hi Comment    gui=italic      term=bold       ctermfg=Red                     guifg=DeepSkyBlue2
hi Constant   gui=italic,bold term=underline  ctermfg=DarkGray                guifg=DarkCyan  "#888999             " gray
hi Error                      term=reverse    ctermfg=15 ctermbg=9            guifg=White     guibg=Red
hi Identifier gui=none        term=underline  ctermfg=Blue                    guifg=LightGreen
hi PreProc    gui=bold        term=underline  ctermfg=Magenta                 guifg=#FF3311             " reddish
hi Special    gui=bold        term=bold       ctermfg=Yellow                  guifg=OrangeRed
hi Statement  gui=bold        term=bold       ctermfg=DarkRed                 guifg=Cyan
hi Tag                        term=bold       ctermfg=DarkGreen               guifg=DarkGreen
hi Todo                       term=standout   ctermbg=Yellow ctermfg=Black    guifg=Blue      guibg=Yellow
hi Type       gui=none        term=underline  ctermfg=Blue                    guifg=LightCyan

hi! link MoreMsg        Comment
hi! link Question       Comment
hi  link Boolean        Constant
hi  link Character      Constant
hi  link Number         Constant
hi  link String         Constant
hi! link WarningMsg     ErrorMsg
hi  link Function       Identifier
hi  link Float          Number
hi  link Define         PreProc
hi  link Include        PreProc
hi  link Macro          PreProc
hi  link PreCondit      PreProc
hi  link Debug          Special
hi  link Delimiter      Special
hi  link Label          Special
hi  link SpecialChar    Special
hi  link SpecialComment Special
hi  link Exception      Statement
hi  link Keyword        Statement
hi  link Operator       Statement
hi  link Conditional    Type
hi  link Repeat         Type
hi  link StorageClass   Type
hi  link Structure      Type
hi  link Typedef        Type
hi! link ErrorMsg       Visual

" local syntax file - set colors on a per-machine basis:
" vim: tw=0 ts=4 sw=4
