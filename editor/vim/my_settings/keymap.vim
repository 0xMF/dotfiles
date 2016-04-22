"** Keyboard Mappings
"   the pipes are necessary in order to have comments on the same line
" vim: nonu: nowrap: nospell

:imap <A-BS> <ESC>ua|                   " --ditto-- in insert mode
:imap <C-D> <Esc>ldea|                  " --ditto-- in insert mode
:imap <C-Del> <Esc>ldwi|                " Window's style delete
:imap <C-s> <Esc>:w<CR>a|               " --ditto-- in insert mode
:imap <End> <Esc>$a|                    " --ditto-- in insert mode
:imap <M-q> <Esc>magq}``a|              " --ditto-- in insert mode
:imap ;a <<Esc>ea><Esc>a|	              " --ditto-- in insert mode
:imap ;l  <Esc>:set list!<CR>a|         " toggle non-printable
:imap ;n <Esc>:set nu!<CR>a|            " toggle numbering
:imap ;nu  <Esc>:set nu!<CR>a|          " toggle line-numbers
:imap ;q  <Esc>magq}``|                 " par command paragraph formatting
:imap ;qw <Esc>magw}``|                 " Vim's paragraph formatting

:map ;fo <Esc>:call My_Convert()<CR>|   " removes dos formatting
:map ;l <Esc>:set list!<CR>|            " toggle non-printable
:map ;n <Esc>:set nu!<CR>|              " toggle line-numbers
:map ;nu <Esc>:set nu!<CR>|             " toggle line-numbers
:map ;q  <Esc>magq}``|                  " par command  paragraph formatting
:map ;qw <Esc>magw}``|                  " Vim's paragraph formatting
:map <A-BS> <ESC>u|                     " Window's style undo
:map <C-D> de|                          " delete word under cursor
:map <C-s> :w<CR>|                      " like Windows CTRL +S to save
:map <End>  <Esc>$|                     " jump to the end
:map ;' i'<Esc>Ea'<Esc>|	              " ' ' around a word
:map ;* i*<Esc>Ea*<Esc>|	              " * * around a word
:map ;+ i+<Esc>Ea+<Esc>|	              " + + around a word
:map ;a i<<Esc>Ea><Esc>|	              " angles <> around a word
:map ;b i*<Esc>Ea*<Esc>|	              " * * around a word
:map ;c i{<Esc>Ea}<Esc>|	              " curlies {} around a word
:map ;ea i</<Esc>ea><Esc>|	            " add end tags <> around a word
:map ;i i'<Esc>Ea'<Esc>|	              " ' ' around a word
:map ;m i+<Esc>Ea+<Esc>|	              " + + around a word
:map ;p i(<Esc>Ea)<Esc>|	              " parens () around a word
