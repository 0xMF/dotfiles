"** Keyboard Mappings
"   the pipes are necessary in order to have comments on the same line

:map  \fo <Esc>:call My_Convert()<CR> | " removes dos formatting
:map  \qw <Esc>magw}``      | " Vim's paragraph formatting
:imap \qw <Esc>magw}``      | " Vim's paragraph formatting
:map  \q  <Esc>magq}``      | " par command  paragraph formatting
:imap \q  <Esc>magq}``      | " par command paragraph formatting
:map \n :set nu!<CR>|  " toggle numbering
:imap \n <Esc>:set nu!<CR>a|  " toggle numbering
:map  \l  <Esc>:set list!<CR>   | "toggle non-printable
:imap \l  <Esc>:set list!<CR>a  | "toggle non-printable
:map  \nu  <Esc>:set nu!<CR>    | " toggle line-numbers
:imap \nu  <Esc>:set nu!<CR>a   | " toggle line-numbers
:imap <M-q> <Esc>magq}``a|" --ditto-- in insert mode
:map <C-s> :w<CR>|          " like Windows CTRL +S to save
:imap <C-s> <Esc>:w<CR>a|  " --ditto-- in insert mode
:map <A-BS> <ESC>u|         " Window's style undo
:imap <A-BS> <ESC>ua|      " --ditto-- in insert mode
:imap <C-Del> <Esc>ldwi|  " Window's style delete
:map <C-D> de|              " delete word under cursor
:imap <C-D> <Esc>ldea|  " --ditto-- in insert mode
:map <End>  <Esc>$|      " jump to the end
:imap <End> <Esc>$a|    " --ditto-- in insert mode


" vim: nonu: nowrap: nospell
