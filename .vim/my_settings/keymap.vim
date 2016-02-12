"** Keyboard Mappings
"   the pipes are necessary in order to have comments on the same line

:map  \fo <Esc>:call My_Convert()<CR> | " removes dos formatting
:map  \qw <Esc>magw}``      | " Vim's paragraph formatting
:imap \qw <Esc>magw}``      | " Vim's paragraph formatting
:map  \q  <Esc>magq}``      | " par command  paragraph formatting
:imap \q  <Esc>magq}``      | " par command paragraph formatting
:map  \l  <Esc>:set list!<CR>   | "toggle non-printable
:imap \l  <Esc>:set list!<CR>a  | "toggle non-printable
:map  \nu  <Esc>:set nu!<CR>    | " toggle line-numbers
:imap \nu  <Esc>:set nu!<CR>a   | " toggle line-numbers


" vim: nonu: nowrap: nospell
