"** Keyboard Mappings
"   the pipes are necessary in order to have comments on the same line
" vim: nonu: nowrap: nospell: ff=unix: ft=vim:
" Function key mapping
" F1:
" F2: toggle taglist window
" F3: save current editing session to ~/work/session.vim

"<Ctrl-A> -- select all
map <C-A>   m'ggVG|         "set mark and select all
imap <C-A>  <Esc>m'ggVG|    "select all in insert mode
vmap <C-A>  ggVG

"<Ctrl-U> -- cut (goto visual mode and cut) (previously used Ctrl-X)
"map  <C-u>  vgG
"imap <C-u>  vgG
"vmap <C-u>  "+x<Esc>a

"<Ctrl-P> -- coPy (goto visual mode and copy)
" have to use because X11 internally maps <C-C> to stopping a search
vmap <C-p>   "+ygv"zy`>
autocmd FocusGained * let @z=@+

"<Ctrl-V> -- paste in insert mode
:if has ("gui_gtk")
: map <C-V>   "+gp
: imap <C-V>  <Esc>l"+gPa
: vmap <C-V>  <Esc>l"+gPa
:else
: map <C-V>   "+gp
: imap <C-V>  <Esc>l"+gPa
: vmap <C-V>  <Esc>l"+gPa
:endif

"cscope settings
map g<C-]> :cs find 3 <C-R>=expand("<cword>")<CR><CR>
map g<C-\> :cs find 0 <C-R>=expand("<cword>")<CR><CR>

" Using 'CTRL-spacebar' then a search type makes the vim window
" split horizontally, with search result displayed in
" the new window.

" half a page down
:map <C-Space> <C-D>

" jump/scroll up/down in other window when in split window mode
:map <C-L> <C-W>p
:imap <C-L> <Esc><C-W>p
:map <C-J> <C-W>p<C-D><C-W>p
:imap <C-J> <Esc><C-W>p<C-D><C-W>pa
:map <C-K> <C-W>p<C-U><C-W>p
:imap <C-K> <Esc><C-W>p<C-U><C-W>pa
map <C-Down> <C-J>
map <C-Up> <C-K>

"nmap <C-Space>s :scs find s <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space>g :scs find g <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space>c :scs find c <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space>t :scs find t <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space>e :scs find e <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space>f :scs find f <C-R>=expand("<cfile>")<CR><CR>
"nmap <C-Space>i :scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
"nmap <C-Space>d :scs find d <C-R>=expand("<cword>")<CR><CR>

" Hitting CTRL-space *twice* before the search type does a vertical
" split instead of a horizontal one

"nmap <C-Space><C-Space>s
"    \:vert scs find s <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space><C-Space>g
"    \:vert scs find g <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space><C-Space>c
"    \:vert scs find c <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space><C-Space>t
"    \:vert scs find t <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space><C-Space>e
"    \:vert scs find e <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space><C-Space>i
"    \:vert scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
"nmap <C-Space><C-Space>d
"    \:vert scs find d <C-R>=expand("<cword>")<CR><CR>

"  mappings for building programs
"map <F2> :Tlist<CR>|      " taglist window toggle
"map <F5> :make<CR>|       " build
"map <F5> :call My_Build()<CR>|       " build
"imap <F5> <Esc>:call My_Build()<CR>|       " build
"map <C-Up> :cprevious<CR>
"map <C-Down> :cnext<CR>
"map <C-F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>
map <F3>  :execute "mksession! " . v:this_session<CR>

" mappings for less like behaviour (works with behave msvim)
" page down
map <Space>   <PageDown>
map f         <PageDown>
" page up
map <S-Space> <PageUp>
map <C-Space> <C-b>
map b         <C-b>
"map q       :q<CR>

" Solves the C-Space problem when using terminal vim insert/normal mode
" Credit: https://shallowsky.com/blog/linux/editors/vim-ctrl-space.html
map <Nul>    <PageUp>
imap <Nul>    <Space>
"#-

:imap <C-w> <Esc><C-W>|                 " setup toggle to other windows with hjkl and UpDnLtRt

:imap <A-BS> <ESC>ua|                   " --ditto-- in insert mode
":imap <C-D> <Esc>ldea|                  " --ditto-- in insert mode
:imap <C-Del> <Esc>ldwi|                " Window's style delete
:imap <C-s> <Esc>:w<CR>a|               " --ditto-- in insert mode
:imap <End> <Esc>$a|                    " --ditto-- in insert mode
:imap <M-q> <Esc>magq}``a|              " --ditto-- in insert mode
:imap ;a <<Esc>ea><Esc>a|               " --ditto-- in insert mode
:imap ;l  <Esc>:set list!<CR>a|         " toggle non-printable
:imap ;n <Esc>:set nu!<CR>a|            " toggle numbering
:imap ;nu  <Esc>:set nu!<CR>a|          " toggle line-numbers
:imap ;q  <Esc>magq}``|                 " par command paragraph formatting
:imap ;qw <Esc>magw}``|                 " Vim's paragraph formatting
:imap ;t <Esc>:set laststatus=0<CR>a|    " off statusbar
:imap ;tm <Esc>:set statusline=%f%h%w\ [%Y]\ [%l,%v]\ %L:%p%%\ %{strftime('%c')}<CR><CR>a| "minimal statusbar
:imap ;ts <Esc>:set statusline=%f%m%r%h%w\ [%{&ff}]\ [%Y]\ [ASCII=\%3.3b]\ [HEX=\%02.2B]\ [%l,%v][%p%%]\ [TOTAL=%L]\ TIME:\ %{strftime('%c')}<CR><CR>a| "normal statusbar
:imap ;tt <Esc>:set laststatus=2<CR>a|    " on statusbar

:imap ;e <Esc>:set spell!<CR>i|  "toggle spell checking

:map ;' i'<Esc>Ea'<Esc>|              " ' ' around a word
:map ;* i*<Esc>Ea*<Esc>|              " * * around a word
:map ;+ i+<Esc>Ea+<Esc>|              " + + around a word
":map ;a i<<Esc>Ea><Esc>|              " angles <> around a word
:map ;a  <Esc>magq}``|                " par command  paragraph formatting
:map ;b i*<Esc>Ea*<Esc>|              " * * around a word
:map ;c i{<Esc>Ea}<Esc>|              " curlies {} around a word
:map ;cc <Esc>:call My_Colours_Change()<CR>|
:map ;cf <Esc>:call My_Fonts_Change()<CR>|
:map ;d :call DeleteTrailingSpaces()<CR>|
:map ;e :set spell!<CR>|                " toggle spell checking
:map ;ea i</<Esc>ea><Esc>|              " add end tags <> around a word
:map ;fo <Esc>:call My_Convert()<CR>|   " removes dos formatting
:map ;h :set hls!<CR>|                  " toggle hilightsearch
:map ;i i'<Esc>Ea'<Esc>|                " ' ' around a word
:map ;l <Esc>:set list!<CR>|            " toggle non-printable
:map ;m i+<Esc>Ea+<Esc>|                " + + around a word
:map ;n <Esc>:set nu!<CR>|              " toggle line-numbers
:map ;nu <Esc>:set nu!<CR>|             " toggle line-numbers
:map ;p mpi(<Esc>Ea)<Esc>`p|            " parens () around a word
:map ;q  <Esc>magq}``|                  " par command  paragraph formatting
:map ;qw <Esc>magw}``|                  " Vim's paragraph formatting
:map ;t <Esc>:set laststatus=0<CR>|     " off statusbar
:map ;tm <Esc>:set statusline=%f%h%w\ [%Y]\ [%l,%v]\ %L:%p%%\ %{strftime('%c')}<CR><CR>| "minimal statusbar
:map ;ts <Esc>:set statusline=%f%m%r%h%w\ [%{&ff}]\ [%Y]\ [ASCII=\%3.3b]\ [HEX=\%02.2B]\ [%l,%v][%p%%]\ [TOTAL=%L]\ TIME:\ %{strftime('%c')}<CR><CR>| "normal statusbar
:map ;tt <Esc>:set laststatus=2<CR>|    " on statusbar
:map ;tp gT|                            " tabprevious for console vim
:map ;tn gt|                            " tabnext
:map <A-BS> <ESC>u|                     " Window's style undo
":map <C-D> de|                          " delete word under cursor
:map <C-s> :w<CR>|                      " like Windows CTRL +S to save
:map <End>  <Esc>$|                     " jump to the end
:map <C-p> gT|                          " tabprevious for console vim
:map <C-n> gt|                          " tabnext

" Word counting
:map <F11> :set report=0<CR>:set nohls<CR>:'t,.s/\i\+/&/g<CR>| " from mark t to current line wordcount
:map <F12> :set report=0<CR>:set nohls<CR>:%s/\i\+/&/g<CR>| " entire file wordcount

" used for completion
:imap <C-Tab> <C-P>

" FAILED COPY ATTEMPTS
"map <C-C>  vgG
"imap <C-C>  <Esc>vgG
"vmap <C-C>   "+ygv"zy`>
"vmap <C-C>   "+v_y<Esc>
"vmap <C-C>   "+x<Esc><CR>l"+gPa<Esc>``
"... jump to previous mark according to platform
":if has ("gui_gtk")
":   vmap <C-C> "+y<Esc>``
":else
":   vmap <C-C> "+y<Esc>``
":endif
" See: http://www.vim.org/tips/tip.php?tip_id=984
"copy
"vmap <F7>   "+ygv"zy`>
""paste (Shift-F7 to paste after normal cursor, Ctrl-F7 to paste over visual selection)
"nmap <F7>   "zgP
"nmap <S-F7> "zgp
"imap <F7>   <C-r><C-o>z
"vmap <C-F7> "zp`]
"cmap <F7>   <C-r><C-o>z
""copy register
"autocmd FocusGained * let @z=@+
"
" Open file under cursor in a new tab
" :help gf
"nnoremap gf :tabe <cfile><CR>
"map <C-F> gf
