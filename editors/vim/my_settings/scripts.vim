""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Here begins my automated wordcount addition.
" This combines several ideas from:
" http://stackoverflow.com/questions/114431/fast-word-count-function-in-vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:word_count="<unknown>"
function! WordCount()
  return g:word_count
endfunction

function! DeleteTrailingSpaces()
  let l:savecursor = winsaveview()
  :silent :%g/ *$/:s/ *$//
  call winrestview(l:savecursor)
endfunction

function! OBS()
  : set guifont=Source\ Code\ Pro\ Semibold\ 20
  : set statusline=%f%m%r%h%w\ [%{&ff}]\ [%Y]\ [ASCII=\%3.3b]\ [HEX=\%02.2B]\ [%l,%v]\ [%{CharCount()}\ CHARS][%{WordCount()}\ WORDS][%p%%:%L\ LINES]}
endfunction

function! UpdateWordCount()
  let lnum = 1
  let n = 0
  while lnum <= line('$')
    let n = n + len(split(getline(lnum)))
    let lnum = lnum + 1
  endwhile
  let g:word_count = n
endfunction

set updatetime=1000
augroup WordCounter
  au! CursorHold,CursorHoldI * call UpdateWordCount()
augroup END

let g:char_count="<unknown>"
function! CharCount()
  return g:char_count
endfunction

function! UpdateCharCount()
  let lnum = 1
  let n = 0
  while lnum <= line('$')
    "let cc= cc + strlen(substitute(getline('.'), '\W', '', 'g'))
    let n = n + strchars(getline(lnum))
    let lnum = lnum + 1
  endwhile
  let g:char_count=n
endfunction

" Update the count when cursor is idle in command or insert mode.
" Update when idle for 1000 msec (default is 4000 msec).
set updatetime=1000
augroup CharCounter
  au! CursorHold,CursorHoldI * call UpdateCharCount()
augroup END

" removes spurious ^M ^H ^[ saves and cursor placed at top of file
:function! My_Convert()
:%s///eg
:%s///eg
:%s///eg
:w
:1
:endfunction

:function! My_Colours_Change()
:   if g:colors_name == "nice-gui"
:     colorscheme darkblue
:   else
:     colorscheme nice-gui
:   endif
:endfunction

:function! My_Fonts_Change()
    :if exists("g:font_small")
    :   unlet g:font_small
    :   if has ("gui_win32")
    :     set guifont=Source_Code_Pro_Medium:h10:cANSI
    :   elseif has ("gui_gtk")
    :     silent let scp_detected=systemlist("fc-list|grep 'Source Code Pro Medium'|wc -l")[0]
    :     if scp_detected == "2"
    :       set guifont=Source\ Code\ Pro\ Medium\ 12
    :     else
    :       set guifont=Monospace\ 12
    :     endif
    :   endif
    :else
    :   let g:font_small = 1
    :   if has ("gui_win32")
    :     set guifont=Source_Code_Pro_Medium:h8:cANSI
    :   elseif has ("gui_gtk")
    :     silent let scp_detected=systemlist("fc-list|grep 'Source Code Pro Medium'|wc -l")[0]
    :     if scp_detected == "2"
    :       set guifont=Source\ Code\ Pro\ Medium\ 10
    :     else
    :       set guifont=Monospace\ 10
    :     endif
    :   endif
    :endif
:endfunction
