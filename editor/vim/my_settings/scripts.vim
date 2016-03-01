""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Here begins my automated wordcount addition.
" This combines several ideas from:
" http://stackoverflow.com/questions/114431/fast-word-count-function-in-vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:word_count="<unknown>"
function! WordCount()
  return g:word_count
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

