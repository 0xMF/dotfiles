" removes spurious ^M ^H ^[ saves and cursor placed at top of file
:function! My_Convert()
:%s///eg
:%s///eg
:%s///eg
:w
:1
:endfunction

