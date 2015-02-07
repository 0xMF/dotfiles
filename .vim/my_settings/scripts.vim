" removes spurious ^M ^H ^[ saves and cursor placed at top of file
:function! My_Convert()
:%s///g
:%s///g
:%s///g
:w
:1
:endfunction

