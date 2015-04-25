# .kshrc 
#-----------------------------------------------------------------

#
REPO=~/.ksh

#
print .kshrc called ...

[ -f $HOME/.profile ] && . ~/.profile
PATH=$PATH:/usr/local/bin:.:~/bin

# source common aliases used by power users
if [ -f $REPO/aliases.ksh ]; then
  source $REPO/aliases.ksh
fi


# vim:nospell:ft=sh:
