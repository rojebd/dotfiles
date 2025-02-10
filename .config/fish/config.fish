set -gx GPG_TTY (tty)
set fish_greeting
set -U autovenv_enable yes
set -U autovenv_announce yes

#set -x HAREPATH /usr/src/hare/stdlib:/usr/src/hare/third-party:/home/roniell/sources/hare-raylib

set -x EDITOR nvim
set -l CODE "/home/roniell/coding/c"
set -Ux C_INCLUDE_PATH  "$CODE/llist/include:$CODE/hashtable/include:$CODE/array_int/include"
set -Ux LIBRARY_PATH    "$CODE/llist/lib:$CODE/hashtable/lib:$CODE/array_int/lib"
set -Ux LD_LIBRARY_PATH "$CODE/llist/lib:$CODE/hashtable/lib:$CODE/array_int/lib"

#alias grep="rg $argv"
#alias find="fd $argv"
#alias sed="sd $argv"

fish_add_path /home/roniell/.local/bin
