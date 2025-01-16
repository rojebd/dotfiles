set -gx GPG_TTY (tty)
set fish_greeting
set -U autovenv_enable yes
set -U autovenv_announce yes
set -x HAREPATH /usr/src/hare/stdlib:/usr/src/hare/third-party:/home/roniell/sources/hare-raylib
set -x EDITOR nvim

set -x C_INCLUDE_PATH /home/roniell/coding/c/llist/include $C_INCLUDE_PATH
set -x LIBRARY_PATH /home/roniell/coding/c/llist/lib $LIBRARY_PATH
set -x LD_LIBRARY_PATH /home/roniell/coding/c/llist/lib $LD_LIBRARY_PATH

#alias grep="rg $argv"
#alias find="fd $argv"
#alias sed="sd $argv"

fish_add_path /home/roniell/.local/bin
