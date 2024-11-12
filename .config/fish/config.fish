set -gx GPG_TTY (tty)
set fish_greeting
set -U autovenv_enable yes
set -U autovenv_announce yes
set -Ux HAREPATH /usr/src/hare/stdlib:/usr/src/hare/third-party:/home/roniell/sources/hare-raylib
set -Ux LSP_USE_PLISTS true
fish_add_path /home/roniell/.local/bin
