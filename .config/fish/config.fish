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

# dotnet autocomplete
complete -f -c dotnet -a "(dotnet complete (commandline -cp))"

# Disable dotnet cli telemetry
set -Ux DOTNET_CLI_TELEMETRY_OPTOUT 1

fish_add_path /home/roniell/.local/bin
fish_add_path /home/roniell/.dotnet/tools
