set -gx EDITOR kak
set -Ux KAKOUNE_POSIX_SHELL /bin/sh

fish_add_path /home/roniell/.local/bin

#function fish_user_key_bindings
#  fish_vi_key_bindings
#end

#set fish_cursor_insert line

if status is-interactive
    # Commands to run in interactive sessions can go here
    function fstclone 
		git clone --depth=1 $argv
    end

    function gclone
		git clone $argv
	end
end

alias psuspend="doas zzz"
alias post-suspend="doas service keyd onerestart; nohup mako & rm nohup.out"
alias find-project="source ~/.local/bin/find-project-fish.fish"

# My fish git prompt format is
# [user@hostname directory (git-branch git-statuses)]
# In the future default the cursor color to green unless a comman failed
function fish_prompt
    set directory (set_color brblue) (prompt_pwd)
	
    set -g __fish_git_prompt_showuntrackedfiles yes
	set -g __fish_git_prompt_showdirtystate yes
	set -g __fish_git_prompt_showupstream auto
	set -g __fish_git_prompt_showstashstate yes
	# set -g __fish_git_prompt_showcolorhints yes
	
	set -g __fish_git_prompt_char_stashstate ' stashed'
	set -g __fish_git_prompt_char_untrackedfiles ' untracked'
	set -g __fish_git_prompt_char_upstream_ahead ' ahead'
	set -g __fish_git_prompt_char_upstream_behind ' behind'
	set -g __fish_git_prompt_char_upstream_diverged ' diverged'
	set -g __fish_git_prompt_char_upstream_equal ' up-to-date'
	set -g __fish_git_prompt_char_cleanstate ' clean'
	set -g __fish_git_prompt_char_dirtystate ' unstaged'
	set -g __fish_git_prompt_char_invalidstate ' unmerged'
	set -g __fish_git_prompt_char_stagedstate ' staged'
	set -g __fish_git_prompt_char_stashstate ' stashed'

	set prompt_shell (set_color ebdbb2) '>> '
	set user_and_hostname (set_color bryellow)"$USER"(set_color brred)"@"(set_color brcyan)"$HOST"
	set git_prompt (set_color brmagenta) "on git:"(fish_git_prompt | grep -e '' | sed 's/ //')
	echo \n" [$user_and_hostname""$directory""$git_prompt"(set_color normal)"]"\n"$prompt_shell"
end

set -Ux GNUPGHOME "$XDG_DATA_HOME/gnupg"
set fish_greeting
