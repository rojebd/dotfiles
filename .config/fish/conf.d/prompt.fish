# My fish prompt also with the fish git prompt customized

set __fish_git_prompt_showdirtystate 1
set __fish_git_prompt_showuntrackedfiles 1
set __fish_git_prompt_showstashstate 1
set __fish_git_prompt_char_cleanstate ' clean'
set __fish_git_prompt_char_dirtystate ' unstaged'
set __fish_git_prompt_char_invalidstate ' unmerged'
set __fish_git_prompt_char_stagedstate ' staged'
set __fish_git_prompt_char_stashstate ' stashed'
set __fish_git_prompt_char_untrackedfiles ' untracked'
set __fish_git_prompt_char_upstream_ahead ' ahead'
set __fish_git_prompt_char_upstream_behind ' behind'
set __fish_git_prompt_char_upstream_diverged ' diverged'
set __fish_git_prompt_char_upstream_equal ' up-to-date'

function fish_prompt
    set user (set_color yellow)"$USER"
    set host_name (set_color cyan)"$hostname"
    set dir (set_color blue)(prompt_pwd)
    set git_prompt (set_color magenta)"git:"(fish_git_prompt | string trim)
    echo \n" [$user"(set_color red)"@""$host_name $dir $git_prompt"(set_color normal)"]"\n' >> '
end
