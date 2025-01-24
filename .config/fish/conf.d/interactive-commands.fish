if status is-interactive
    # Commands to run in interactive sessions can go here
    
    function git-url
        git remote set-url origin $(git config --get remote.origin.url | sed 's/https:\/\/github.com\//git@github.com:/g' | awk '{print $1".git"}')
    end
    
    function neovide
        /usr/bin/neovide $argv & disown; sleep 0.1; exit
    end

    function venv
        set dir (pwd)
        python -m venv $argv
        cd ..
        cd $dir
    end

    function glg
	      git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative $argv
    end

    # function emacs
    #     setsid emacs $argv &
    # end
    
    # function emacs-dir
    #     cd $argv[1]; and setsid emacs $argv[2]; and exit &
    # end
    
    function gt
        git log --graph --decorate --pretty=oneline --abbrev-commit --all $argv
    end

    function gb
	git branch $argv
    end

    function gc
        git commit $argv
    end

    function ga
        git add $argv
    end

    function ga.
        git add . $argv
    end

    function gf
        git fetch $argv
    end

    function gp
        git push $argv
    end

    function gpu
        git pull $argv
    end

    function gs
        git status $argv
    end

    function gsh
        git show $argv
    end

    function gl
        git log --all --graph --format=oneline $argv
    end

    function gck
        git checkout $argv
    end

    function gr
        git rebase $argv
    end

    function gd
        git diff $argv
    end

    function fstclone
        git clone --depth=1 $argv
    end

end
