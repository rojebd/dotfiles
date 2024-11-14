if status is-interactive
    # Commands to run in interactive sessions can go here

    function venv
        set dir (pwd)
        python -m venv $argv
        cd ..
        cd $dir
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
