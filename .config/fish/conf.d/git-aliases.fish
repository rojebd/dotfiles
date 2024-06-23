if status is-interactive
    # Commands to run in interactive sessions can go here
    function gc
	git commit $argv
    end

    function gcm
	git commit -m $arv
    end

    function gl
        git log --all --graph --format=oneline $argv
    end

    function gp
	git push $argv
    end

    function gpl
	git pull $argv
    end

    function ga.
        git add . $argv
    end

    function ga
	git add $argv
    end

    function gco
	git checkout $argv
    end
    
    function gb
	git branch $argv
    end

    function gd
	git diff $argv
    end

    function gm
	git merge $argv
    end

    function gs
	git show $argv
    end

    function gf
        git fetch $argv
    end

    function gr
        git rebase $argv
    end

    function gsta
	git stash $argv
    end

end
