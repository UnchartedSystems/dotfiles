function ec
    /Applications/Macports/Emacs.app/Contents/MacOS/bin/emacsclient -e '(+ 1 0)' &>/dev/null
    if test $status -ne 0
        /Applications/Macports/Emacs.app/Contents/MacOS/Emacs --daemon
    end
    /Applications/Macports/Emacs.app/Contents/MacOS/bin/emacsclient -c -n -e '(select-frame-set-input-focus (selected-frame))' $argv
end

alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
set -xg PATH ~/.local/bin /opt/local/bin /opt/local/sbin /Applications/Obsidian.app/Contents/MacOS $PATH
