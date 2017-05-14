# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export LANG=en_US.UTF-8

source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"

unsetopt CORRECT

extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2) tar xvjf $1;;
            *.tar.gz) tar xvzf $1;;
            *.bz2) bunzip2 $1;;
            *.rar) unrar x $1;;
            *.gz) gunzip $1;;
            *.tar) tar xvf $1;;
            *.tbz2) tar xvjf $1;;
            *.tgz) tar xvzf $1;;
            *.zip) unzip $1;;
            *.Z) uncompress $1;;
            *.7z) 7za x $1;;
            *.rar) unrar $1;;
            *.zip) unzip $1;;
            *) echo "'$1' cannot be extracted via >extract<" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

alias open="xdg-open"

alias ga="git add $1"
alias gaa="git add \$(git ls-files -m | fzf -m)"
alias gr="git rm $1"
alias gc="git commit -m $1"
alias gp="git push $1 $2"
alias gs="git status"

alias song="mpc searchplay title $1"

alias update="pacaur -Syu && flatpak update --user"

source ~/.zprofile

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
