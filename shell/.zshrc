export LANG=en_US.UTF-8

export ZSH=$HOME/.oh-my-zsh

SPACESHIP_PROMPT_SYMBOL="➜"
ZSH_THEME="spaceship"

plugins=(git pass sudo)

source $ZSH/oh-my-zsh.sh

unsetopt CORRECT

alias open="xdg-open"

alias ga="git add $1"
alias gaa="git add \$(git ls-files -m | fzf -m)"
alias gr="git rm $1"
alias gc="git commit -m $1"
alias gp="git push $1 $2"
alias gs="git status"

alias song="mpc searchplay title $1"

alias update="pacaur -Syu && flatpak update --user"
alias fp="flatpak"

source ~/.zprofile

