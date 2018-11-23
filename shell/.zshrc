export LANG=en_US.UTF-8

export ZSH=/home/felipe/.oh-my-zsh

ZSH_THEME="simple"

plugins=(
  git
)

source $ZSH/oh-my-zsh.sh

unsetopt CORRECT

alias open="xdg-open"

alias ga="git add $1"
alias gc="git commit -m $1"
alias gp="git push $1 $2"
alias gs="git status"

alias docket="docker"
alias dc="docker-compose"

alias song="mpc searchplay title $1"

# Execute ls every time we cd
function chpwd() {
  emulate -L zsh
  ls
}

mkcd () {
  mkdir -p -- "$1" &&
  cd -P -- "$1"
}

source ~/.profile
