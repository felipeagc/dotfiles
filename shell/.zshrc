export LANG=en_US.UTF-8

export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="simple"

plugins=(
  git
)

source $ZSH/oh-my-zsh.sh

unsetopt CORRECT

export EDITOR=nvim
export GIT_EDITOR=nvim
export BROWSER=chromium
export TERMINAL=xst
export FILEBROWSER=nautilus

if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

alias ga="git add $1"
alias gc="git commit -m $1"
alias gp="git push $1 $2"
alias gs="git status"

alias docket="docker"
alias dc="docker-compose"

# Execute ls every time we cd
function chpwd() {
  emulate -L zsh
  ls
}

mkcd () {
  mkdir -p -- "$1" &&
  cd -P -- "$1"
}
