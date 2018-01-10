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
alias gaa="git add \$(git ls-files -m | fzf -m)"
alias gr="git rm $1"
alias gc="git commit -m $1"
alias gp="git push $1 $2"
alias gs="git status"

alias docket="docker"
alias d="docker"
alias dc="docker-compose"

alias v="nvim"

alias song="mpc searchplay title $1"

# Execute ls every time we cd
function chpwd() {
    emulate -L zsh
    ls
}

pacs () {
	results=$(pacaur -Ssq $@)
	if [[ -z "$results" ]]; then
		echo "No results."
	else
		selected=$(echo $results | fzf -m)
		pacaur -S $selected
	fi
}

mkcd () {
	mkdir -p -- "$1" &&
	cd -P -- "$1"
}

source ~/.profile
