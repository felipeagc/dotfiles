export LANG=en_US.UTF-8

export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="spaceship"

SPACESHIP_DOCKER_SHOW=false

plugins=(git pass sudo docker)

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

alias song="mpc searchplay title $1"

mkcd () {
	mkdir -p -- "$1" &&
	cd -P -- "$1"
}

update () {
	pacaur -Syu &&
	flatpak update --user &&
	sudo flatpak update &&
	rustup update &&
	sudo npm -g update
}


transfer() {
	if [ $# -eq 0 ]; then echo -e "No arguments specified. Usage:\necho transfer /tmp/test.md\ncat /tmp/test.md | transfer test.md"; return 1; fi
	tmpfile=$( mktemp -t transferXXX ); if tty -s; then basefile=$(basename "$1" | sed -e 's/[^a-zA-Z0-9._-]/-/g'); curl --progress-bar --upload-file "$1" "https://transfer.sh/$basefile" >> $tmpfile; else curl --progress-bar --upload-file "-" "https://transfer.sh/$1" >> $tmpfile ; fi; cat $tmpfile; rm -f $tmpfile;
}

alias fp="flatpak"

alias ec="emacsclient -n"

source ~/.zprofile

if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
        source /etc/profile.d/vte.sh
fi
