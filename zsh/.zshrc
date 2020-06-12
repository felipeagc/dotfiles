# Set up the prompt
export PS1=$'%F{cyan}[%F{reset}%B%n@%M%b %~%F{cyan}]%F{green}$%F{reset} '

setopt histignorealldups sharehistory hist_ignore_space

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

alias ls='ls --color=auto'

alias ga="git add $1"
alias gc="git commit -m $1"
alias gp="git push $1 $2"
alias gs="git status"

# Execute ls every time we cd
function chpwd() {
  emulate -L zsh
  ls
}

mkcd () {
  mkdir -p -- "$1" &&
  cd -P -- "$1"
}
