# Set up the prompt
export PS1=$'%F{cyan}[%F{reset}%B%n@%M%b %F{yellow}%~%F{cyan}]%F{green}$%F{reset} '

setopt histignorealldups sharehistory hist_ignore_space

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

export EDITOR=nvim

# Use modern completion system
autoload -Uz compinit
compinit

eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' menu select

eval `keychain -q --eval --agents ssh id_rsa`

alias ls='ls --color=auto'
alias ll="ls -l"

alias ga="git add $1"
alias gc="git commit -m $1"
alias gp="git push $1 $2"
alias gs="git status"
alias runemacs="emacs & disown"

# Execute ls every time we cd
function chpwd() {
  emulate -L zsh
  ls
}

mkcd () {
  mkdir -p -- "$1" &&
  cd -P -- "$1"
}

export GOPATH=$HOME/.gopath
export PATH=$PATH:$GOPATH/bin

export _JAVA_AWT_WM_NONREPARENTING=1

export QEMU_LD_PREFIX=/usr/aarch64-linux-gnu

# opam configuration
test -r /home/felipe/.opam/opam-init/init.zsh && . /home/felipe/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
