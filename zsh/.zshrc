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

if type "keychain" > /dev/null; then
    KEYCHAIN_CMD=(keychain -q --eval --agents ssh)
    [[ -e ~/.ssh/id_primary ]] && KEYCHAIN_CMD+=id_primary
    [[ -e ~/.ssh/id_secondary ]] && KEYCHAIN_CMD+=id_secondary
    eval `$KEYCHAIN_CMD`
fi

if type "direnv" > /dev/null; then
    eval "$(direnv hook zsh)"
fi

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

export PATH=$PATH:$HOME/.local/bin

export GOPATH=$HOME/.gopath
export PATH=$PATH:$GOPATH/bin

export _JAVA_AWT_WM_NONREPARENTING=1
export COMPOSE_DOCKER_CLI_BUILD=1
export DOCKER_BUILDKIT=1

# opam configuration
test -r $HOME/.opam/opam-init/init.zsh && . $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
    export MOZ_ENABLE_WAYLAND=1
fi
