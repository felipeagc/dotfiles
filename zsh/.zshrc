# Set up the prompt
export PROMPT=$'%F{green}[%F{reset}%B%n@%M%b%F{green}] %F{cyan}%~ %F{red}$vcs_info_msg_0_%f\n%F{yellow}$%F{reset} '
# Print new line before every prompt except the first one
precmd() {
    precmd() {
        echo
    }
}

setopt histignorealldups sharehistory hist_ignore_space

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 5000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.zsh_history

export EDITOR=nvim


# Use modern completion system
fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit && compinit -i

# Load git plugin
autoload -Uz add-zsh-hook vcs_info
setopt prompt_subst
add-zsh-hook precmd vcs_info
# Style the vcs_info message
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*' formats ' %b%u%c'
# Format when the repo is in an action (merge, rebase, etc)
zstyle ':vcs_info:git*' actionformats '%F{14}⏱ %*%f'
zstyle ':vcs_info:git*' unstagedstr '*'
zstyle ':vcs_info:git*' stagedstr '+'
# This enables %u and %c (unstaged/staged changes) to work,
# but can be slow on large repos
zstyle ':vcs_info:*:*' check-for-changes true

if type "dircolors" > /dev/null; then
    eval "$(dircolors -b)"
fi

if type "gdircolors" > /dev/null; then
    eval "$(gdircolors -b)"
fi

zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' menu select
zstyle ':completion:*:*:docker:*' option-stacking yes
zstyle ':completion:*:*:docker-*:*' option-stacking yes

if type "keychain" > /dev/null; then
    KEYCHAIN_CMD=(keychain -q --eval)
    [[ -e ~/.ssh/id_ed25519 ]] && KEYCHAIN_CMD+=id_ed25519
    [[ -e ~/.ssh/id_secondary ]] && KEYCHAIN_CMD+=id_secondary
    eval `$KEYCHAIN_CMD`
fi

if [[ "$OSTYPE" == "linux"* ]]; then
    export SSH_ASKPASS=/usr/lib/seahorse/ssh-askpass
    if [ -z "$SSH_AUTH_SOCK" ]; then
        eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
        export SSH_AUTH_SOCK
    fi
fi

if type "direnv" > /dev/null; then
    eval "$(direnv hook zsh)"
fi

alias ls='ls --color=auto'
alias ll="ls -l"
alias lg="lazygit"

alias ga="git add $1"
alias gc="git commit -m $1"
alias gp="git push $1 $2"
alias gs="git status"
alias runemacs="emacs & disown"

alias pn="peanuts"

# Execute ls every time we cd
# function chpwd() {
#   emulate -L zsh
#   ls
# }

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
    export QT_QPA_PLATFORM="wayland"
fi
export QT_QPA_PLATFORMTHEME="qt5ct"

export DOTNET_CLI_TELEMETRY_OPTOUT=1

export ERL_AFLAGS="-kernel shell_history enabled"

bindkey -s ^f "tmux-sessionizer\n"

[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

alias j=just
alias s="kitten ssh"

if type "mise" > /dev/null; then
    eval "$(mise activate zsh)"
fi

if type "xc" > /dev/null; then
    autoload -U +X bashcompinit && bashcompinit
    complete -o nospace -C /opt/homebrew/bin/xc xc
fi

test -r /opt/homebrew/opt/dotnet/libexec && export DOTNET_ROOT="/opt/homebrew/opt/dotnet/libexec"

eval "$(zoxide init zsh)"
alias cd=z
