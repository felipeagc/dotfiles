# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="spaceship"

# Use case-sensitive completion.
CASE_SENSITIVE="false"

plugins=(git)

source $ZSH/oh-my-zsh.sh

. $HOME/.profile

