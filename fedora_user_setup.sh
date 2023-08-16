#!/bin/sh

gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-1 '[]'
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-2 '[]'
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-3 '[]'
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-4 '[]'
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-5 '[]'
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-6 '[]'
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-7 '[]'
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-8 '[]'
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-9 '[]'
gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-0 '[]'

gsettings set org.gnome.shell.keybindings switch-to-application-1 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-2 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-3 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-4 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-5 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-6 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-7 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-8 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-9 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-0 '[]'

gsettings set org.gnome.desktop.wm.keybindings switch-to-application-1 []
gsettings set org.gnome.desktop.wm.keybindings switch-to-application-2 []
gsettings set org.gnome.desktop.wm.keybindings switch-to-application-3 []
gsettings set org.gnome.desktop.wm.keybindings switch-to-application-4 []
gsettings set org.gnome.desktop.wm.keybindings switch-to-application-5 []
gsettings set org.gnome.desktop.wm.keybindings switch-to-application-6 []
gsettings set org.gnome.desktop.wm.keybindings switch-to-application-7 []
gsettings set org.gnome.desktop.wm.keybindings switch-to-application-8 []
gsettings set org.gnome.desktop.wm.keybindings switch-to-application-9 []

gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-1 "['<Super>1']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-2 "['<Super>2']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-3 "['<Super>3']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-4 "['<Super>4']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-5 "['<Super>5']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-6 "['<Super>6']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-7 "['<Super>7']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-8 "['<Super>8']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-9 "['<Super>9']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-10 "['<Super>0']"

gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-1 "['<Shift><Super>1']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-2 "['<Shift><Super>2']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-3 "['<Shift><Super>3']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-4 "['<Shift><Super>4']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-5 "['<Shift><Super>5']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-6 "['<Shift><Super>6']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-7 "['<Shift><Super>7']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-8 "['<Shift><Super>8']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-9 "['<Shift><Super>9']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-10 "['<Shift><Super>0']"

gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 20
gsettings set org.gnome.desktop.peripherals.keyboard delay 230
gsettings set org.gnome.desktop.peripherals.mouse accel-profile flat

mkdir -p ~/.local/bin
mkdir -p ~/.config
stow zsh nvim emacs tmux bin kitty

zsh -c "go install github.com/jesseduffield/lazygit@latest"
