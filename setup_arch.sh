#!/usr/bin/env bash
USER=felipe

sudo pacman -S --needed - < arch_packages.txt

pikaur -S --needed - < aur_packages.txt

mkdir -p ~/.config
mkdir -p ~/.local/bin
mkdir -p ~/.local/share/applications

stow nvim
stow ghostty-linux
stow walker
stow waybar
stow hypr
stow zsh
stow jj
stow tmux
stow linux-bin
stow uwsm
stow xdg

elephant service enable
systemctl enable --user elephant
systemctl start --user elephant

# Tell SDDM to auto-login
sudo mkdir -p /etc/sddm.conf.d
sudo tee /etc/sddm.conf.d/autologin.conf > /dev/null <<EOF
[Autologin]
User=$USER
Session=hyprland-uwsm
EOF

if [ ! -f $HOME/.local/share/applications/ChatGPT.desktop ]; then
    omarchy-webapp-install ChatGPT "https://chatgpt.com" "https://cdn.jsdelivr.net/gh/homarr-labs/dashboard-icons/png/chatgpt.png"
fi
if [ ! -f $HOME/.local/share/applications/WhatsApp.desktop ]; then
    omarchy-webapp-install WhatsApp "https://web.whatsapp.com" "https://cdn.jsdelivr.net/gh/homarr-labs/dashboard-icons/png/whatsapp.png"
fi
