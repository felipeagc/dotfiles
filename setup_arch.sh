#!/usr/bin/env bash
USER=felipe

sudo tee /usr/share/wayland-sessions/niri-uwsm.desktop > /dev/null <<'EOF'
[Desktop Entry]
Name=Niri (UWSM)
Comment=Niri compositor managed by UWSM
Exec=uwsm start -F -- niri-session
DesktopNames=niri
Type=Application
EOF

sudo tee /etc/pacman.conf > /dev/null <<'EOF'
# See the pacman.conf(5) manpage for option and repository directives

[options]
Color
ILoveCandy
VerbosePkgLists
HoldPkg = pacman glibc
Architecture = auto
CheckSpace
ParallelDownloads = 5
DownloadUser = alpm

# By default, pacman accepts packages signed by keys that its local keyring
# trusts (see pacman-key and its man page), as well as unsigned packages.
SigLevel = Required DatabaseOptional
LocalFileSigLevel = Optional

# pacman searches repositories in the order defined here
[core]
Include = /etc/pacman.d/mirrorlist

[extra]
Include = /etc/pacman.d/mirrorlist

[multilib]
Include = /etc/pacman.d/mirrorlist

[omarchy]
SigLevel = Optional TrustAll
Server = https://pkgs.omarchy.org/$arch
EOF

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
Session=niri-uwsm
EOF

if [ ! -f $HOME/.local/share/applications/ChatGPT.desktop ]; then
    omarchy-webapp-install ChatGPT "https://chatgpt.com" "https://cdn.jsdelivr.net/gh/homarr-labs/dashboard-icons/png/chatgpt.png"
fi
if [ ! -f $HOME/.local/share/applications/WhatsApp.desktop ]; then
    omarchy-webapp-install WhatsApp "https://web.whatsapp.com" "https://cdn.jsdelivr.net/gh/homarr-labs/dashboard-icons/png/whatsapp.png"
fi

sudo ln -sf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
