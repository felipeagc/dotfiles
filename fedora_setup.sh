#!/bin/sh

dnf5 install -y dnf-plugins-core

dnf install -y https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm

rpm --import https://download.owncloud.com/desktop/ownCloud/stable/latest/linux/Fedora_38/repodata/repomd.xml.key
dnf config-manager --add-repo https://download.owncloud.com/desktop/ownCloud/stable/latest/linux/Fedora_38/owncloud.repo

dnf config-manager --add-repo https://pkgs.tailscale.com/stable/fedora/tailscale.repo

dnf config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo

groupadd docker

dnf5 install -y \
    nodejs go rust python \
    git stow @development-tools gcc g++ clang \
    kitty neovim zsh emacs tmux \
    owncloud-client tailscale \
    thunderbird mpv toolbox

dnf install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

systemctl enable --now tailscaled
systemctl enable --now docker
systemctl enable --now docker.socket

# Codecs
dnf swap -y ffmpeg-free ffmpeg --allowerasing
dnf groupupdate -y multimedia --setop="install_weak_deps=False" --exclude=PackageKit-gstreamer-plugin
dnf groupupdate -y sound-and-video

# Hardware encoders
dnf install -y intel-media-driver
dnf swap mesa-va-drivers mesa-va-drivers-freeworld

# Flatpaks
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak install -y flathub com.discordapp.Discord
flatpak install -y flathub com.brave.Browser
flatpak install -y flathub com.spotify.Client
flatpak install -y flathub com.obsproject.Studio
flatpak install -y flathub org.blender.Blender
flatpak install -y flathub org.telegram.desktop
flatpak install -y flathub com.mattjakeman.ExtensionManager
flatpak install -y flathub md.obsidian.Obsidian
