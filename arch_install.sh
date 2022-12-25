#!/bin/bash

# Connect to wifi
iwctl
	device list
	station $WLAN_DEVICE scan
	station $WLAN_DEVICE get-networks # list networks
	station $WLAN_DEVICE connect $SSID
	exit

loadkeys br-abnt2 # load keyboard layout

timedatectl set-ntp true # update system clock

parted /dev/sda
	mklabel gpt
	mkpart "EFI system partition" fat32 1MiB 512MiB
	set 1 esp on
	mkpart "root partition" ext4 512MiB 100GiB
	mkpart "home partition" ext4 100GiB 100%
	quit

mkfs.fat -F32 /dev/sda1 # boot partition
mkfs.ext4 /dev/sda2 # root partition
mkfs.ext4 /dev/sda3 # home partition

e2label /dev/sda2 arch_os # set root label to "arch_os"

mkdir -p /mnt/boot
mkdir -p /mnt/home

mount /dev/sda2 /mnt # root partition
mount /dev/sda1 /mnt/boot # boot partition
mount /dev/sda3 /mnt/home # home partition

pacstrap /mnt $(cat arch_packages)
genfstab -U /mnt >> /mnt/etc/fstab
arch-chroot /mnt

echo "swapfc_enabled=1" >> /etc/systemd/swap.conf
systemctl enable systemd-swap
systemctl enable systemd-networkd
systemctl enable systemd-resolved
systemctl enable iwd
systemctl enable dhcpcd
systemctl enable sshd
systemctl enable libvirtd

ln -sf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
ln -sf /usr/share/zoneinfo/America/Sao_Paulo /etc/localtime
hwclock --systohc

systemctl enable systemd-timesyncd
timedatectl set-ntp true

echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
locale-gen
echo "KEYMAP=br-abnt2" > /etc/vconsole.conf # keyboard layout
localectl set-locale LANG=en_US.UTF-8

echo "felipe-desktop" >> /etc/hostname

echo "127.0.0.1		localhost" >> /etc/hosts
echo "::1			localhost" >> /etc/hosts
echo "127.0.0.1		felipe-desktop.localdomain	felipe-desktop" >> /etc/hosts

mkinitcpio -P

passwd # set root password

bootctl install
echo 'title		Arch Linux' >> /boot/loader/entries/arch.conf
echo 'linux		/vmlinuz-linux' >> /boot/loader/entries/arch.conf
echo 'initrd	/intel-ucode.img' >> /boot/loader/entries/arch.conf
echo 'initrd	/initramfs-linux.img' >> /boot/loader/entries/arch.conf
echo 'options	root="LABEL=arch_os" rw' >> /boot/loader/entries/arch.conf

# replace default entry
sed -i "s/default.*/default arch\.conf/g" /boot/loader/loader.conf
echo "timeout 3" >> /boot/loader/loader.conf

bootctl update # maybe not needed?

useradd -m -G wheel -s /bin/zsh felipe
passwd felipe # set user password

echo "%wheel ALL=(ALL) ALL" >> /etc/sudoers # allow users in wheel to use sudo

# Fix thinkpad touchpad not working after sleep
echo "options psmouse synaptics_intertouch=0" >> /etc/modprobe.d/psmouse_serio2_setup.conf

su felipe
cd $HOME
git clone git@github.com:felipeagc/dotfiles.git
cd dotfiles
make
exit # exit su

exit # exit chroot

umount -R /mnt

reboot
