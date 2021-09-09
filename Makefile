PACKAGES=$(shell cat ./arch_packages)
all:
	stow */
	sudo pacman -S --needed $(PACKAGES)
