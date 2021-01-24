PACKAGES=$(shell cat ./arch_packages)
all:
	stow */
	sudo pacman -S --needed $(PACKAGES)
	sudo make -C ~/.dwm install
	sudo make -C ~/.st install
	sudo make -C ~/.dmenu install

