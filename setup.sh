# This is a script I made for easily setting up a new system with my configs.
# It's made for personal use, so you might not want to use it.

echo "This script will:"
echo " - Install git (if not installed)"
echo " - Install pacaur (if not installed)"
echo " - Install packages listed in packages.txt (including AUR packages)"
echo " - Create symlinks for all of the configurations using \"stow\""
echo " - Set up the neovim package manager \"dein\""
echo " - Change the default shell of this user to zsh"
echo " - Set up oh-my-zsh"
echo " - Install the \"spaceship\" zsh theme"
echo ""
echo "The script also checks to see if things are already installed (except for packages)."
echo "You can disable package installation with \"-p false\"."
echo ""
echo "Press enter to continue."

read

PREFIX="$HOME/.felipeac"
INSTALL_PACKAGES=true
mkdir $PREFIX

while getopts "p" opt; do
	case $opt in
		p) INSTALL_PACKAGES=$OPTARG;;
	esac
done

trap "rm -rf $PREFIX; exit" INT

if ! hash sudo 2>/dev/null; then
	echo "The script requires \"sudo\" to be installed."
	exit
fi

if hash pacman 2>/dev/null; then
	if ! hash git 2>/dev/null; then
		echo "Installing git..."
		sudo pacman -S git
	else
		echo "Git is already installed"
	fi

	if ! hash pacaur 2>/dev/null; then
		echo "Installing pacaur..."


		git clone https://aur.archlinux.org/cower.git $PREFIX/cower
		git clone https://aur.archlinux.org/pacaur.git $PREFIX/pacaur

		makepkg -si -p $PREFIX/cower/PKGBUILD
		makepkg -si -p $PREFIX/pacaur/PKGBUILD

		rm -rf $PREFIX
	else
		echo "pacaur is already installed"
	fi
else
	echo "pacman not installed. This script only supports Arch Linux, sorry."
	exit
fi

if [ "$INSTALL_PACKAGES" == "true" ]; then
	if hash pacaur 2>/dev/null; then
		echo "Installing packages..."
		pacaur -S --noconfirm --noedit - < packages.txt
	fi
fi

if hash stow 2>/dev/null; then
	echo "Making symlinks..."
	stow */
fi

if ! test -e "$HOME/.local/share/dein"; then
	echo "Setting up dein.vim..."
	curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > $PREFIX/installer.sh
	sh $PREFIX/installer.sh ~/.local/share/dein
else
	echo "dein.vim is already installed"
fi

if [ "$SHELL" != "/bin/zsh" ]; then
	chsh -s /bin/zsh
else
	echo "zsh is already the default shell"
fi

if ! test -e "$HOME/.oh-my-zsh"; then
	echo "Setting up oh-my-zsh..."
	git clone --depth=1 https://github.com/robbyrussell/oh-my-zsh.git $HOME/.oh-my-zsh
else
	echo "oh-my-zsh is already installed"
fi

ZSH_CUSTOM="$HOME/.oh-my-zsh/custom/"

if ! test -e "$ZSH_CUSTOM/themes/spaceship.zsh-theme"; then
	echo "Installing the \"spacehip\" zsh theme"
	mkdir $ZSH_CUSTOM/themes
	curl https://raw.githubusercontent.com/denysdovhan/spaceship-zsh-theme/master/spaceship.zsh > $ZSH_CUSTOM/themes/spaceship.zsh-theme
else
	echo "Spaceship theme is already installed"
fi

rm -rf $PREFIX
echo "Done."
