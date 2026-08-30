#!/bin/sh
# Print "light" or "dark" for the current system appearance.

# Manual override wins, for SSH sessions or terminals that can't be detected.
if [ -n "$TMUX_THEME" ]; then
	echo "$TMUX_THEME"
	exit 0
fi

# macOS: the key is absent entirely in light mode.
if [ "$(uname)" = "Darwin" ]; then
	if [ "$(defaults read -g AppleInterfaceStyle 2>/dev/null)" = "Dark" ]; then
		echo dark
	else
		echo light
	fi
	exit 0
fi

# Linux, desktop-agnostic: the XDG appearance portal.
# 0 = no preference, 1 = prefer dark, 2 = prefer light.
if command -v gdbus >/dev/null 2>&1; then
	scheme=$(gdbus call --session \
		--dest org.freedesktop.portal.Desktop \
		--object-path /org/freedesktop/portal/desktop \
		--method org.freedesktop.portal.Settings.ReadOne \
		org.freedesktop.appearance color-scheme 2>/dev/null)
	case "$scheme" in
	*"uint32 1"*) echo dark ; exit 0 ;;
	*"uint32 2"*) echo light ; exit 0 ;;
	esac
fi

# GNOME fallback.
if command -v gsettings >/dev/null 2>&1; then
	scheme=$(gsettings get org.gnome.desktop.interface color-scheme 2>/dev/null)
	case "$scheme" in
	*prefer-dark*) echo dark ; exit 0 ;;
	*prefer-light*) echo light ; exit 0 ;;
	esac
	# color-scheme can be "default"; fall back to the GTK theme name.
	case "$(gsettings get org.gnome.desktop.interface gtk-theme 2>/dev/null)" in
	*-[Dd]ark*) echo dark ; exit 0 ;;
	esac
fi

# Last resort: some terminals export COLORFGBG as "fg;bg" with an ANSI bg index.
case "$COLORFGBG" in
*';'[0-6]|*';'8) echo dark ; exit 0 ;;
*';'7|*';'1[0-5]) echo light ; exit 0 ;;
esac

echo dark
