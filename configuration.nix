# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  is_laptop = builtins.pathExists /sys/class/power_supply/BAT0;
in
{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  boot.kernelPackages = pkgs.linuxPackages_zen;
  hardware.enableAllFirmware = true;
  virtualisation.libvirtd.enable = true;
  virtualisation.docker.enable = true;
  programs.dconf.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = if is_laptop then "felipe-laptop" else "felipe-desktop"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = if is_laptop then {
    font = "Lat2-Terminus16";
    keyMap = "us";
  } else {
    font = "Lat2-Terminus16";
    keyMap = "br-abnt2";
  };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;

  programs.xwayland.enable = true;
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [
      waybar
      swaylock
      swayidle
      swaybg
      wl-clipboard xsel xclip
      mako # notification daemon
      alacritty # Alacritty is the default terminal in the config
      dmenu
	  rofi
      slurp
      brightnessctl
      gammastep
    ];
  };

  programs.zsh.enable = true;
  programs.zsh.promptInit = ''
    any-nix-shell zsh --info-right | source /dev/stdin
  '';

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.felipe = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "video" "docker" "libvirtd" ]; # Enable ‘sudo’ for the user.
  };

  fonts.fonts = with pkgs; [
	cantarell-fonts
    noto-fonts noto-fonts-emoji noto-fonts-cjk
    font-awesome font-awesome-ttf font-awesome_4
    jetbrains-mono hack-font cascadia-code
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget curl neovim git stow emacs fzf silver-searcher
    keychain
    firefox chromium discord spotify
    obs-studio obs-wlrobs xdg-desktop-portal xdg-desktop-portal-wlr
    clang_11 gcc10 gdb lldb gnumake cmake ninja meson ctags manpages
    lxappearance gnome-themes-standard
    pavucontrol playerctl
    youtube-dl mpv zathura tree htop psmisc
    any-nix-shell
    virt-manager
	python3
  ] ++ (if is_laptop then [] else [ pkgs.zenstates ]);

  systemd.services.disable-c6 = if !is_laptop then {
    description = "Ryzen Disable C6 State";
    wantedBy = [ "basic.target" ];
    after = [ "sysinit.target" "local-fs.target" ];
    serviceConfig.Type = "oneshot";
    serviceConfig.ExecStart = pkgs.writeBash "disable-c6-state" ''
      ${pkgs.kmod}/bin/modprobe msr
      ${pkgs.python3}/bin/python ${pkgs.zenstates}/zenstates.py --c6-disable --list
    '';
  } else {};

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

