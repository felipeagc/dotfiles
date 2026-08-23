{ pkgs, ... }:

let
  help = pkgs.writeShellScriptBin "help" ''
    cat <<'EOF'
    NixOS configuration
    ===================

    Apply dotfile changes, validate the system, and activate it:

      chezmoi diff
      chezmoi apply
      sudo nixos-rebuild dry-build --flake ~/.config/nixos#felipe-nixos
      sudo nixos-rebuild switch --flake ~/.config/nixos#felipe-nixos

    Update Nix packages and apply the new lock file:

      cd ~/.config/nixos
      nix flake update
      sudo nixos-rebuild dry-build --flake .#felipe-nixos
      sudo nixos-rebuild switch --flake .#felipe-nixos
      chezmoi add ~/.config/nixos/flake.lock

    Inspect flake inputs:

      nix flake metadata ~/.config/nixos

    Roll back to the previous system generation:

      sudo nixos-rebuild switch --rollback

    Useful chezmoi commands:

      chezmoi status
      chezmoi diff
      chezmoi apply
      chezmoi cd
    EOF
  '';
in
{
  home.username = "felipe";
  home.homeDirectory = "/home/felipe";
  home.stateVersion = "26.05";

  news.display = "silent";

  home.packages = with pkgs; [
    help
    neovim
    jujutsu
    jjui
    lazygit
    fzf
    ripgrep
    tmux
    nodejs_24
    tree-sitter
    zenity
    rustup
    delta
    ast-grep
    _1password-gui
    _1password-cli
    just
    tokei
    chezmoi
    sesh

    llm-agents.claude-code
    llm-agents.pi
    llm-agents.cursor-agent
    codex
  ];

  programs.home-manager.enable = true;
}
