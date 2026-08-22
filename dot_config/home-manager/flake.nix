{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    llm-agents.url = "github:numtide/llm-agents.nix";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
      nixpkgs,
      llm-agents,
      home-manager,
      ...
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;

        config.allowUnfree = true;

        overlays = [ llm-agents.overlays.shared-nixpkgs ];
      };
    in {
      homeConfigurations.felipe = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [{
          home.username = "felipe";
          home.homeDirectory = "/home/felipe";
          home.stateVersion = "26.05";

          news.display = "silent";

          home.packages = with pkgs; [
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

            pkgs.llm-agents.claude-code
            pkgs.llm-agents.grok
            pkgs.llm-agents.pi
            codex
          ];

          programs.home-manager.enable = true;
        }];
      };
    };
}
