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
      home-manager,
      llm-agents,
      ...
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
        ];
      };
    in {
      homeConfigurations.felipe = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [{
          home.username = "felipe";
          home.homeDirectory = "/home/felipe";
          home.stateVersion = "26.05";

          news.display = "silent";

          nixpkgs.overlays = [ llm-agents.overlays.default ];

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
            blender
            f3d
            zenity
            rustup
            delta
            ast-grep
            _1password-gui
            _1password-cli
            godotPackages_4_6.godot-mono
            godotPackages_4_6.export-templates-mono-bin
            dotnet-sdk_10
            dotnet-runtime_10
            just

            claude-code
            codex
          ];

          programs.home-manager.enable = true;
        }];
      };
    };
}
