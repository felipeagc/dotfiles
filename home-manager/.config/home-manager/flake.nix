{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
      nixpkgs,
      home-manager,
      ...
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
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
            rustup
          ];

          programs.home-manager.enable = true;
        }];
      };
    };
}
