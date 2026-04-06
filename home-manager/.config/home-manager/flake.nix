{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
      nixpkgs,
      # neovim-nightly-overlay,
      home-manager,
      ...
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          # neovim-nightly-overlay.overlays.default
        ];
      };
    in {
      homeConfigurations.felipe = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [{
          home.username = "felipe";
          home.homeDirectory = "/home/felipe";
          home.stateVersion = "24.11";

          news.display = "silent";

          home.packages = with pkgs; [
            neovim
            jujutsu
            jjui
            lazygit
            fzf
            ripgrep
            eternal-terminal
            tmux
            nodejs_24
            tree-sitter
            pi-coding-agent
          ];

          systemd.user.services.etserver = {
            Unit.Description = "Eternal Terminal Server";
            Service = {
              ExecStart = "${pkgs.eternal-terminal}/bin/etserver";
              Restart = "on-failure";
            };
            Install.WantedBy = [ "default.target" ];
          };

          programs.home-manager.enable = true;
        }];
      };
    };
}
