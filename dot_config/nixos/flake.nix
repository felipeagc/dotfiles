{
  description = "Felipe's NixOS configuration";

  nixConfig = {
    extra-substituters = [ "https://noctalia.cachix.org" ];
    extra-trusted-public-keys = [ "noctalia.cachix.org-1:pCOR47nnMEo5thcxNDtzWpOxNFQsBRglJzxWPp3dkU4=" ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    noctalia = {
      url = "github:noctalia-dev/noctalia";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      llm-agents,
      noctalia,
      ...
    }:
    {
      nixosConfigurations.felipe-nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          ./hardware-configuration.nix
          ./configuration.nix
          home-manager.nixosModules.home-manager
          noctalia.nixosModules.default

          {
            programs.noctalia = {
              enable = true;
              systemd.enable = true;
              # Enables NetworkManager, Bluetooth, UPower, and a power profile service.
              recommendedServices.enable = true;
            };

            nixpkgs.overlays = [
              llm-agents.overlays.shared-nixpkgs
            ];

            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.felipe = import ./home.nix;
            };
          }
        ];
      };
    };
}
