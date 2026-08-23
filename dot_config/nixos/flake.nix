{
  description = "Felipe's NixOS configuration";

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
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      llm-agents,
      ...
    }:
    {
      nixosConfigurations.felipe-nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          ./hardware-configuration.nix
          ./configuration.nix
          home-manager.nixosModules.home-manager

          {
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
