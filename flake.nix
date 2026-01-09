{
  description = "Valhalla TimeDist HS Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
  };

  outputs = { self, nixpkgs, flake-utils, process-compose-flake, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true; # Valhalla might have unfree dependencies depending on version
        };

        haskellPackages = pkgs.haskell.packages.ghc910;

        myApp = haskellPackages.callCabal2nix "valhalla-timedist-hs" ./. {};

      in {
        # nix run (dev)
        packages.default = process-compose-flake.lib.${system}.processCompose {
          name = "valhalla-dev-env";
          process-compose = {
            processes = {
              valhalla = {
                command = "${pkgs.valhalla}/bin/valhalla_service config/valhalla.json 2";
                working_dir = "./";
                availability = {
                  restart = "on_failure";
                };
              };

              haskell-server = {
                command = "${pkgs.cabal-install}/bin/cabal run exe:valhalla-timedist-hs-exe";
                environment = [
                  "VALHALLA_URL=http://localhost:8002"
                ];
                depends_on = {
                  valhalla.condition = "process_healthy";
                };
              };
            };
          };
        };

        # nix build .#docker (production)
        packages.docker = pkgs.dockerTools.buildLayeredImage {
          name = "valhalla-timedist-hs";
          tag = "latest";
          created = "now";

          contents = [
            pkgs.cacert       # SSL certs
            pkgs.iana-etc     # Network protocols
            myApp             # Your app
          ];

          config = {
            Cmd = [ "${myApp}/bin/valhalla-timedist-hs-exe" ];
            ExposedPorts = { "3000/tcp" = {}; };
            Env = [ "VALHALLA_URL=http://valhalla:8002" ];
          };
        };

        # nix develop
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            cabal-install
            haskellPackages.ghc
            haskellPackages.haskell-language-server
            pkgs.valhalla # So you can run valhalla CLI tools manually
            pkgs.zlib
          ];

          # Sets up library paths for GHC to find zlib/etc
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.zlib pkgs.gmp ];
        };
      }
    );
}