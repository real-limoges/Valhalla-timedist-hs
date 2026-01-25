{
  description = "Valhalla TimeDist HS Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        haskellPackages = pkgs.haskell.packages.ghc910;
        myApp = haskellPackages.callCabal2nix "valhalla-timedist-hs" ./. {};

        pcConfig = pkgs.writeText "process-compose.yaml" (builtins.toJSON {
          version = "0.5";
          processes = {
            # Service 1: Valhalla (Docker)
            valhalla = {
              command = "docker run --rm --name valhalla_nix -p 8002:8002 -v $(pwd)/config:/config -v $(pwd)/data:/data ghcr.io/valhalla/valhalla:3.6.1 valhalla_service /config/valhalla.json 2";
              availability = {
                restart = "always";
              };
            };

            haskell-server = {
              command = "${pkgs.cabal-install}/bin/cabal run exe:valhalla-timedist-hs-exe";
              environment = [ "VALHALLA_URL=http://localhost:8002" ];
              depends_on = {
                valhalla.condition = "process_healthy";
              };
            };
          };
        });

      in {
        packages.default = pkgs.writeShellScriptBin "start-dev" ''
          # Ensure config exists or warn user
          if [ ! -f ./config/valhalla.json ]; then
            echo "Error: ./config/valhalla.json not found."
            exit 1
          fi

          echo "Starting Valhalla & Haskell Environment..."
          ${pkgs.process-compose}/bin/process-compose -f ${pcConfig}
        '';

        packages.docker = pkgs.dockerTools.buildLayeredImage {
          name = "valhalla-timedist-hs";
          tag = "latest";
          contents = [ pkgs.cacert pkgs.iana-etc myApp ];
          config = {
            Cmd = [ "${myApp}/bin/valhalla-timedist-hs-exe" ];
            ExposedPorts = { "9000/tcp" = {}; };
            Env = [ "VALHALLA_URL=http://valhalla:8002" ];
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            cabal-install
            haskellPackages.ghc
            haskellPackages.haskell-language-server
            pkgs.zlib
            pkgs.process-compose # So you can run 'process-compose' manually if you want
          ];
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.zlib pkgs.gmp ];
        };
      }
    );
}