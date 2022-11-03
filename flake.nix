{
  description = "ELynx";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
  # inputs.nixpkgs.url = "path:/home/dominik/Nix/Nixpkgs";

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        lib = nixpkgs.lib;
        packageNames = [
          "elynx"
          "elynx-markov"
          "elynx-nexus"
          "elynx-seq"
          "elynx-tools"
          "elynx-tree"
          "slynx"
          "tlynx"
        ];
        ghcVersion = "ghc924";
        haskellMkPackage = f: name: f name (./. + "/${name}") rec { };
        haskellOverlay = (
          selfn: supern: {
            haskellPackages = supern.haskell.packages.${ghcVersion}.override {
              overrides = selfh: superh:
                lib.genAttrs packageNames (haskellMkPackage selfh.callCabal2nix);
            };
          }
        );
        overlays = [ haskellOverlay ];
        pkgs = import nixpkgs { inherit system overlays; };
        # When changing the package set, the override above also has to be amended.
        hpkgs = pkgs.haskellPackages;
        elynxPkgsNoCompletion = lib.genAttrs packageNames (n: hpkgs.${n});
        # Add Bash completion.
        elynxPkgs =
          let
            f = pkgs.haskell.lib.generateOptparseApplicativeCompletion;
            slynxCompletion = f "slynx" elynxPkgsNoCompletion.slynx;
            tlynxCompletion = f "tlynx" elynxPkgsNoCompletion.tlynx;
            elynxCompletion = f "elynx" elynxPkgsNoCompletion.elynx;
          in
          elynxPkgsNoCompletion // {
            slynx = slynxCompletion;
            tlynx = tlynxCompletion;
            elynx = elynxCompletion;
          };
        # Development environment with benchmarks.
        elynxPkgsDev = builtins.mapAttrs
          (
            _: x: pkgs.haskell.lib.overrideCabal x (
              _: { doBenchmark = true; }
            )
          )
          elynxPkgs;
        # Environment including all packages.
        elynxSuite = pkgs.buildEnv {
          name = "ELynx suite";
          paths = builtins.attrValues elynxPkgs;
        };
      in
      {
        packages = elynxPkgs // {
          inherit elynxSuite;
          default = elynxSuite;
        };

        devShells.default = hpkgs.shellFor {
          packages = _: (builtins.attrValues elynxPkgsDev);
          buildInputs = with pkgs; [
            bashInteractive

            hpkgs.cabal-fmt
            hpkgs.cabal-install
            hpkgs.haskell-language-server
          ];
          doBenchmark = true;
          # withHoogle = true;
        };
      }
    );
}
