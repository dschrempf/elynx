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
        elynx-create-package = f: name: f name (./. + "/${name}") rec { };
        elynx-overlay = (
          selfn: supern: {
            haskellPackages = supern.haskell.packages.ghc923.override {
              overrides = selfh: superh:
                lib.genAttrs packageNames
                  (elynx-create-package selfh.callCabal2nix);
            };
          }
        );
        overlays = [ elynx-overlay ];
        pkgs = import nixpkgs { inherit system overlays; };
        # When changing the package set, the override above also has to be amended.
        hpkgs = pkgs.haskellPackages;
        elynx-pkgs = lib.genAttrs packageNames (n: hpkgs.${n});
        # Add Bash completion.
        elynx =
          let
            f = pkgs.haskell.lib.generateOptparseApplicativeCompletion;
            slynx-completion = f "slynx" elynx-pkgs.slynx;
            tlynx-completion = f "tlynx" elynx-pkgs.tlynx;
            elynx-completion = f "elynx" elynx-pkgs.elynx;
          in
          elynx-pkgs // {
            slynx = slynx-completion;
            tlynx = tlynx-completion;
            elynx = elynx-completion;
          };
        # Development environment with benchmarks.
        elynx-dev = builtins.mapAttrs
          (
            _: x: pkgs.haskell.lib.overrideCabal x (
              _: { doBenchmark = true; }
            )
          )
          elynx;
        # Environment including all packages.
        elynx-suite = pkgs.buildEnv {
          name = "ELynx suite";
          paths = builtins.attrValues elynx;
        };
      in
      {
        packages = elynx // {
          inherit elynx-suite;
          default = elynx-suite;
        };

        devShells.default = hpkgs.shellFor {
          packages = _: (builtins.attrValues elynx-dev);
          buildInputs = with pkgs; [
            bashInteractive
            hpkgs.cabal-install
            hpkgs.haskell-language-server
          ];
          doBenchmark = true;
          # withHoogle = true;
        };
      }
    );
}
