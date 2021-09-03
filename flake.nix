{
  description = "ELynx";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

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
            elynx-create-package = f: name: f name (./. + "/${name}") rec {};
            elynx-overlay = (
              selfn: supern: {
                haskellPackages = supern.haskellPackages.override {
                  overrides = selfh: superh:
                    lib.genAttrs packageNames
                      (elynx-create-package selfh.callCabal2nix);
                };
              }
            );
            overlays = [ elynx-overlay ];
            pkgs = import nixpkgs { inherit system overlays; };
            elynx = lib.genAttrs packageNames (n: pkgs.haskellPackages.${n});
            elynx-dev = builtins.mapAttrs (
              _: x: pkgs.haskell.lib.overrideCabal x (
                _: { doBenchmark = true; }
              )
            ) elynx;
            elynx-suite = pkgs.buildEnv {
              name = "ELynx suite";
              paths = builtins.attrValues elynx;
            };
          in
            {
              packages = elynx // { inherit elynx-suite; };

              defaultPackage = elynx-suite;

              devShell = pkgs.haskellPackages.shellFor {
                packages = _: (builtins.attrValues elynx-dev);
                buildInputs = with pkgs; [
                  bashInteractive
                  haskellPackages.cabal-install
                  haskellPackages.haskell-language-server
                  haskellPackages.stack
                ];
                doBenchmark = true;
                withHoogle = true;
              };
            }
      );
}
