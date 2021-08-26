{
  description = "ELynx";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, flake-utils, nixpkgs }:
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
          elynx = lib.genAttrs packageNames (
            # This should be removed but it does not work (see below).
            n: pkgs.haskell.lib.doBenchmark pkgs.haskellPackages.${n}
          );
        in
          {
            packages = elynx;

            defaultPackage = elynx.elynx;

            devShell = pkgs.haskellPackages.shellFor {
              packages = p: map (n: p.${n}) packageNames;
              buildInputs = with pkgs.haskellPackages; [
                haskell-language-server
                cabal-install
              ];
              # TODO.
              # Somehow this flag does not work.
              doBenchmark = true;
            };
          }
    );
}
