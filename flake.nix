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
    let
      theseHpkgNames = [
        "elynx"
        "elynx-markov"
        "elynx-nexus"
        "elynx-seq"
        "elynx-tools"
        "elynx-tree"
        "slynx"
        "tlynx"
      ];
      thisGhcVersion = "ghc943";
      hMkPackage = h: n: h.callCabal2nix n (./. + "/${n}") { };
      hOverlay = selfn: supern: {
        haskell = supern.haskell // {
          packageOverrides = selfh: superh:
            supern.haskell.packageOverrides selfh superh //
              nixpkgs.lib.genAttrs theseHpkgNames (hMkPackage selfh);
        };
      };
      perSystem = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ hOverlay ];
          };
          hpkgs = pkgs.haskell.packages.${thisGhcVersion};
          hlib = pkgs.haskell.lib;
          theseHpkgsNoCompletion = nixpkgs.lib.genAttrs theseHpkgNames (n: hpkgs.${n});
          # Add Bash completion.
          theseHpkgs =
            let
              f = pkgs.haskell.lib.generateOptparseApplicativeCompletion;
              slynxCompletion = f "slynx" theseHpkgsNoCompletion.slynx;
              tlynxCompletion = f "tlynx" theseHpkgsNoCompletion.tlynx;
              elynxCompletion = f "elynx" theseHpkgsNoCompletion.elynx;
            in
            theseHpkgsNoCompletion // {
              slynx = slynxCompletion;
              tlynx = tlynxCompletion;
              elynx = elynxCompletion;
            };
          theseHpkgsDev = builtins.mapAttrs (_: x: hlib.doBenchmark x) theseHpkgs;
          # Environment including all packages.
          elynxSuite = pkgs.buildEnv {
            name = "ELynx suite";
            paths = builtins.attrValues theseHpkgs;
          };
        in
        {
          packages = theseHpkgs // { inherit elynxSuite; default = elynxSuite; };

          devShells.default = hpkgs.shellFor {
            # shellHook =
            #   let
            #     scripts = ./scripts;
            #   in
            #   ''
            #     export PATH="${scripts}:$PATH"
            #   '';
            packages = _: (builtins.attrValues theseHpkgsDev);
            nativeBuildInputs = with pkgs; [
              # See https://github.com/NixOS/nixpkgs/issues/59209.
              bashInteractive

              # Haskell toolchain.
              hpkgs.cabal-fmt
              hpkgs.cabal-install
              hpkgs.haskell-language-server
            ];
            buildInputs = with pkgs; [
            ];
            doBenchmark = true;
            # withHoogle = true;
          };
        };
    in
    { overlays.default = hOverlay; } // flake-utils.lib.eachDefaultSystem perSystem;
}
