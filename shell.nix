{ghc}:

with (import <nixpkgs> {config.allowUnfree = true;});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "stackEnv";
  buildInputs = [
    gfortran
    glpk
    gsl
    llvm_9
    mkl
    pkg-config
    sundials
    zlib
  ];
}
