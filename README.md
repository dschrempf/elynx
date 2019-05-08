# Reproducible evolution made easy

EvoMod is a Haskell library and a tool set for computational biology.

Available binaries:

- `seq-ana`: View, examine, and modify evolutionary sequences (FASTA format).
- `seq-sim`: Simulate evolutionary sequences (FASTA format).
- `tree-sim`: Simulate phylogenetic trees (Newick format).

# Installation
EvoMod is written in [Haskell](https://www.haskell.org/) and can be installed
with [Stack](https://docs.haskellstack.org/en/stable/README/).

1. Install Stack with your package manager, or directly from the web page.

   ```
   curl -sSL https://get.haskellstack.org/ | sh
   ```

2. Clone the EvoMod repository.
```
git clone clone https://github.com/dschrempf/evomod
```
    
3. Navigate to the newly created `evomod` folder and build the binaries.
   This will take a while.
```
stack build
```

4. Run a binary from within the project directory. For example,
```
stack exec tree-sim -- --help
```

5. If needed, install the binaries.
```
stack install
```
The binaries are installed into `~/.local/bin/` which has to be added
[PATH](https://en.wikipedia.org/wiki/PATH_(variable)). Now, the binary can be
directly used.
