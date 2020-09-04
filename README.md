

# The ELynx Suite

Version: 0.4.0.
Reproducible evolution made easy.

<p align="center"><img src="https://travis-ci.org/dschrempf/elynx.svg?branch=master"/></p>

A Haskell library and tool set for computational biology. The goal of ELynx is
reproducible research. Evolutionary sequences and phylogenetic trees can be
read, viewed, modified and simulated. The command line with all arguments is
logged consistently, and automatically. Data integrity is verified using SHA256
sums so that validation of past analyses is possible without the need to
recompute the result.

The Elynx Suite consists of library packages and executables providing a range
of sub commands.

The library packages are:

-   **elynx-nexus:** Nexus file support.
-   **elynx-markov:** Simulate multi sequence alignments along phylogenetic trees.
-   **elynx-seq:** Handle evolutionary sequences and multi sequence alignments.
-   **elynx-tools:** Tools for the provided executables.
-   **elynx-tree:** Handle phylogenetic trees.

The executables are:

-   **slynx:** Analyze, modify, and simulate evolutionary sequences.
-   **tlynx:** Analyze, modify, and simulate phylogenetic trees.
-   **elynx:** Validate and redo past analyses.

**ELynx is actively developed. We happily receive comments, ideas, feature
requests, and pull requests!**


# Installation

ELynx is written in [Haskell](https://www.haskell.org/) and can be installed with [Stack](https://docs.haskellstack.org/en/stable/README/).

1.  Install Stack with your package manager, or directly from the web
    page.
    
        curl -sSL https://get.haskellstack.org/ | sh

2.  Clone the ELynx repository.
    
        git clone https://github.com/dschrempf/elynx

3.  Navigate to the newly created `elynx` folder and build the binaries.
    This will take a while.
    
        stack build

4.  Run a binary from within the project directory. For example,
    
        stack exec tlynx -- --help

5.  If needed, install the binaries.
    
        stack install
    
    The binaries are installed into `~/.local/bin/` which has to be added to the
    [PATH](https://en.wikipedia.org/wiki/PATH_(variable)) environment variable. Then, they can be used directly.


# Documentation

Documentation is available on [Hackage](https://hackage.haskell.org/).

Libraries:

-   [elynx-nexus](https://hackage.haskell.org/package/elynx-nexus)
-   [elynx-markov](https://hackage.haskell.org/package/elynx-markov)
-   [elynx-seq](https://hackage.haskell.org/package/elynx-seq)
-   [elynx-tools](https://hackage.haskell.org/package/elynx-tools)
-   [elynx-tree](https://hackage.haskell.org/package/elynx-tree)

Executables:

-   [elynx](https://hackage.haskell.org/package/elynx)
-   [slynx](https://hackage.haskell.org/package/slynx)
-   [tlynx](https://hackage.haskell.org/package/tlynx)


# SLynx

Handle evolutionary sequences.

    slynx --help | head -n -16

    ELynx Suite version 0.4.0.
    Developed by Dominik Schrempf.
    Compiled on September 4, 2020, at 13:37 pm, UTC.
    
    Usage: slynx [-v|--verbosity VALUE] [-o|--output-file-basename NAME] 
                 [-f|--force] COMMAND
      Analyze, and simulate multi sequence alignments.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -v,--verbosity VALUE     Be verbose; one of: Quiet Warning Info
                               Debug (default: Info)
      -o,--output-file-basename NAME
                               Specify base name of output file
      -f,--force               Ignore previous analysis and overwrite existing
                               output files.
    
    Available commands:
      concatenate              Concatenate sequences found in input files.
      examine                  Examine sequences. If data is a multi sequence alignment, additionally analyze columns.
      filter-columns           Filter columns of multi sequence alignments.
      filter-rows              Filter rows (or sequences) found in input files.
      simulate                 Simulate multi sequence alignments.
      sub-sample               Sub-sample columns from multi sequence alignments.
      translate                Translate from DNA to Protein or DNAX to ProteinX.
    
    
    Available sequence file formats:
      - FASTA
    
    Available alphabets:
      - DNA (nucleotides)
      - DNAX (nucleotides; including gaps)
      - DNAI (nucleotides; including gaps, and IUPAC codes)
      - Protein (amino acids)
      - ProteinX (amino acids; including gaps)
      - ProteinS (amino acids; including gaps, and translation stops)


## Concatenate

Concatenate multi sequence alignments.

    slynx concatenate --help

    ELynx Suite version 0.4.0.
    Developed by Dominik Schrempf.
    Compiled on September 4, 2020, at 13:37 pm, UTC.
    
    Usage: slynx concatenate (-a|--alphabet NAME) INPUT-FILE
      Concatenate sequences found in input files.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      -h,--help                Show this help text


## Examine

Examine sequence with `slynx examine`.

    slynx examine --help

    ELynx Suite version 0.4.0.
    Developed by Dominik Schrempf.
    Compiled on September 4, 2020, at 13:37 pm, UTC.
    
    Usage: slynx examine (-a|--alphabet NAME) INPUT-FILE [--per-site]
      Examine sequences. If data is a multi sequence alignment, additionally analyze columns.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      --per-site               Report per site summary statistics
      -h,--help                Show this help text


## Filter

Filter sequences with `filer-rows`.

    slynx filter-rows --help

    ELynx Suite version 0.4.0.
    Developed by Dominik Schrempf.
    Compiled on September 4, 2020, at 13:37 pm, UTC.
    
    Usage: slynx filter-rows (-a|--alphabet NAME) INPUT-FILE [--longer-than LENGTH] 
                             [--shorter-than LENGTH] [--standard-characters]
      Filter rows (or sequences) found in input files.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      --longer-than LENGTH     Only keep sequences longer than LENGTH
      --shorter-than LENGTH    Only keep sequences shorter than LENGTH
      --standard-characters    Only keep sequences containing at least one standard
                               (i.e., non-IUPAC) character
      -h,--help                Show this help text

Filter columns of multi sequence alignments with `filter-columns`.

    slynx filter-columns --help

    ELynx Suite version 0.4.0.
    Developed by Dominik Schrempf.
    Compiled on September 4, 2020, at 13:37 pm, UTC.
    
    Usage: slynx filter-columns (-a|--alphabet NAME) INPUT-FILE 
                                [--standard-chars DOUBLE]
      Filter columns of multi sequence alignments.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      --standard-chars DOUBLE  Keep columns with a proportion standard (non-IUPAC)
                               characters larger than DOUBLE in [0,1]
      -h,--help                Show this help text


## Simulate

Simulate sequences with `slynx simulate`.

    slynx simulate --help

    ELynx Suite version 0.4.0.
    Developed by Dominik Schrempf.
    Compiled on September 4, 2020, at 13:37 pm, UTC.
    
    Usage: slynx simulate (-t|--tree-file Name) [-s|--substitution-model MODEL] 
                          [-m|--mixture-model MODEL] [-e|--edm-file NAME] 
                          [-p|--siteprofile-files NAMES] 
                          [-w|--mixture-model-weights "[DOUBLE,DOUBLE,...]"] 
                          [-g|--gamma-rate-heterogeneity "(NCAT,SHAPE)"]
                          (-l|--length NUMBER) [-S|--seed [INT]]
      Simulate multi sequence alignments.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -t,--tree-file Name      Read tree from Newick file NAME
      -s,--substitution-model MODEL
                               Set the phylogenetic substitution model; available
                               models are shown below (mutually exclusive with -m
                               option)
      -m,--mixture-model MODEL Set the phylogenetic mixture model; available models
                               are shown below (mutually exclusive with -s option)
      -e,--edm-file NAME       Empirical distribution model file NAME in Phylobayes
                               format
      -p,--siteprofile-files NAMES
                               File names of site profiles in Phylobayes format
      -w,--mixture-model-weights "[DOUBLE,DOUBLE,...]"
                               Weights of mixture model components
      -g,--gamma-rate-heterogeneity "(NCAT,SHAPE)"
                               Number of gamma rate categories and shape parameter
      -l,--length NUMBER       Set alignment length to NUMBER
      -S,--seed [INT]          Seed for random number generator; list of 32 bit
                               integers with up to 256 elements (default: random)
      -h,--help                Show this help text
    
    Substitution models:
    -s "MODEL[PARAMETER,PARAMETER,...]{STATIONARY_DISTRIBUTION}"
       Supported DNA models: JC, F81, HKY, GTR4.
         For example,
           -s HKY[KAPPA]{DOUBLE,DOUBLE,DOUBLE,DOUBLE}
           -s GTR4[e_AC,e_AG,e_AT,e_CG,e_CT,e_GT]{DOUBLE,DOUBLE,DOUBLE,DOUBLE}
              where the 'e_XY' are the exchangeabilities from nucleotide X to Y.
       Supported Protein models: Poisson, Poisson-Custom, LG, LG-Custom, WAG, WAG-Custom, GTR20.
         MODEL-Custom means that only the exchangeabilities of MODEL are used,
         and a custom stationary distribution is provided.
         For example,
           -s LG
           -s LG-Custom{...}
           -s GTR20[e_AR,e_AN,...]{...}
              the 'e_XY' are the exchangeabilities from amino acid X to Y (alphabetical order).
       Notes: The F81 model for DNA is equivalent to the Poisson-Custom for proteins.
              The GTR4 model for DNA is equivalent to the GTR20 for proteins.
    
    Mixture models:
    -m "MIXTURE(SUBSTITUTION_MODEL_1,SUBSTITUTION_MODEL_2[PARAMETERS]{STATIONARY_DISTRIBUTION},...)"
       For example,
         -m "MIXTURE(JC,HKY[6.0]{0.3,0.2,0.2,0.3})"
    Mixture weights have to be provided with the -w option.
    
    Special mixture models:
    -m CXX
       where XX is 10, 20, 30, 40, 50, or 60; CXX models, Quang et al., 2008.
    -m "EDM(EXCHANGEABILITIES)"
       Arbitrary empirical distribution mixture (EDM) models.
       Stationary distributions have to be provided with the -e or -p option.
       For example,
         LG exchangeabilities with stationary distributions given in FILE.
         -m "EDM(LG-Custom)" -e FILE
         LG exchangeabilities with site profiles (Phylobayes) given in FILES.
         -m "EDM(LG-Custom)" -p FILES
    For special mixture models, mixture weights are optional.


## Sub-sample

Sub-sample columns from multi sequence alignments.

    slynx sub-sample --help

    ELynx Suite version 0.4.0.
    Developed by Dominik Schrempf.
    Compiled on September 4, 2020, at 13:37 pm, UTC.
    
    Usage: slynx sub-sample (-a|--alphabet NAME) INPUT-FILE
                            (-n|--number-of-sites INT)
                            (-m|--number-of-alignments INT) [-S|--seed [INT]]
      Sub-sample columns from multi sequence alignments.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      -n,--number-of-sites INT Number of sites randomly drawn with replacement
      -m,--number-of-alignments INT
                               Number of multi sequence alignments to be created
      -S,--seed [INT]          Seed for random number generator; list of 32 bit
                               integers with up to 256 elements (default: random)
      -h,--help                Show this help text
    
    Create a given number of multi sequence alignments, each of which contains a given number of random sites drawn from the original multi sequence alignment.


## Translate

Translate sequences.

    slynx translate --help

    ELynx Suite version 0.4.0.
    Developed by Dominik Schrempf.
    Compiled on September 4, 2020, at 13:37 pm, UTC.
    
    Usage: slynx translate (-a|--alphabet NAME) INPUT-FILE (-r|--reading-frame INT)
                           (-u|--universal-code CODE)
      Translate from DNA to Protein or DNAX to ProteinX.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      -r,--reading-frame INT   Reading frame [0|1|2].
      -u,--universal-code CODE universal code; one of: Standard,
                               VertebrateMitochondrial.
      -h,--help                Show this help text


# TLynx

Handle phylogenetic trees in Newick format.

    tlynx --help | head -n -16

    ELynx Suite version 0.4.0.
    Developed by Dominik Schrempf.
    Compiled on September 4, 2020, at 13:37 pm, UTC.
    
    Usage: tlynx [-v|--verbosity VALUE] [-o|--output-file-basename NAME] 
                 [-f|--force] COMMAND
      Compare, examine, and simulate phylogenetic trees.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -v,--verbosity VALUE     Be verbose; one of: Quiet Warning Info
                               Debug (default: Info)
      -o,--output-file-basename NAME
                               Specify base name of output file
      -f,--force               Ignore previous analysis and overwrite existing
                               output files.
    
    Available commands:
      compare                  Compare two phylogenetic trees (compute distances and branch-wise differences).
      connect                  Connect two phylogenetic trees in all ways (possibly honoring constraints).
      distance                 Compute distances between many phylogenetic trees.
      examine                  Compute summary statistics of phylogenetic trees.
      shuffle                  Shuffle a phylogenetic tree (keep coalescent times, but shuffle topology and leaves).
      simulate                 Simulate phylogenetic trees using a birth and death or coalescent process.
    
    
    Available tree file formats:
      - Newick Standard: Branch support values are stored in square brackets after branch lengths.
      - Newick IqTree:   Branch support values are stored as node names after the closing bracket of forests.


## Compare

Compute distances between phylogenetic trees.

    tlynx compare --help

    ELynx Suite version 0.4.0.
    Developed by Dominik Schrempf.
    Compiled on September 4, 2020, at 13:37 pm, UTC.
    
    Usage: tlynx compare [-n|--normalize] [-b|--bipartitions] [-t|--intersect] 
                         [-f|--newick-format FORMAT] NAMES
      Compare two phylogenetic trees (compute distances and branch-wise differences).
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -n,--normalize           Normalize trees before comparison
      -b,--bipartitions        Print and plot common and missing bipartitions
      -t,--intersect           Compare intersections; i.e., before comparison, drop
                               leaves that are not present in the other tree
      -f,--newick-format FORMAT
                               Newick tree format: Standard, IqTree, or RevBayes;
                               default: Standard; for detailed help, see 'tlynx
                               --help'
      NAMES                    Tree files
      -h,--help                Show this help text


## Examine

Compute summary statistics of phylogenetic trees.

    tlynx examine --help

    ELynx Suite version 0.4.0.
    Developed by Dominik Schrempf.
    Compiled on September 4, 2020, at 13:37 pm, UTC.
    
    Usage: tlynx examine INPUT-FILE [-f|--newick-format FORMAT]
      Compute summary statistics of phylogenetic trees.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      INPUT-FILE               Read trees from INPUT-FILE
      -f,--newick-format FORMAT
                               Newick tree format: Standard, IqTree, or RevBayes;
                               default: Standard; for detailed help, see 'tlynx
                               --help'
      -h,--help                Show this help text


## Simulate

Simulate phylogenetic trees using birth and death processes.

    tlynx simulate --help

    ELynx Suite version 0.4.0.
    Developed by Dominik Schrempf.
    Compiled on September 4, 2020, at 13:37 pm, UTC.
    
    Usage: tlynx simulate (-t|--nTrees INT) (-n|--nLeaves INT) PROCESS 
                          [-u|--sub-sample DOUBLE] [-s|--summary-statistics] 
                          [-S|--seed [INT]]
      Simulate phylogenetic trees using a birth and death or coalescent process.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -t,--nTrees INT          Number of trees
      -n,--nLeaves INT         Number of leaves per tree
      -u,--sub-sample DOUBLE   Perform sub-sampling; see below.
      -s,--summary-statistics  For each branch, print length and number of children
      -S,--seed [INT]          Seed for random number generator; list of 32 bit
                               integers with up to 256 elements (default: random)
      -h,--help                Show this help text
    
    Available processes:
      birthdeath               Birth and death process
      coalescent               Coalescent process
    
    See, for example, 'tlynx simulate birthdeath --help'.
    Sub-sample with probability p:
      1. Simulate one big tree with n'=round(n/p), n'>=n, leaves;
      2. Randomly sample sub-trees with n leaves.
      (With p=1.0, the same tree is reported over and over again.)


# ELynx

Validate and (optionally) redo past ELynx analyses.

    elynx --help | head -n -16

    ELynx Suite version 0.4.0.
    Developed by Dominik Schrempf.
    Compiled on September 4, 2020, at 13:37 pm, UTC.
    
    Usage: elynx COMMAND
      Validate and redo past ELynx analyses
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
    
    Available commands:
      validate                 Validate an ELynx analysis
      redo                     Redo an ELynx analysis

