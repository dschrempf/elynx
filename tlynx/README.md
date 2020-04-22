

# The ELynx Suite

Version: 0.1.0.
Reproducible evolution made easy.

The ELynx Suite is a Haskell library and a tool set for computational biology.
The goal of the ELynx Suite is reproducible research. Evolutionary sequences and
phylogenetic trees can be read, viewed, modified and simulated. Exact
specification of all options is necessary, and nothing is assumed about the data
(e.g., the type of the genetic code). The command line with all arguments is
consistently, and automatically logged. The work overhead in the beginning
usually pays off in the end.

The Elynx Suite consists of four library packages and two executables providing
a range of sub commands.

The library packages are:

-   **elynx-markov:** Simulate multi sequence alignments along phylogenetic trees.
-   **elynx-seq:** Handle evolutionary sequences and multi sequence alignments.
-   **elynx-tools:** Tools for the provided executables.
-   **elynx-tree:** Handle phylogenetic trees.

The executables are:

-   **slynx:** Analyze, modify, and simulate evolutionary sequences (FASTA format).
-   **tlynx:** Analyze, modify, and simulate phylogenetic trees (Newick format).

**ELynx is actively developed. We happily receive comments, ideas, feature
requests, and pull requests!**


# Installation

ELynx is written in [Haskell](https://www.haskell.org/) and can be installed with [Stack](https://docs.haskellstack.org/en/stable/README/).

1.  Install Stack with your package manager, or directly from the web
    page.
    
        curl -sSL https://get.haskellstack.org/ | sh

2.  Clone the ELynx repository.
    
        git clone clone https://github.com/dschrempf/elynx

3.  Navigate to the newly created `elynx` folder and build the binaries.
    This will take a while.
    
        stack build

4.  Run a binary from within the project directory. For example,
    
        stack exec tlynx -- --help

5.  If needed, install the binaries.
    
        stack install
    
    The binaries are installed into `~/.local/bin/` which has to be added to the
    [PATH](https://en.wikipedia.org/wiki/PATH_(variable)) environment variable. Then, they can be used directly.


# SLynx

Handle evolutionary sequences.

    slynx --help

    ELynx Suite version 0.1.0. Developed by Dominik Schrempf. Compiled on March 21,
    2020, at 16:37 pm, UTC.
    
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
      examine                  Examine sequences. If data is a multi sequence
                               alignment, additionally analyze columns.
      filter-columns           Filter columns of multi sequence alignments.
      filter-rows              Filter rows (or sequences) found in input files.
      simulate                 Simulate multi sequence alignments.
      sub-sample               Sub-sample columns from multi sequence alignments.
      translate                Translate from DNA to Protein or DNAX to ProteinX.
    
    File formats:
      - FASTA
    
    Alphabet types:
      - DNA (nucleotides)
      - DNAX (nucleotides; including gaps)
      - DNAI (nucleotides; including gaps, and IUPAC codes)
      - Protein (amino acids)
      - ProteinX (amino acids; including gaps)
      - ProteinS (amino acids; including gaps, and translation stops)
      - ProteinI (amino acids; including gaps, translation stops, and IUPAC codes)
    
    The ELynx Suite
    ---------------
    A Haskell library and a tool set for computational biology. The goal of the
    ELynx Suite is reproducible research. Evolutionary sequences and phylogenetic
    trees can be read, viewed, modified and simulated. Exact specification of all
    options is necessary, and nothing is assumed about the data (e.g., the type of
    code). The command line with all arguments is consistently, and automatically
    logged.
    
    slynx     Analyze, modify, and simulate evolutionary sequences.
    tlynx     Analyze, modify, and simulate phylogenetic trees.
    
    Get help for specific commands:
      slynx examine --help


## Concatenate

Concatenate multi sequence alignments.

    slynx concatenate --help

    Usage: slynx concatenate (-a|--alphabet NAME) INPUT-FILE
      Concatenate sequences found in input files.
    
    Available options:
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      -h,--help                Show this help text


## Examine

Examine sequence with `slynx examine`.

    slynx examine --help

    Usage: slynx examine (-a|--alphabet NAME) [INPUT-FILE] [--per-site]
      Examine sequences. If data is a multi sequence alignment, additionally analyze
      columns.
    
    Available options:
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      --per-site               Report per site summary statistics
      -h,--help                Show this help text


## Filter

Filter sequences with `filer-rows`.

    slynx filter-rows --help

    Usage: slynx filter-rows (-a|--alphabet NAME) [INPUT-FILE]
                             [--longer-than LENGTH] [--shorter-than LENGTH]
                             [--standard-characters]
      Filter rows (or sequences) found in input files.
    
    Available options:
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      --longer-than LENGTH     Only keep sequences longer than LENGTH
      --shorter-than LENGTH    Only keep sequences shorter than LENGTH
      --standard-characters    Only keep sequences containing at least one standard
                               (i.e., non-IUPAC) character
      -h,--help                Show this help text

Filter columns of multi sequence alignments with `filter-columns`.

    slynx filter-columns --help

    Usage: slynx filter-columns (-a|--alphabet NAME) [INPUT-FILE]
                                [--standard-chars DOUBLE]
      Filter columns of multi-sequence alignments.
    
    Available options:
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      --standard-chars DOUBLE  Keep columns with a proportion standard (non-IUPAC)
                               characters larger than DOUBLE in [0,1]
      -h,--help                Show this help text


## Simulate

Simulate sequences with `slynx simulate`.

    slynx simulate --help

    Usage: slynx simulate (-t|--tree-file Name) [-s|--substitution-model MODEL]
                          [-m|--mixture-model MODEL] [-e|--edm-file NAME]
                          [-p|--siteprofile-files NAMES]
                          [-w|--mixture-model-weights "[DOUBLE,DOUBLE,...]"]
                          [-g|--gamma-rate-heterogeneity "(NCAT,SHAPE)"]
                          (-l|--length NUMBER) [-S|--seed [INT]]
      Simulate multi sequence alignments.
    
    Available options:
      -t,--tree-file Name      Read trees from file NAME
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
       Supported DNA models: JC, HKY.
         For example,
           -s HKY[KAPPA]{DOUBLE,DOUBLE,DOUBLE,DOUBLE}
       Supported Protein models: Poisson, Poisson-Custom, LG, LG-Custom, WAG, WAG-Custom.
         MODEL-Custom means that only the exchangeabilities of MODEL are used,
         and a custom stationary distribution is provided.
         For example,
           -s LG-Custom{...}
    
    Mixture models:
    -m "MIXTURE(SUBSTITUTION_MODEL_1,SUBSTITUTION_MODEL_2)"
       For example,
         -m "MIXTURE(JC,HKY[6.0]{0.3,0.2,0.2,0.3})"
    Mixture weights have to be provided with the -w option.
    
    Special mixture models:
    -m CXX
       where XX is 10, 20, 30, 40, 50, or 60; CXX models, Quang et al., 2008.
    -m "EDM(EXCHANGEABILITIES)"
       Arbitrary empirical distribution mixture (EDM) models.
       Stationary distributions have to be provided with the -e option.
       For example,
         LG exchangeabilities with stationary distributions given in FILE.
         -m "EDM(LG-Custom)" -e FILE
    For special mixture models, mixture weights are optional.


## Sub-sample

Sub-sample columns from multi sequence alignments.

    slynx sub-sample --help

    Usage: slynx sub-sample (-a|--alphabet NAME) [INPUT-FILE]
                            (-n|--number-of-sites INT)
                            (-m|--number-of-alignments INT) [-S|--seed [INT]]
      Sub-sample columns from multi sequence alignments.
    
    Available options:
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      -n,--number-of-sites INT Number of sites randomly drawn with replacement
      -m,--number-of-alignments INT
                               Number of multi sequence alignments to be created
      -S,--seed [INT]          Seed for random number generator; list of 32 bit
                               integers with up to 256 elements (default: random)
      -h,--help                Show this help text
    
    Create a given number of multi sequence alignments, each of which contains a
    given number of random sites drawn from the original multi sequence alignment.


## Translate

Translate sequences.

    slynx translate --help

    Usage: slynx translate (-a|--alphabet NAME) [INPUT-FILE]
                           (-r|--reading-frame INT) (-u|--universal-code CODE)
      Translate from DNA to Protein or DNAX to ProteinX.
    
    Available options:
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      -r,--reading-frame INT   Reading frame [0|1|2].
      -u,--universal-code CODE universal code; one of: Standard,
                               VertebrateMitochondrial.
      -h,--help                Show this help text


# TLynx

Handle phylogenetic trees in Newick format.

    tlynx --help

    ELynx Suite version 0.1.0. Developed by Dominik Schrempf. Compiled on March 21,
    2020, at 16:37 pm, UTC.
    
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
      coalesce                 Simulate phylogenetic trees using the coalescent
                               processes (see also the 'simulate' command for
                               simulations using the birth and death process).
      compare                  Compare two phylogenetic trees (compute distances and
                               branch-wise differences).
      connect                  Connect two phylogenetic trees in all ways (possibly
                               honoring constraints).
      distance                 Compute distances between many phylogenetic trees.
      examine                  Compute summary statistics of phylogenetic trees.
      shuffle                  Shuffle a phylogenetic tree (keep coalescent times,
                               but shuffle topology and leaves).
      simulate                 Simulate phylogenetic trees using birth and death
                               processes (see also the 'coalesce' command for
                               simulations using the coalescent process).
    
    File formats:
      - Newick
    
    The ELynx Suite
    ---------------
    A Haskell library and a tool set for computational biology. The goal of the
    ELynx Suite is reproducible research. Evolutionary sequences and phylogenetic
    trees can be read, viewed, modified and simulated. Exact specification of all
    options is necessary, and nothing is assumed about the data (e.g., the type of
    code). The command line with all arguments is consistently, and automatically
    logged.
    
    slynx     Analyze, modify, and simulate evolutionary sequences.
    tlynx     Analyze, modify, and simulate phylogenetic trees.
    
    Get help for specific commands:
      slynx examine --help


## Compare

Compute distances between phylogenetic trees.

    tlynx compare --help

    Usage: tlynx compare [-n|--normalize] [-b|--bipartitions] [-i|--newick-iqtree]
                         NAME
      Compare two phylogenetic trees (compute distances and branch-wise
      differences).
    
    Available options:
      -n,--normalize           Normalize trees before comparison
      -b,--bipartitions        Print common and missing bipartitions
      -i,--newick-iqtree       Use IQ-TREE Newick format (internal node labels are
                               branch support values)
      NAME                     Tree file
      -h,--help                Show this help text


## Examine

Compute summary statistics of phylogenetic trees.

    tlynx examine --help

    Usage: tlynx examine [INPUT-FILE] [-i|--newick-iqtree]
      Compute summary statistics of phylogenetic trees.
    
    Available options:
      INPUT-FILE               Read trees from INPUT-FILE
      -i,--newick-iqtree       Use IQ-TREE Newick format (internal node labels are
                               branch support values)
      -h,--help                Show this help text


## Simulate

Simulate phylogenetic trees using birth and death processes.

    tlynx simulate --help

    Usage: tlynx simulate [-t|--nTrees INT] [-n|--nLeaves INT] [-H|--height DOUBLE]
                          [-M|--condition-on-mrca] [-l|--lambda DOUBLE]
                          [-m|--mu DOUBLE] [-r|--rho DOUBLE] [-u|--sub-sample]
                          [-s|--summary-statistics] [-S|--seed [INT]]
      Simulate phylogenetic trees using birth and death processes.
    
    Available options:
      -t,--nTrees INT          Number of trees (default: 10)
      -n,--nLeaves INT         Number of leaves per tree (default: 5)
      -H,--height DOUBLE       Fix tree height (no default)
      -M,--condition-on-mrca   Do not condition on height of origin but on height of
                               MRCA
      -l,--lambda DOUBLE       Birth rate lambda (default: 1.0)
      -m,--mu DOUBLE           Death rate mu (default: 0.9)
      -r,--rho DOUBLE          Sampling probability rho (default: 1.0)
      -u,--sub-sample          Perform sub-sampling; see below.
      -s,--summary-statistics  Only output number of children for each branch
      -S,--seed [INT]          Seed for random number generator; list of 32 bit
                               integers with up to 256 elements (default: random)
      -h,--help                Show this help text
    
    Height of Trees: if no tree height is given, the heights will be randomly drawn from the expected distribution given the number of leaves, the birth and the death rate.
    Summary statistics only: only print (NumberOfExtantChildren BranchLength) pairs for each branch of each tree. The trees are separated by a newline character.
    Sub-sampling: simulate one big tree with n'=round(n/rho), n'>=n, leaves, and randomly sample sub-trees with n leaves. Hence, with rho=1.0, the same tree is reported over and over again.
    Gernhard, T. (2008). The conditioned reconstructed process. Journal of Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005


# Library documentation

Documentation of the libraries can be found on [Hackage](https://hackage.haskell.org/):

-   [elynx-markov](https://hackage.haskell.org/package/elynx-markov)
-   [elynx-seq](https://hackage.haskell.org/package/elynx-seq)
-   [elynx-tools](https://hackage.haskell.org/package/elynx-tools)
-   [elynx-tree](https://hackage.haskell.org/package/elynx-tree)

Documentation of the executables is, or course, also available:

-   [slynx](https://hackage.haskell.org/package/slynx)
-   [tlynx](https://hackage.haskell.org/package/tlynx)

