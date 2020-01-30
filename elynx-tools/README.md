

# The ELynx Suite

Version: 0.1.0.
Reproducible evolution made easy.

The ELynx Suite is a Haskell library and a tool set for computational biology.
The goal of the ELynx Suite is reproducible research. Evolutionary sequences and
phylogenetic trees can be read, viewed, modified and simulated. Exact
specification of all options is necessary, and nothing is assumed about the data
(e.g., the type of code). The command line with all arguments is consistently,
and automatically logged. The work overhead in the beginning usually pays off in
the end.

The Elynx Suite consists of three library packages and two executables providing
a range of sub commands.

The library packages are:

-   **elynx-seq:** Handle evolutionary sequences and multi sequence alignments;
-   **elynx-tree:** Handle phylogenetic trees;
-   **elynx-tools:** Tools for the provided executables;

The executables are:

-   **SLynx:** Analyze, modify, and simulate evolutionary sequences (FASTA format);
-   **TLynx:** Analyze, modify, and simulate phylogenetic trees (Newick format).

**ELynx is still under development. We happily receive comments, ideas, feature
requests, or pull requests!**


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
    
    The binaries are installed into `~/.local/bin/` which has to be added [PATH](https://en.wikipedia.org/wiki/PATH_(variable)).
    Then, they can be used directly.


# SLynx

Handle evolutionary sequences.

    slynx --help

    ELynx Suite version 0.5.1. Developed by Dominik Schrempf. Compiled on September
    9, 2019, at 10:48 am, UTC.
    
    Usage: slynx [-v|--verbosity VALUE] [-o|--output-file-basename NAME] COMMAND
      Analyze, and simulate multi sequence alignments.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -v,--verbosity VALUE     Be verbose; one of: Quiet Warning Info
                               Debug (default: Info)
      -o,--output-file-basename NAME
                               Specify base name of output file
    
    Available commands:
      concatenate              
      examine                  If data is a multi sequence alignment, additionally
                               analyze columns.
      filter-rows              
      filter-columns           
      simulate                 
      sub-sample               Create a given number of multi sequence alignments,
                               each of which containing a given number of random
                               sites drawn from the original multi sequence
                               alignment.
      translate                
    
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
    
    The ELynx Suite.
    A Haskell library and a tool set for computational biology. The goal of the
    ELynx Suite is reproducible research. Evolutionary sequences and phylogenetic
    trees can be read, viewed, modified and simulated. Exact specification of all
    options is necessary, and nothing is assumed about the data (e.g., the type of
    code). The command line with all arguments is consistently, and automatically
    logged. The work overhead in the beginning usually pays off in the end.
    slynx     Analyze, modify, and simulate evolutionary sequences.
    tlynx     Analyze, modify, and simulate phylogenetic trees.


## Concatenate

Concatenate multi sequence alignments.

    slynx concatenate --help

    Concatenate sequences found in input files.
    
    Usage: slynx concatenate (-a|--alphabet NAME) INPUT-FILE
    
    Available options:
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      -h,--help                Show this help text


## Examine

Examine sequence with `slynx examine`.

    slynx examine --help

    Examine sequences.
    
    Usage: slynx examine (-a|--alphabet NAME) [INPUT-FILE] [--per-site]
      If data is a multi sequence alignment, additionally analyze columns.
    
    Available options:
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      --per-site               Report per site summary statistics
      -h,--help                Show this help text


## Filter

Filter sequences with `filer-rows`.

    slynx filter-rows --help

    Filter rows (or sequences) found in input files.
    
    Usage: slynx filter-rows (-a|--alphabet NAME) [INPUT-FILE]
                             [--longer-than LENGTH] [--shorter-than LENGTH]
    
    Available options:
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      --longer-than LENGTH     Only keep sequences longer than LENGTH
      --shorter-than LENGTH    Only keep sequences shorter than LENGTH
      -h,--help                Show this help text

Filter columns of multi sequence alignments with `filter-columns`.

    slynx filter-columns --help

    Filter columns of multi-sequence alignments.
    
    Usage: slynx filter-columns (-a|--alphabet NAME) [INPUT-FILE]
                                [--standard-chars DOUBLE]
    
    Available options:
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      --standard-chars DOUBLE  Keep columns with a proportion standard (non-IUPAC)
                               characters larger than DOUBLE in [0,1]
      -h,--help                Show this help text


## Simulate

Simulate sequences with `slynx simulate`.

    slynx simulate --help

    Simulate multi sequence alignments.
    
    Usage: slynx simulate (-t|--tree-file Name) [-s|--substitution-model MODEL]
                          [-m|--mixture-model MODEL] [-e|--edm-file NAME]
                          [-w|--mixture-model-weights "[DOUBLE,DOUBLE,...]"]
                          [-g|--gamma-rate-heterogeneity "(NCAT,SHAPE)"]
                          (-l|--length NUMBER) [-S|--seed [INT]]
    
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
      Create a given number of multi sequence alignments, each of which containing a
      given number of random sites drawn from the original multi sequence alignment.
    
    Available options:
      -a,--alphabet NAME       Specify alphabet type NAME
      INPUT-FILE               Read sequences from INPUT-FILE
      -n,--number-of-sites INT Number of sites randomly drawn with replacement
      -m,--number-of-alignments INT
                               Number of multi sequence alignments to be created
      -S,--seed [INT]          Seed for random number generator; list of 32 bit
                               integers with up to 256 elements (default: random)
      -h,--help                Show this help text


## Translate

Translate sequences.

    slynx translate --help

    Translate from DNA to Protein or DNAX to ProteinX.
    
    Usage: slynx translate (-a|--alphabet NAME) [INPUT-FILE]
                           (-r|--reading-frame INT) (-u|--universal-code CODE)
    
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

    ELynx Suite version 0.5.1. Developed by Dominik Schrempf. Compiled on September
    9, 2019, at 10:48 am, UTC.
    
    Usage: tlynx [-v|--verbosity VALUE] [-o|--output-file-basename NAME] COMMAND
      Compare, examine, and simulate phylogenetic trees.
    
    Available options:
      -h,--help                Show this help text
      -V,--version             Show version
      -v,--verbosity VALUE     Be verbose; one of: Quiet Warning Info
                               Debug (default: Info)
      -o,--output-file-basename NAME
                               Specify base name of output file
    
    Available commands:
      compare                  
      examine                  
      simulate                 Simulate reconstructed trees using the point process.
                               See Gernhard, T. (2008). The conditioned
                               reconstructed process. Journal of Theoretical
                               Biology, 253(4), 769–778.
                               http://doi.org/10.1016/j.jtbi.2008.04.005
    
    File formats:
      - Newick
    
    The ELynx Suite.
    A Haskell library and a tool set for computational biology. The goal of the
    ELynx Suite is reproducible research. Evolutionary sequences and phylogenetic
    trees can be read, viewed, modified and simulated. Exact specification of all
    options is necessary, and nothing is assumed about the data (e.g., the type of
    code). The command line with all arguments is consistently, and automatically
    logged. The work overhead in the beginning usually pays off in the end.
    slynx     Analyze, modify, and simulate evolutionary sequences.
    tlynx     Analyze, modify, and simulate phylogenetic trees.


## Compare

Compute distances between phylogenetic trees.

    tlynx compare --help

    Compute distances between phylogenetic trees.
    
    Usage: tlynx compare (-d|--distance MEASURE) [-s|--summary-statistics]
                         [INPUT-FILES]
    
    Available options:
      -d,--distance MEASURE    Type of distance to calculate (available distance
                               measures are listed below)
      -s,--summary-statistics  Report summary statistics only
      INPUT-FILES              Read tree(s) from INPUT-FILES; if more files are
                               given, one tree is expected per file
      -h,--help                Show this help text
    
    Available distance measures:
      Symmetric distance: -d symmetric
      Incompatible split distance: -d incompatible-split[VAL]
        Collapse branches with support less than VAL before distance calculation;
        in this way, only well supported difference contribute to the distance measure.


## Examine

Compute summary statistics of phylogenetic trees.

    tlynx examine --help

    Compute summary statistics of phylogenetic trees.
    
    Usage: tlynx examine [INPUT-FILE]
    
    Available options:
      INPUT-FILE               Read trees from INPUT-FILE
      -h,--help                Show this help text


## Simulate

Simulate phylogenetic trees using birth and death processes.

    tlynx simulate --help

    Simulate phylogenetic trees using birth and death processes.
    
    Usage: tlynx simulate [-t|--nTrees INT] [-n|--nLeaves INT] [-H|--height DOUBLE]
                          [-M|--condition-on-mrca] [-l|--lambda DOUBLE]
                          [-m|--mu DOUBLE] [-r|--rho DOUBLE] [-u|--sub-sample]
                          [-s|--summary-statistics] [-S|--seed [INT]]
      Simulate reconstructed trees using the point process. See Gernhard, T. (2008).
      The conditioned reconstructed process. Journal of Theoretical Biology, 253(4),
      769–778. http://doi.org/10.1016/j.jtbi.2008.04.005
    
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


# ELynx

Documentation of the library can be found on Hackage.

