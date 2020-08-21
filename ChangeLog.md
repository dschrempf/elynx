
# Changelog for ELynx


## Unreleased changes


## Version 0.3.4

-   Improve `slynx examine`; show hamming distance; show constant sites.
-   PhyloStrict -> PhyloExplicit; and some conversion functions were changed.
-   `tlynx coalesce` was merged into `tlynx simulate`, the syntax has changed; see
    `tlynx simulate --help`.


## Version 0.3.3

-   Fix test suites.


## Version 0.3.2

-   Remove llvm dependency.
-   Move away from hpack.


## Version 0.3.1

-   Use Attoparsec.
-   Use ByteString consistently.
-   Remove elynx-tools dependency from libaries.


## Version 0.3.0

-   **`elynx-nexus`:** library to import and export Nexus files.
-   **`elynx-tree`:** major refactor and big cleanup; use rose trees with branch
    labels.
-   **`elynx-tree`:** provide zippers.


## Version 0.2.2

-   Validation and repetition of previous analyses is finally possible with the
    new `elynx` binary.
-   A library `elynx-markov` for running Markov processes along phylogenetic trees
    has been split off `elynx-seq`. This library performs the computations when
    executing `slynx simulate ...`.
-   Many other small improvements.

