#!/usr/bin/env bash

source include
./build

file_profile=test/Data/Profile.fa.gz

msg "EvoMode-Seq profiling run."
# See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#profiling
# Run the program with
# -p: profiling support
# -S: additional statistics
# -h: memory heap profile
# -H: set heap size
# -K: set stack size
# -l: event log (compile option is necessary)
stack exec evomod-seq --work-dir .stack-work-profiling -- summarize -a DNA_IUPAC $file_profile +RTS -p -hy -l -s

# Create a graph of the memory heap profile.
hp2ps -e10in -c evomod-seq.hp

# Print which RTS options are available.
# stack exec $program_name -- +RTS -?

