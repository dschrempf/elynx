#!/usr/bin/env bash

program_name=evolution-modify
file_name=test/Data/ProfileSmall.fa.gz

stack build --profile || exit 1
# See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#profiling
# Run the program with
# -p: profiling support
# -S: additional statistics
# -h: memory heap profile
# -H: set heap size
# -K: set stack size
# -l: event log (compile option is necessary)
stack exec $program_name -- -a DNA_IUPAC $file_name +RTS -p -hy -K100M -l -s

# Create a graph of the memory heap profile.
hp2ps -e10in -c ${program_name}.hp

# Print which RTS options are available.
# stack exec $program_name -- +RTS -?

