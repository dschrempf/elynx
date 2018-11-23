#!/usr/bin/env bash

prog_name=evomod
file_name=test/Data/Profile.fa.gz

stack build --profile --work-dir .stack-work-profiling || exit 1
echo

# See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#profiling
# Run the program with
# -p: profiling support
# -S: additional statistics
# -h: memory heap profile
# -H: set heap size
# -K: set stack size
# -l: event log (compile option is necessary)
stack exec $prog_name --work-dir .stack-work-profiling -- summarize -a DNA_IUPAC $file_name +RTS -p -hy -l -s

# Create a graph of the memory heap profile.
hp2ps -e10in -c ${prog_name}.hp

# Print which RTS options are available.
# stack exec $program_name -- +RTS -?

