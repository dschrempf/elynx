#!/usr/bin/env bash

program_name=evolution-modify

stack build --profile
# Run the program with
# -p: profiling support
# -S: additional statistics
# -h: memory heap profile
# -H: set heap size
# -l: event log (compile option is necessary)
stack exec $program_name -- -a DNA_IUPAC test/Data/Profile.fa.gz +RTS -p -h -H1G -l -s

# Create a graph of the memory heap profile.
hp2ps -e10in -c ${program_name}.hp

# Print which RTS options are available.
# stack exec $program_name -- +RTS -?

