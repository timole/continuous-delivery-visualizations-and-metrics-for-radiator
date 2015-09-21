#!/bin/sh

VCS_ROOT=$1

logfile="`pwd`/jobs/CD-info-vcs.log"
echo -e "date\trev\tbranch\tparents\tchildren\tdesc" > "$logfile"
cd "$VCS_ROOT"

hg log --date "-100" --template '{date|isodatesec}\t{rev}\t{branch}\t{parents}\t{children}\t{desc|strip|firstline}\n' >> "$logfile"
