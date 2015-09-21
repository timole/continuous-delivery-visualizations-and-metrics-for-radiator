#!/bin/sh

./jobs/CD-info-update-vcs-log.sh "$1"
Rscript ./jobs/CD-info-get.R
