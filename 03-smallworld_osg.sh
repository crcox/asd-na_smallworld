#!/bin/bash

set -e

Rscript --vanilla --default-packages=methods,utils,stats,graphics \
    "03-smallworld_osg.R" $1 $2 $3 $4 $5 $6
