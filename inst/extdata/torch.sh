#!/bin/sh
set -e

R CMD INSTALL --build .
CUDA_HOME="$CONDA_PREFIX" Rscript -e 'torch::install_torch()'
