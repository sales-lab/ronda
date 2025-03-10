
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ronda

<!-- badges: start -->
<!-- badges: end -->

The primary goal of `ronda` is to simplify the conversion of R packages
into a format that can be easily installed and managed via Conda,
enhancing interoperability between R and other programming environments
supported by Conda.

## Installation

You can install the development version of ronda from its [source
repository](https://github.com/sales-lab/ronda) with:

``` r
# install.packages("pak")
pak::pak("sales-lab/ronda")
```

You will also need to install Miniconda. We recommend using the
conda-forge installer, available at <https://conda-forge.org/download/>.

Once Miniconda is installed, activate the `base` environment and install
`conda-build`. The [official
documentation](https://docs.conda.io/projects/conda-build/en/latest/install-conda-build.html#way-of-working)
suggests installing it directly in the base environment for simplicity
and consistency.

## Usage

Here’s a simple example of how to use `ronda` to build Conda packages
for some popular R packages.

``` r
library(ronda)
ronda_build(c("cli", "jsonlite"))
#> ℹ Building cli
#> ℹ Loading metadata database
#> ✔ Loading metadata database ... done
#> 
#> ℹ Building cli
#> ✔ Built cli [4m 32.4s]
#> 
#> ℹ Building jsonlite
#> ✔ Built jsonlite [3m 39s]
```

Once the procedure is completed successfully, the packages will be
available in the `conda-bld` subdirectory of your main Conda
environment. You can use `conda env list` to locate this directory.
