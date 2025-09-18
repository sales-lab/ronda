
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

After installing Miniconda, create and activate a new environment with
the necessary build tools.

``` bash
conda create -n ronda conda-build rattler-build rattler-index
conda activate ronda
```

## Usage

Here’s a simple example of how to use `ronda` to build Conda packages
for some popular R packages.

``` r
library(ronda)
ronda_build(c("cli", "jsonlite"))
#> Warning: Some packages declare unknown dependencies.
#> ℹ Ignoring 13 packagess.
#> ℹ Building cli
#> ℹ Loading metadata database
#> ✔ Loading metadata database ... done
#> 
#> ℹ Building cli✔ Built cli [1m 13.9s]
#> 
#> ℹ Building jsonlite
#> ✔ Built jsonlite [42.2s]
```

Once the procedure is completed successfully, the packages will be
available in the `conda-bld` subdirectory of your main Conda
environment. You can use `conda env list` to locate this directory.
