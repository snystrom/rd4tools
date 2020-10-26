
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rd4utils

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

rd4utils wraps the [d4utils](https://github.com/38/d4-format) tool for
reading & writing d4 files using R data structures.

## Installation

### Development Version

You can install the development version of rd4utils from
[GitHub](https://github.com/snystrom/rd4utils) with:

``` r
if (!requireNamespace("remotes", quietly=TRUE))
  install.packages("remotes")
remotes::install_github("snystrom/rd4utils")
```

``` r
library(rd4utils)
```

### d4utils

rd4utils relies on a local install of
[d4utils](https://github.com/38/d4-format). You can find binaries at the
[d4utils releases](https://github.com/38/d4-format/releases) page, or if
you have [Rust](https://www.rust-lang.org/tools/install) installed, can
compile from source using `cargo install d4utils`.

rd4utils provides `d4_install_cargo()` which will run `cargo install
d4utils` from R, installing it in the users default cargo directory.

rd4utils needs to know the location of `rd4utils` on your local machine.
If installing using cargo, rd4utils should autodetect the binary if
installed at `~/.cargo/bin/d4utils`. Otherwise, you can set the
`d4utils` option once per session like so:

`options("d4utils" = "path/to/d4utils")`

Alternatively, all rd4utils functions contain the `d4utils` argument,
which accepts a path to the `d4utils` binary.

To test whether R detects d4utils, run

``` r
d4_install_check()
#> checking main install
#> ✓ /home/rstudio/.cargo/bin
```

## Examples

Examples are still a work-in-progress, but the following functions all
work in my hands. See their man pages for details on usage.

  - `d4_create()` convert a Bigwig, BAM, or CRAM file to d4 format.
  - `d4_view()` returns signal within target regions as `GRanges`
  - `d4_view_genome()` returns genome file used as `Seqinfo`
  - `d4_stat()` returns summary statistics
  - `d4_histogram()` returns a histogram of score values

# Citation

Please cite the d4 authors if using rd4utils:

Efficient storage and analysis of quantitative genomics data with the
Dense Depth Data Dump (D4) format and d4tools. Hao Hou, Brent S
Pedersen, Aaron Quinlan bioRxiv 2020.10.23.352567; doi:
<https://doi.org/10.1101/2020.10.23.352567>
