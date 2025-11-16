# ggseqplot: ggplotify sequence data plots

The main goal of [`{ggseqplot}`](https://maraab23.github.io/ggseqplot/)
is to provide functions that reproduce the sequence plots from
[`{TraMineR}`](http://traminer.unige.ch)’s `seqplot` using
[`{ggplot2}`](https://ggplot2.tidyverse.org/). These plots are produced
on the basis of state sequence objects defined with
[`TraMineR::seqdef`](https://rdrr.io/pkg/TraMineR/man/seqdef.html). The
package automates the reshaping and plotting of sequence data. This
library literally builds on the excellent work of the
[`{TraMineR}`](http://traminer.unige.ch) and
[`{ggplot2}`](https://ggplot2.tidyverse.org/) developers and uses
several of their functions to produce ggplot2-flavored figures.

Note, that this library was not written because I personally dislike the
plots produced by [`{TraMineR}`](http://traminer.unige.ch), but rather
because I normally use [`{ggplot2}`](https://ggplot2.tidyverse.org/)
instead of base R’s `plot` environment for data visualization.
[`{TraMineR}`](http://traminer.unige.ch) was developed before
[`{ggplot2}`](https://ggplot2.tidyverse.org/) became as popular as it is
today, when most users were more familiar with base R plots. Today,
however, many researchers and students prefer to use
[`{ggplot2}`](https://ggplot2.tidyverse.org/) and draw on their existing
skills rather than learn base R graphics just to visualize sequence
data.

[`{ggseqplot}`](https://maraab23.github.io/ggseqplot/) contains the
following functions:

- `ggseqdplot` (equivalent to
  [`TraMineR::seqdplot`](https://rdrr.io/pkg/TraMineR/man/seqplot.html))
- `ggseqeplot` (equivalent to `TraMineRextras::seqplot.tentrop`)
- `ggseqmsplot` (equivalent to
  [`TraMineR::seqmsplot`](https://rdrr.io/pkg/TraMineR/man/seqplot.html))
- `ggseqmtplot` (equivalent to
  [`TraMineR::seqmtplot`](https://rdrr.io/pkg/TraMineR/man/seqplot.html))
- `ggseqtrplot` (based on transitions rates from
  [`TraMineR::seqtrate`](https://rdrr.io/pkg/TraMineR/man/seqtrate.html))
- `ggseqiplot` (equivalent to
  [`TraMineR::seqIplot`](https://rdrr.io/pkg/TraMineR/man/seqplot.html))
- `ggseqfplot` (equivalent to
  [`TraMineR::seqfplot`](https://rdrr.io/pkg/TraMineR/man/seqplot.html))
- `ggseqrplot` (equivalent to
  [`TraMineR::seqrplot`](https://rdrr.io/pkg/TraMineR/man/seqplot.html))
- `ggseqrfplot` (equivalent to `TraMineRextras::seqplot.rf`)

A complementing
[vignette](https://maraab23.github.io/ggseqplot/articles/ggseqplot.html)
outlines how [`{ggseqplot}`](https://maraab23.github.io/ggseqplot/)
reshapes sequence data generated with
[`{TraMineR}`](http://traminer.unige.ch) functions for visualization
with [`{ggplot2}`](https://ggplot2.tidyverse.org/). It also shows how to
customize the plots using familiar
[`{ggplot2}`](https://ggplot2.tidyverse.org/) functions and extensions.

If you find errors or have feature requests, feel free to [create an
issue](https://github.com/maraab23/ggseqplot/issues/new) on GitHub or
send me an
[email](mailto:marcel.raab@ifb.uni-bamberg.de?subject=ggseqplot%3A%20feature%20request).

## Citation

If you use [`{ggseqplot}`](https://cran.r-project.org/package=ggseqplot)
in your work, please cite it as:

> Raab, M. (2022). *ggseqplot: Render Sequence Plots using ‘ggplot2’*.
> <https://doi.org/10.32614/CRAN.package.ggseqplot>

You can also retrieve the citation in R with:

``` r
citation("ggseqplot")
```

## Installation

You can install the CRAN version of
[`{ggseqplot}`](https://maraab23.github.io/ggseqplot/) with:

``` r
install.packages("ggseqplot")
```

If a more recent version is available, you can install the development
version from GitHub with:

``` r
devtools::install_github("maraab23/ggseqplot")
```

## Acknowledgements

I would like to thank Gilbert Ritschard, Tim Liao, and Emanuela
Struffolino for their helpful comments on earlier versions of this
library.
