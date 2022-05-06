
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggseqplot: ggplotify sequence data plots <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![Version
badge](https://img.shields.io/github/r-package/v/maraab23/ggseqplot)](https://github.com/maraab23/ggseqplot)
[![R-CMD-check](https://github.com/maraab23/ggseqplot/workflows/R-CMD-check/badge.svg)](https://github.com/maraab23/ggseqplot/actions)
[![Codecov test
coverage](https://codecov.io/gh/maraab23/ggseqplot/branch/main/graph/badge.svg)](https://app.codecov.io/gh/maraab23/ggseqplot?branch=main)
<!-- badges: end -->

The main goal of `{ggseqplot}` is to provide functions that reproduce
some of the sequence plots from <a href="http://traminer.unige.ch"
target="_blank"><code>{TraMineR}</code></a>’s `seqplot` using
<a href="https://ggplot2.tidyverse.org/"
target="_blank"><code>{ggplot2}</code></a>. These plots are produced on
the basis of a sequence object defined with `TraMineR::seqdef`. The
package automates the reshaping and plotting of sequence data.

This package literally builds on the excellent work of the
<a href="http://traminer.unige.ch"
target="_blank"><code>{TraMineR}</code></a>,
<a href="http://traminer.unige.ch"
target="_blank"><code>{TraMineRExtras}</code></a>, and
<a href="https://ggplot2.tidyverse.org/"
target="_blank"><code>{ggplot2}</code></a> developers and uses several
of the their functions to produce ggplot2-flavored figures.

Note, that this library was not written because I personally dislike the
plots produced by <a href="http://traminer.unige.ch"
target="_blank"><code>{TraMineR}</code></a>, but rather because I am
normally using <a href="http://traminer.unige.ch"
target="_blank"><code>{TraMineR}</code></a> instead of base R’s `plot`
environment for visualizing data. <a href="http://traminer.unige.ch"
target="_blank"><code>{TraMineR}</code></a> was developed before
<a href="https://ggplot2.tidyverse.org/"
target="_blank"><code>{ggplot2}</code></a> was as popular as it is today
and back then many users were more familiar with coding base R plots. To
date, however, many researchers and students are more accustomed to
using <a href="https://ggplot2.tidyverse.org/"
target="_blank"><code>{ggplot2}</code></a> and prefer to draw on the
related skills and experiences instead of learning how to refine base R
plots just for the single purpose of visualizing sequence data.

The development of this library is in an early stage and will be
hopefully complemented by a few additional functions in the near future.

Currently, `{ggseqplot}` contains five functions:

-   `ggseqdplot` (equivalent to `TraMineR::seqdplot`)
-   `ggseqeplot` (equivalent to `TraMineRextras::seqplot.tentrop`)
-   `ggstrqeplot` (based on transitions rates computed with
    `TraMineR::seqtrate`)
-   `ggseqiplot` (equivalent to `TraMineR::seqIplot`)
-   `ggseqrfplot` (equivalent to `TraMineRextras::seqplot.rf`)

The documentation (including vignettes) is still very much **work in
progress**. While I am working on these, it is very likely that the
existing functions will be slightly revised and extended.

If you have preferences which plot types should be added, [create an
issue](https://github.com/maraab23/ggseqplot/issues/new) on github or
send me an
[email](mailto:marcel.raab@ifb.uni-bamberg.de?subject=ggseqplot%3A%20feature%20request).

Two vignettes outlines how `{ggseqplot}` reshapes sequence data
generated with `TraMineR::seqdef` to visualize them using
<a href="https://ggplot2.tidyverse.org/"
target="_blank"><code>{ggplot2}</code></a>. We compare the resulting
plots to those produced by <a href="http://traminer.unige.ch"
target="_blank"><code>{TraMineR}</code></a> and illustrate how to adjust
the baseline output using familiar
<a href="https://ggplot2.tidyverse.org/"
target="_blank"><code>{ggplot2}</code></a> functions (and add ons).

| Vignette                               | Plot type                                                              | Function                                        |
|:---------------------------------------|:-----------------------------------------------------------------------|:------------------------------------------------|
| `vignette("seq-summarization-plots")`  | state distribution plot<br/>entropy line plot<br/>transition rate plot | `ggseqdplot`<br/>`ggseqeplot`<br/>`ggseqtrplot` |
| `vignette("seq-representation-plots")` | sequence index plot<br/>relative frequency seqence plot                | `ggseqiplot`<br/>`ggseqrfplot`                  |

## Installation

You can install `{ggseqplot}` by typing:

``` r
devtools::install_github("maraab23/ggseqplot")
```
