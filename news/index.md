# Changelog

## ggseqplot 0.8.9

CRAN release: 2025-11-15

- added `ytlab` argument for `ggseqiplot` (requested by
  [@helske](https://github.com/helske),
  [\#9](https://github.com/maraab23/ggseqplot/issues/9)); allows to use
  ID labels in index plots similar to
  [TraMineR](http://traminer.unige.ch)
- fixed empty aesthetic warning in `ggseqmsplot`
- moved `ggplot2` and `TraMineR` to Imports
- behind the scenes: revised tests and added `revdepcheck`

## ggseqplot 0.8.8

CRAN release: 2025-10-08

- housekeeping update with a bunch of improvements of the code base
  ([\#8](https://github.com/maraab23/ggseqplot/issues/8), thanks to
  [@olivroy](https://github.com/olivroy))
- remove dependency on `hrbrthemes`, adjust vignette accordingly due to
  `extrafont`/CRAN issue
  ([\#10](https://github.com/maraab23/ggseqplot/issues/10), thanks to
  [@helske](https://github.com/helske))
- fixed
  [`ggtext::element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html)
  problem in rplots by specifying theme argument

## ggseqplot 0.8.7

CRAN release: 2025-06-30

- fixed `default_aes` issue in anticipation of upcoming
  [ggplot2](https://ggplot2.tidyverse.org) ggplot2 u
  ([\#7](https://github.com/maraab23/ggseqplot/issues/7), thanks to
  [@teunbrand](https://github.com/teunbrand))

## ggseqplot 0.8.6

CRAN release: 2025-05-06

- fixed ggseqmtplot error if alphabet is numeric (raised and solved by
  Gilbert Ritschard)
- revised and extended Vignette

## ggseqplot 0.8.5

CRAN release: 2024-10-29

- fixed [\#5](https://github.com/maraab23/ggseqplot/issues/5): if group
  vector is numeric, grouped plots are now sorted by number instead of
  order of appearance
- `.data` in [tidyselect](https://tidyselect.r-lib.org) expressions was
  deprecated in [tidyselect](https://tidyselect.r-lib.org) 1.2.0; update
  takes care of this change
- fixed incorrect group handling in `ggseqfplot` (issue reported by
  Gilbert Ritschard)

## ggseqplot 0.8.4

CRAN release: 2024-05-17

- fixed [\#3](https://github.com/maraab23/ggseqplot/issues/3):
  haven_labelled group vars are converted into factors before plotting
- fixed [\#4](https://github.com/maraab23/ggseqplot/issues/4): legends
  display key glyph for non-visited states (this wasn’t the case anymore
  due to [ggplot2](https://ggplot2.tidyverse.org) release 3.5.0)

## ggseqplot 0.8.3

CRAN release: 2023-09-22

- added missing minimum versions for dependencies & imports (ggplot2,
  dplyr, forcats)
- changed default for sorting sequences in ggseqrfplot to match behavior
  TraMineR::seqrfplot
- added [TraMineR](http://traminer.unige.ch) and
  [ggplot2](https://ggplot2.tidyverse.org) as dependencies

## ggseqplot 0.8.2

CRAN release: 2023-03-15

- implemented changes required due to recent tidyverse updates
- removed startup message

## ggseqplot 0.8.1

CRAN release: 2022-10-11

- reverted TraMiner to Import
- removed unnecessary Suggests from TraMineRextras
- revised vignette, tests, and documentation accordingly
- harmonized example code

## ggseqplot 0.8.0

CRAN release: 2022-09-07

- complete revision of `ggseqrfplot` after
  [TraMineR](http://traminer.unige.ch) release 2.2-5 (special thanks to
  Gilbert Ritschard for the support)

- added [TraMineR](http://traminer.unige.ch) as dependency

- the group argument now respects order of grouping variable when
  rendering faceted plots (issue raised bv Lucille Mattijssen)

## ggseqplot 0.7.2

CRAN release: 2022-08-05

- fixed sorting issue for y-axis labels in grouped iplots

- allow for additional `facet_wrap` arguments to change plot appearance

## ggseqplot 0.7.1

- Tweaking the theme: adding ticks and x axis line

## ggseqplot 0.7.0

- Added option to break down dplots by state (`dissect`)

- Set minimum R version because of R’s new pipe
  ([\#1](https://github.com/maraab23/ggseqplot/issues/1),
  [@cbrueffer](https://github.com/cbrueffer))

## ggseqplot 0.6.2

CRAN release: 2022-07-04

- Added a `NEWS.md` file to track changes to the package.
