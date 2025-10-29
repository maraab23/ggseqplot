# ggseqplot 0.8.9

* added `ytlab` argument for `ggseqiplot` (requested by @helske, #9); allows to use ID labels in index plots similar to `{TraMineR}` 
* fixed empty aesthetic warning in `ggseqmsplot`
* moved `ggplot2` and `TraMineR` to Imports
* behind the scenes: revised tests and added `revdepcheck`

# ggseqplot 0.8.8

* housekeeping update with a bunch of improvements of the code base (#8, thanks to @olivroy)
* remove dependency on `hrbrthemes`, adjust vignette accordingly due to `extrafont`/CRAN issue (#10, thanks to @helske)
* fixed `ggtext::element_markdown()` problem in rplots by specifying theme argument 

# ggseqplot 0.8.7

* fixed `default_aes` issue in anticipation of upcoming `{ggplot2}` ggplot2 u (#7, thanks to @teunbrand)

# ggseqplot 0.8.6

* fixed ggseqmtplot error if alphabet is numeric (raised and solved by Gilbert Ritschard)
* revised and extended Vignette

# ggseqplot 0.8.5

* fixed #5: if group vector is numeric, grouped plots are now sorted by number instead of order of appearance
* `.data` in `{tidyselect}` expressions was deprecated in `{tidyselect}` 1.2.0; update takes care of this change
* fixed incorrect group handling in `ggseqfplot` (issue reported by Gilbert Ritschard)

# ggseqplot 0.8.4

* fixed #3: haven_labelled group vars are converted into factors before plotting
* fixed #4: legends display key glyph for non-visited states (this wasn't the case anymore due to `{ggplot2}` release 3.5.0)


# ggseqplot 0.8.3

* added missing minimum versions for dependencies & imports (ggplot2, dplyr, forcats)
* changed default for sorting sequences in ggseqrfplot to match behavior TraMineR::seqrfplot
* added `{TraMineR}` and `{ggplot2}` as dependencies 

# ggseqplot 0.8.2

* implemented changes required due to recent tidyverse updates
* removed startup message

# ggseqplot 0.8.1

* reverted TraMiner to Import 
* removed unnecessary Suggests from TraMineRextras
* revised vignette, tests, and documentation accordingly
* harmonized example code

# ggseqplot 0.8.0

* complete revision of `ggseqrfplot` after `{TraMineR}` release 2.2-5 
  (special thanks to Gilbert Ritschard for the support)
  
* added `{TraMineR}` as dependency 

* the group argument now respects order of grouping variable when rendering faceted plots 
  (issue raised bv Lucille Mattijssen)

# ggseqplot 0.7.2

* fixed sorting issue for y-axis labels in grouped iplots

* allow for additional `facet_wrap` arguments to change plot appearance 

# ggseqplot 0.7.1

* Tweaking the theme: adding ticks and x axis line

# ggseqplot 0.7.0 

* Added option to break down dplots by state (`dissect`)

* Set minimum R version because of R's new pipe (#1, @cbrueffer)

# ggseqplot 0.6.2

* Added a `NEWS.md` file to track changes to the package.
