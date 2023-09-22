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
