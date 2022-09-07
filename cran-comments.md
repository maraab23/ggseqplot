# Resubmission note

## R CMD check results

Runs without errors, warnings, and notes.

## devtools::check_rhub()

0 errors; 0 warnings

Two notes on possibly invalid URLS: both URLS are fine

One note that is only found on Windows (Server 2022, R-devel 64-bit): 

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.


## Changes since release 0.7.2

- Added TraMineR (>= 2.2-5) to dependencies; updated documentation/vignette accordingly 
- Completely revised `ggseqrfplot` to reflect new TraMineR release
- fixed sorting order for functions which allow for group argument (minor change)
