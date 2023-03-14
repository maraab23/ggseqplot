# Resubmission note

## R CMD check results

Runs without errors, warnings, and notes.

## devtools::check_rhub()

0 errors; 0 warnings

Two notes  on Windows (Server 2022, R-devel 64-bit): 


```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.


```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable
```

Unrelated to package


## Changes since release 0.8.1

- Updated to geom_line aes argument "linewidth" due to new release of ggplot2
