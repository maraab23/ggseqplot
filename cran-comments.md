# Resubmission note

## R CMD check results

Runs without errors, warnings, and notes.

## devtools::check_rhub()

### Windows Server 2022, R-devel 64-bit

0 errors; 0 warnings; 2 notes  

Note 1: 
```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.


Note 2:
```
* checking HTML version of manual ... NOTE
Skipping checking math rendering: package 'V8' unavailable
```
Unrelated to package

### Fedora Linux, R-devel, clang, gfortran

0 errors; 0 warnings; 1 note

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable
```
Unrelated to package

### Ubuntu Linux 20.04.1 LTS, R-release, GCC

0 errors; 0 warnings; 1 note

```
Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1111/j.1467-985X.2009.00606.x
    From: inst/doc/ggseqplot.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.1177/0049124113506563
    From: inst/doc/ggseqplot.html
    Status: 403
    Message: Forbidden
```
Checked URLs, both work


## Changes since release 0.8.1

- Updated to geom_line aes argument "linewidth" due to new release of ggplot2
- removed startup message

