# Resubmission note

## R CMD check results

Runs without errors, warnings, and notes.

## devtools::check_rhub()

### Windows Server 2022, R-devel 64-bit

0 errors; 0 warnings; 4 notes  

Note 1: 
```
* checking R code for possible problems ... [14s] NOTE

```

Empty message, presumably due to usage of `packageStartupMessage`. Startup message can be suppressed.


Note 2:
```
* checking HTML version of manual ... NOTE
Skipping checking math rendering: package 'V8' unavailable
```
Unrelated to package


Note 3:
```
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
    ''NULL''
```
[R-hub issue #560]https://github.com/r-hub/rhub/issues/560


Note 4: 
```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.




### Ubuntu Linux 20.04.1 LTS, R-release, GCC

0 errors; 0 warnings; 2 notes

Note 1: 
```
* checking R code for possible problems ... [14s] NOTE
File ‘ggseqplot/R/zzz.R’:
  .onLoad calls: ...
```

Note due to `packageStartupMessage`. Startup message can be suppressed.


Note 2:
```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable
```
Unrelated to package


### Fedora Linux, R-devel, clang, gfortran

0 errors; 0 warnings; 2 notes

same notes as for Ubuntu - no problem


## Changes since release 0.8.2

- minor fixed
- added dependencies

