# both don't work
.onLoad  <- function(libname, pkgname) {

  suppressPackageStartupMessages(requireNamespace("TraMineR", quietly = TRUE))
  suppressPackageStartupMessages(library(TraMineR, quietly = TRUE))
}


.onAttach <- function(libname, pkgname) {

  suppressPackageStartupMessages(requireNamespace("TraMineR", quietly = TRUE))
  suppressPackageStartupMessages(library(TraMineR, quietly = TRUE))
}
