.onAttach <- function(libname, pkgname) {
  packageStartupMessage(cli::format_message(
    c(
      "i" = "{.pkg ggseqplot} version {utils::packageVersion(pkgname)} loaded",
      " " = "{.emph Website}: {.url https://maraab23.github.io/ggseqplot/}",
      " " = "{.emph Citation}: {.code citation(\"ggseqplot\")}"
    )
  ))
}
