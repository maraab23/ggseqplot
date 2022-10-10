.onAttach  <- function(libname, pkgname) {
  old <- options()
  on.exit(options(old))
  options(quietly = T)
  packageStartupMessage(
    usethis::ui_info(glue::glue("{usethis::ui_field('ggseqplot')} version {utils::packageVersion(pkgname)}
                            Website: {usethis::ui_field('https://maraab23.github.io/ggseqplot/')}
                            Please type {usethis::ui_code('citation(\"ggseqplot\")')} for citation information.")),
    usethis::ui_info(glue::glue("{usethis::ui_field('ggseqplot')} heavily builds on the {usethis::ui_field('TraMineR')} library (current version {utils::packageVersion('TraMineR')})
                            Please type {usethis::ui_code('citation(\"TraMineR\")')} for citation information."))
  )
}
