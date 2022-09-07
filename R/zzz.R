.onAttach <- function(libname, pkgname) {
  #quietly <- getOption('quietly')
  old <- options()
  on.exit(options(old))
  options(quietly = T)
  packageStartupMessage(
    usethis::ui_info(glue::glue("{usethis::ui_field('ggseqplot')} version {utils::packageVersion(pkgname)}
                            Website: {usethis::ui_field('https://maraab23.github.io/ggseqplot/')}
                            Please type {usethis::ui_code('citation(\"ggseqplot\")')} for citation information.")),
    usethis::ui_info(glue::glue("{usethis::ui_field('ggseqplot')} attached {usethis::ui_field('TraMineR')} version {utils::packageVersion('TraMineR')}
                            Please type {usethis::ui_code('citation(\"TraMineR\")')} for citation information."))
  )
  #options(quietly = quietly)
}
