.onLoad  <- function(libname, pkgname) {

  if (requireNamespace("TraMineR", quietly = TRUE)) {
    # Use functions from TraMineR
  } else {
    stop("Package 'TraMineR' not found.")
  }

  packageStartupMessage(
    glue::glue("{usethis::ui_field('ggseqplot')} version {utils::packageVersion(pkgname)}
             Website: {usethis::ui_field('https://maraab23.github.io/ggseqplot/')}
             Please type {usethis::ui_code('citation(\"ggseqplot\")')} for citation information.

             {usethis::ui_field('ggseqplot')} heavily builds on the {usethis::ui_field('TraMineR')} library (current version {utils::packageVersion('TraMineR')})
             Please type {usethis::ui_code('citation(\"TraMineR\")')} for citation information.")
  )
}
