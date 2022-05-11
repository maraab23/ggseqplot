shared_params <- function() {
  c(
    "@param seqdata State sequence object (class \\code{stslist}) created with the \\code{\\link[TraMineR:seqdef]{TraMineR::seqdef}} function.",
    "@param weighted Controls if weights (specified in \\code{\\link[TraMineR:seqdef]{TraMineR::seqdef}}) should be used. Default is \\code{TRUE}, i.e. if available weights are used",
    "@param group Grouping variable of length equal to the number of sequences. When not NULL, a distinct plot is generated for each level of group."
  )
}

shared_facet <- function() {
  c(
    "@param facet_ncol Number of columns in faceted (i.e. grouped) plot",
    "@param facet_nrow Number of rows in faceted (i.e. grouped) plot"
  )
}


xandgrouplab <- function(seqdata, weighted, no.n, group) {
  if (weighted == FALSE) {
    attributes(seqdata)$weights <- rep(1, nrow(seqdata))
  }

  ylabspec <- purrr::map(
    unique(group),
    ~ sum(attributes(seqdata[group == .x,])$weights)
  ) |>
    unlist()

  if (length(ylabspec) == 1 & weighted == TRUE) {
    ylabspec <- glue::glue("Rel. Freq. (weighted n={round(ylabspec,2)})")
  } else if (length(ylabspec) == 1 & weighted == FALSE) {
    ylabspec <- glue::glue("Rel. Freq. (n={ylabspec})")
  } else if (weighted == TRUE) {
    ylabspec <- glue::glue("{unique(group)} (weighted n={round(ylabspec,2)})")
  } else {
    ylabspec <- glue::glue("{unique(group)} (n={ylabspec})")
  }

  grouplabspec <- dplyr::tibble(
    group = unique(group),
    grouplab = ylabspec
  )

  if (no.n == TRUE) {
    grouplabspec <- grouplabspec |>
      dplyr::mutate(grouplab = .data$group)
    ylabspec <- "Rel. Freq."
  }

  xandgrouplab <- list(grouplabspec = grouplabspec,
                       ylabspec = ylabspec)

  return(xandgrouplab)

}

