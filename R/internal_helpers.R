xandgrouplab <- function(seqdata, weighted, no.n, group, grinorder, ylabprefix) {
  if (weighted == FALSE) {
    attributes(seqdata)$weights <- rep(1, nrow(seqdata))
  }

  ylabspec <- purrr::map(
    grinorder,
    ~ sum(attributes(seqdata[group == .x,])$weights)
  ) |>
    unlist()

  if (length(ylabspec) == 1 && weighted == TRUE) {
    ylabspec <- glue::glue("{ylabprefix} (weighted n={round(ylabspec,2)})")
  } else if (length(ylabspec) == 1 && weighted == FALSE) {
    ylabspec <- glue::glue("{ylabprefix} (n={ylabspec})")
  } else if (weighted == TRUE) {
    ylabspec <- glue::glue("{grinorder} (weighted n={round(ylabspec,2)})")
  } else {
    ylabspec <- glue::glue("{grinorder} (n={ylabspec})")
  }

  grouplabspec <- dplyr::tibble(
    group = forcats::fct_inorder(grinorder),
    grouplab = forcats::fct_inorder(ylabspec)
  )

  if (no.n == TRUE) {
    grouplabspec <- grouplabspec |>
      dplyr::mutate(grouplab = .data$group)
    ylabspec <- ylabprefix
  }

  xandgrouplab <- list(grouplabspec = grouplabspec,
                       ylabspec = ylabspec)

  return(xandgrouplab)

}

