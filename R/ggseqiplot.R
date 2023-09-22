#' Sequence Index Plot
#'
#' Function for rendering sequence index plots with
#' \code{\link[ggplot2]{ggplot2}} \insertCite{wickham2016}{ggseqplot} instead
#' of base R's \code{\link[base]{plot}} function that is used by
#' \code{\link[TraMineR:seqplot]{TraMineR::seqplot}}
#' \insertCite{gabadinho2011}{ggseqplot}.
#'
#' @eval shared_params()
#' @param no.n specifies if number of (weighted) sequences is shown as part of
#' the y-axis title or group/facet title (default is \code{TRUE})
#' @param sortv Vector of numerical values sorting the sequences or a sorting
#' method (either \code{"from.start"} or \code{"from.end"}). See details.
#' @param border if \code{TRUE} bars are plotted with black outline; default is \code{FALSE} (also accepts \code{NULL})
#' @param facet_scale Specifies if y-scale in faceted plot should be free
#' (\code{"free_y"} is default) or \code{"fixed"}
#' @eval shared_facet()
#' @param ... if group is specified additional arguments of \code{\link[ggplot2:facet_wrap]{ggplot2::facet_wrap}}
#' such as \code{"labeller"} or \code{"strip.position"} can be used to change the appearance of the plot
#'
#' @return A sequence index plot. If stored as object the resulting list object
#' also contains the data (spell format) used for rendering the plot.
#' @export
#'
#' @details Sequence index plots have been introduced by
#' \insertCite{scherer2001;textual}{ggseqplot} and display each sequence as
#' horizontally stacked bar or line. For a more detailed discussion of this
#' type of sequence visualization see, for example,
#' \insertCite{brzinsky-fay2014;textual}{ggseqplot},
#' \insertCite{fasang2014;textual}{ggseqplot},
#' and \insertCite{raab2022;textual}{ggseqplot}.
#'
#' The function uses \code{\link[TraMineR:seqformat]{TraMineR::seqformat}}
#' to reshape \code{seqdata} stored in wide format into a spell/episode format.
#' Then the data are further reshaped into the long format, i.e. for
#' every sequence each row in the data represents one specific sequence
#' position. For example, if we have 5 sequences of length 10, the long file
#' will have 50 rows. In the case of sequences of unequal length not every
#' sequence will contribute the same number of rows to the long data.
#'
#' The reshaped data are used as input for rendering the index plot using
#' ggplot2's \code{\link[ggplot2]{geom_rect}}. \code{ggseqiplot} uses
#' \code{\link[ggplot2]{geom_rect}} instead of \code{\link[ggplot2]{geom_tile}}
#' because this allows for a straight forward implementation of weights.
#' If weights are specified for \code{seqdata} and \code{weighted=TRUE}
#' the sequence height corresponds to its weight.
#'
#' If weights and a grouping variable are used, and \code{facet_scale="fixed"}
#' the values of the y-axis are not labeled, because
#' \code{\link[ggplot2]{ggplot2}} reasonably does not allow for varying scales
#' when the facet scale is fixed.
#'
#' When a \code{sortv} is specified, the sequences are arranged in the order of
#' its values. With \code{sortv="from.start"} sequence data are sorted
#' according to the states of the alphabet in ascending order starting with the
#' first sequence position, drawing on succeeding positions in the case of
#' ties. Likewise, \code{sortv="from.end"} sorts a reversed version of the
#' sequence data, starting with the final sequence position turning to
#' preceding positions in case of ties.
#'
#' Note that the default aspect ratio of \code{ggseqiplot} is different from
#' \code{\link[TraMineR:seqIplot]{TraMineR::seqIplot}}. This is most obvious
#' when \code{border=TRUE}. You can change the ratio either by adding code to
#' \code{ggseqiplot} or by specifying the ratio when saving the code with
#' \code{\link[ggplot2]{ggsave}}.
#'
#' @author Marcel Raab
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' # Use example data from TraMineR: actcal data set
#' data(actcal)
#'
#' # We use only a sample of 300 cases
#' set.seed(1)
#' actcal <- actcal[sample(nrow(actcal), 300), ]
#' actcal.lab <- c("> 37 hours", "19-36 hours", "1-18 hours", "no work")
#' actcal.seq <- seqdef(actcal, 13:24, labels = actcal.lab)
#'
#' # ex1 using weights
#' data(ex1)
#' ex1.seq <- seqdef(ex1, 1:13, weights = ex1$weights)
#'
#' # sequences sorted by age in 2000 and grouped by sex
#' # with TraMineR::seqplot
#' seqIplot(actcal.seq, group = actcal$sex, sortv = actcal$age00)
#' # with ggseqplot
#' ggseqiplot(actcal.seq, group = actcal$sex, sortv = actcal$age00)
#'
#' # sequences of unequal length with missing state, and weights
#' seqIplot(ex1.seq)
#' ggseqiplot(ex1.seq)
#'
#' # ... turn weights off and add border
#' seqIplot(ex1.seq, weighted = FALSE, border = TRUE)
#' ggseqiplot(ex1.seq, weighted = FALSE, border = TRUE)
#'
#' @import ggplot2
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
ggseqiplot <- function(seqdata,
                       no.n = FALSE,
                       group = NULL,
                       sortv = NULL,
                       weighted = TRUE,
                       border = FALSE,
                       facet_scale = "free_y",
                       facet_ncol = NULL,
                       facet_nrow = NULL,
                       ...) {
  if (!inherits(seqdata, "stslist")) {
    stop("data are not stored as sequence object, use 'TraMineR::seqdef' to create one")
  }

  if (is.null(border)) border <- FALSE

  if (!is.logical(weighted) | !is.logical(border)) {
    stop(glue::glue("the arguments `weighted` or `border` have to \\
    be objects of type logical"))
  }


  if (exists("weights", where = attributes(seqdata)) == TRUE &
      weighted == TRUE) {
    weights <- attributes(seqdata)$weights
  } else {
    weights <- rep(1, nrow(seqdata))
    weighted <- FALSE
  }

  if (!facet_scale %in% c("free_y", "fixed")) {
    stop('the argument `facet_scale` has to be either "free_y" or "fixed"')
  }

  if (!is.null(facet_ncol) && as.integer(facet_ncol) != facet_ncol) {
    stop("`facet_ncol` must be NULL or an integer.")
  }

  if (!is.null(facet_nrow) && as.integer(facet_nrow) != facet_nrow) {
    stop("`facet_nrow` must be NULL or an integer.")
  }

  if (is.factor(group)) {
    group <- forcats::fct_drop(group)
    grinorder <- levels(group)
  } else {
    grinorder <- factor(unique(group))
  }
  if (is.null(group)) grinorder <- factor(1)


  auxid <- dplyr::tibble(id = as.character(attributes(seqdata)$row.names)) |>
    dplyr::mutate(
      idnew = dplyr::row_number(),
      weight = weights,
      group = NA
    )

  if (is.null(group) == FALSE) auxid$group <- group


  if (length(sortv) == 1 && sortv == "from.end") {
    sortx <- dplyr::select(
      as.data.frame(seqdata),
      rev(colnames(seqdata))
    )
  }

  if (length(sortv) == 1 && sortv == "from.start") {
    sortx <- as.data.frame(seqdata)
  }

  if (length(sortv) == 1 && sortv %in% c("from.start", "from.end")) {
    sortv <- sortx |>
      dplyr::mutate(idx = dplyr::row_number()) |>
      dplyr::arrange(dplyr::across(-.data$idx)) |>
      dplyr::pull(.data$idx) |>
      order()
  }

  if (is.null(sortv) == FALSE) {
    auxid <- auxid |>
      dplyr::mutate(sortv = {{ sortv }}) |>
      dplyr::arrange(.data$sortv) |>
      dplyr::mutate(idnew = dplyr::row_number())
  } else {
    auxid$sortv <- auxid$idnew
  }

  suppressMessages(
    spelldata <- TraMineR::seqformat(seqdata, to = "SPELL") |>
      dplyr::full_join(auxid, by = dplyr::join_by("id")) |>
      dplyr::select(.data$idnew, dplyr::everything()) |>
      dplyr::mutate(
        states = factor(.data$states,
                        levels = TraMineR::alphabet(seqdata),
                        labels = attributes(seqdata)$labels
        ),
        states = forcats::fct_na_value_to_level(.data$states,
                                          level = "Missing"
        ),
        states = forcats::fct_drop(.data$states, "Missing") # shouldn't be necessary
      ) |>
      dplyr::group_by(.data$idnew) |>
      dplyr::mutate(spell = dplyr::row_number(), .after = 1) |>
      dplyr::as_tibble() |>
      dplyr::rename(left = .data$begin, right = .data$end)
  )


  suppressMessages(
    dt <- spelldata |>
      dplyr::arrange(sortv) |>
      dplyr::select(.data$idnew, .data$weight, group) |>
      dplyr::distinct(.data$idnew, .keep_all = TRUE) |>
      dplyr::ungroup() |>
      dplyr::group_by(group) |>
      dplyr::mutate(begin = 0, end = cumsum(.data$weight)) |>
      dplyr::ungroup() |>
      dplyr::select(-.data$weight, -.data$group) |>
      dplyr::full_join(spelldata, by = "idnew")
  )


  dt2 <- dt |>
    dplyr::group_by(group) |>
    dplyr::mutate(begin = ifelse(dplyr::row_number() == 1, 0, NA)) |>
    dplyr::arrange(.data$spell, .data$idnew) |>
    dplyr::mutate(
      begin = ifelse(is.na(.data$begin), .data$end - .data$weight, .data$begin),
      left = .data$left - 1
    ) |>
    dplyr::arrange(.data$idnew, .data$spell) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      left = ifelse(is.na(.data$left), 0, .data$left),
      right = ifelse(is.na(.data$right), 0, .data$right)
    )

  if (weighted == FALSE) {
    dt2 <- dt2 |>
      dplyr::mutate(begin = .data$begin + .5,
                    end = .data$end + .5)
  }


  if (is.null(group)) dt2$group <- 1
  if (is.null(group)) group <- dt2$group

  ybrks <- dt2 |>
    dplyr::distinct(.data$idnew, .keep_all = T) |>
    dplyr::mutate(
      breaks = (.data$begin + .data$end) / 2,
      breaks = ifelse(.data$begin == .data$end, .data$breaks + 1,
                      .data$breaks
      )
    )


  ylabspec <- purrr::map(
    grinorder,
    ~ dt2 |>
      dplyr::filter(.data$group == .x) |>
      dplyr::summarise(
        group = dplyr::first(.data$group),
        maxwgt = max(.data$end),
        nseq = dplyr::n_distinct(.data$idnew)
      )
  ) |>
    dplyr::bind_rows()


  scalebreaks <- purrr::map(
    grinorder,
    ~ ybrks |>
      dplyr::filter(.data$group == .x) |>
      dplyr::pull(.data$breaks)
  )

  scalelabels <- purrr::map(
    grinorder,
    ~ ybrks |>
      dplyr::filter(.data$group == .x) |>
      dplyr::transmute(laby = dplyr::row_number()) |>
      dplyr::pull()
  )

  grsize <- purrr::map(scalelabels, max)

  scalelabels <- purrr::map(
    scalelabels,
    ~ pretty(.x)
  ) |>
    purrr::map(~ {
      .x[1] <- 1
      .x
    }) |>
    purrr::map2(
      grsize,
      ~ {
        .x[length(.x)] <- .y
        .x
      }
    )

  scalelabels <- purrr::map(scalelabels, ~ .x[(.x %% 1 == 0)])

  scalebreaks <- purrr::map2(
    scalebreaks, scalelabels,
    ~ .x[.y]
  )

  if (facet_scale == "fixed") {
    maxyidx <- purrr::map(scalebreaks, max) |>
      unlist() |>
      which.max()

    scalebreaks <- scalebreaks[maxyidx]
    scalelabels <- scalelabels[maxyidx]
  }

  scales <- purrr::map2(
    scalebreaks,
    scalelabels,
    ~ scale_y_continuous(
      expand = expansion(add = 0),
      breaks = .x,
      labels = .y,
      guide = guide_axis(check.overlap = TRUE)
    )
  )


  if (nrow(ylabspec) == 1 & weighted == TRUE) {
    ylabspec <- glue::glue("{ylabspec$nseq} sequences",
                           "(weighted n={round(ylabspec$maxwgt,2)})")
  } else if (nrow(ylabspec) == 1 & weighted == FALSE) {
    ylabspec <- glue::glue("# sequences (n = {ylabspec$nseq})")
  } else if (weighted == TRUE) {
    ylabspec <- glue::glue("{ylabspec$group} \n({ylabspec$nseq} sequences; ",
                           "weighted n={round(ylabspec$maxwgt,2)})")
  } else {
    ylabspec <- glue::glue("{ylabspec$group} (n={ylabspec$nseq})")
  }

  grouplabspec <- dplyr::tibble(
    group = forcats::fct_inorder(grinorder),
    grouplab = forcats::fct_inorder(ylabspec)
  )

  if (no.n == TRUE) {
    grouplabspec <- grouplabspec |>
      dplyr::mutate(grouplab = .data$group)
    ylabspec <- "# sequences"
  }

  suppressMessages(
    if (nrow(grouplabspec) > 1) {

      if (!is.factor(dt2$group)) dt2$group <- factor(dt2$group)

      dt2 <- dt2 |>
        dplyr::full_join(grouplabspec)
    }
  )


  if ("Missing" %in% dt2$states == TRUE) {
    cpal <- c(
      attributes(seqdata)$cpal,
      attributes(seqdata)$missing.color
    )
  } else {
    cpal <- attributes(seqdata)$cpal
  }


  kbreaks <- 1:length(attributes(seqdata)$names)

  xbrks <- pretty(1:length(kbreaks))
  xbrks[1] <- 1
  xbrks[length(xbrks)] <- length(kbreaks)

  if (xbrks[length(xbrks)] == xbrks[length(xbrks) - 1] + 1) {
    xbrks <- xbrks[xbrks != xbrks[length(xbrks) - 1]]
  }
  if (xbrks[1] == xbrks[2] - 1) {
    xbrks <- xbrks[xbrks != xbrks[2]]
  }

  kbreaks <- kbreaks[xbrks]
  klabels  <- attributes(seqdata)$names[xbrks]


  dt2 <- dt2 |>
    dplyr::mutate(
      aux = .data$right - .data$left,
      aux2 = .data$aux,
      aux = ifelse(.data$aux == 0, 1, .data$aux)
    ) |>
    tidyr::uncount(.data$aux) |>
    dplyr::select(-.data$aux) |>
    dplyr::group_by(.data$idnew) |>
    dplyr::mutate(
      left = dplyr::row_number() - 1,
      right = ifelse(.data$aux2 == 0, .data$left, .data$left + 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$aux2)


  dt2 <- dt2 |>
      dplyr::mutate(x = rep(factor(1:length(attributes(seqdata)$names)),
                            length.out = nrow(dt2)),
                    left = .data$left +.5,
                    right = .data$right +.5)


  if (border == FALSE) {
    suppressMessages(
      ggiplot <- dt2 |>
        ggplot(aes(x = .data$x,
                   xmin = .data$left, xmax = .data$right,
                   ymin = .data$begin, ymax = .data$end,
                   fill = .data$states, colour = .data$states
        )) +
        geom_rect() +
        scale_fill_manual(values = cpal, drop = FALSE) +
        scale_color_manual(values = cpal, drop = FALSE) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          legend.title = element_blank()
        )
    )
  } else {

    suppressMessages(
      ggiplot <- dt2 |>
        ggplot(aes(x = .data$x,
                   xmin = .data$left, xmax = .data$right,
                   ymin = .data$begin, ymax = .data$end,
                   fill = .data$states
        )) +
        geom_rect(colour = "black") +
        scale_fill_manual(values = cpal, drop = FALSE) +
        scale_color_manual(values = cpal, drop = FALSE) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          legend.title = element_blank()
        )
    )
  }

  grsize <- length(unique(dt2$group))


  if (grsize > 1) {
    suppressMessages(
      ggiplot <- ggiplot +
        facet_wrap(~ .data$grouplab,
                   scales = facet_scale,
                   ncol = facet_ncol,
                   nrow = facet_nrow,
                   ...
        ) +
        labs(y = ifelse(weighted == TRUE,
                        "# weighted sequences",
                        "# sequences"
        )) +
        theme(panel.spacing = unit(2, "lines"),
              strip.text.x = element_text(margin = margin( b = 10, t = 0))) +
        ggh4x::facetted_pos_scales(y = scales)
    )
  } else {
    suppressMessages(
      ggiplot <- ggiplot +
        scales +
        labs(y = ylabspec)
    )
  }

  if (grsize > 1 & facet_scale == "fixed" & weighted == TRUE) {
    ggiplot <- ggiplot +
      labs(y = "weighted sequences") +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  }

  suppressMessages(
    ggiplot <- ggiplot +
      scale_x_discrete(
        breaks = kbreaks,
        labels = klabels,
        guide = guide_axis(check.overlap = TRUE),
        expand = expansion(add = 0 )
      ) +
      labs(x = "") +
      theme(
        axis.title.y = element_text(vjust = +3),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = margin(15, 15, 10, 15),
        axis.line.x = element_line(linewidth = .3),
        axis.ticks = element_line(linewidth = .3)
      )
  )


  return(ggiplot)
}
