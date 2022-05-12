#' Representative Sequence plot
#'
#' Function for rendering representative sequence plots with \code{\link[ggplot2]{ggplot2}} instead of base
#' R's \code{\link[base]{plot}} function that is used by \code{\link[TraMineR:seqplot]{TraMineR::seqplot}}.
#' Note that \code{ggseqrplot} uses \code{\link[patchwork]{patchwork}} to combine the different components of the plot.
#' For further details on representative sequence plots see documentation of
#' \code{\link[TraMineR:seqplot]{TraMineR::seqplot}} and related documentation files.

#'
#' @inheritParams TraMineR::seqrep
#' @param seqdata State sequence object (class \\code{stslist}) created with
#' the \\code{\\link[TraMineR:seqdef]{TraMineR::seqdef}} function.
#' @param weighted Controls if weights (specified in
#' \\code{\\link[TraMineR:seqdef]{TraMineR::seqdef}}) should be used. Default
#' is \\code{TRUE}, i.e. if available weights are used
#' @param diss pairwise dissimilarities between sequences in \code{seqdata} (see \code{TraMineRextras::seqdist})
#' @param border if \code{TRUE} bars are plotted with black outline
#' @param proportional if \\code{TRUE} (default), the sequence heights are
#' displayed proportional to the number of represented sequences
#' @param stats if \\code{TRUE} (default), mean discrepancy in each subset
#' defined by all sequences attributed to one representative sequence and the
#' mean distance to this representative sequence are displayed.
#' @param colored.stats specifies if representatives in stats plot should be
#' color coded; only recommended if number of representatives is small;
#' if set to \code{NULL} (default) colors are used if n rep. <= 10;
#' use \code{TRUE} or \code{FALSE} to change manually
#'
#' @return A representative sequence plot using \code{\link[ggplot2]{ggplot}}.
#' @export
#' @importFrom patchwork plot_layout
#'
#' @examples
#' library(TraMineR)
#' ## Defining a sequence object with the data in columns 10 to 25
#' ## (family status from age 15 to 30) in the biofam data set
#' data(biofam)
#' biofam.lab <- c("Parent", "Left", "Married", "Left+Marr",
#' "Child", "Left+Child", "Left+Marr+Child", "Divorced")
#' biofam.seq <- seqdef(biofam, 10:25, labels=biofam.lab)
#'
#' ## Computing the distance matrix
#' costs <- seqsubm(biofam.seq, method="TRATE")
#' biofam.om <- seqdist(biofam.seq, method="OM", sm=costs)
#'
#' ## Representative sequence  plot (using defaults)
#' ## ... with TraMineR::seqplot
#' seqrplot(biofam.seq, diss = biofam.om)
#'
#' ## ... with ggseqrplot
#' ggseqrplot(biofam.seq, diss = biofam.om)
ggseqrplot <- function(seqdata,
                       diss,
                       criterion = "density",
                       coverage = .25,
                       nrep = NULL,
                       pradius = 0.10,
                       dmax = NULL,
                       border = FALSE,
                       proportional = TRUE,
                       weighted = TRUE,
                       stats = TRUE,
                       colored.stats = NULL) {



  if (!is.null(nrep) && (nrep%%1!=0 | nrep == 0)) {
    stop("nrep has to be a positive whole number")
  }


  seq.rep <- TraMineR::seqrep(seqdata,
                              diss = diss,
                              criterion = criterion,
                              weighted = weighted,
                              coverage = coverage,
                              nrep = nrep,
                              pradius = pradius,
                              dmax = dmax
  )


  if (is.null(colored.stats) & nrow(seq.rep) <=10) colored.stats <- TRUE
  if (is.null(colored.stats) & nrow(seq.rep) > 10) colored.stats <- FALSE

  if (proportional == TRUE) {
    attributes(seq.rep)$weights <- attributes(seq.rep)$Statistics[1:nrow(seq.rep),4]
  }

  aux <- ggseqiplot(seq.rep)$data
  ybreaks <- (unique(aux$begin) + unique(aux$end)) / 2


  coverage <- attributes(seq.rep)$Statistics[nrow(seq.rep) + 1, 4]
  coverage <- paste0(round(coverage, 2), "%")


  rplotdata <- attributes(seq.rep)$Statistics |>
    dplyr::transmute(
      id = dplyr::row_number(),
      `Discrepancy (mean dist. to center)` = .data$V,
      `Mean dist. to representative seq.` = .data$MD
    ) |>
    dplyr::filter(.data$id != max(.data$id)) |>
    tidyr::pivot_longer(cols = -.data$id)


  if (colored.stats == TRUE & stats == TRUE) {
    p2 <- rplotdata |>
      ggplot(aes(
        x = .data$value,
        y = "1",
        label = .data$id,
        fill = factor(.data$id)
      )) +
      ggrepel::geom_label_repel(
        size = 3, colour = "black",
        alpha = .8, point.size = NA,
        direction = "y"
      ) +
      scale_y_discrete(
        labels = NULL,
        expand = expansion(add = 1.5)
      ) +
      facet_wrap(vars(.data$name),
        nrow = 2,
        scales = "fixed"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank()
      )


    color <- ggplot_build(p2)$data[[1]] |>
      dplyr::group_by(.data$label) |>
      dplyr::summarise(col = dplyr::first(.data$fill)) |>
      dplyr::pull(.data$col)

    labs <- glue::glue("<b style='color:{color}'>{1:length(color)}</b>")

    suppressMessages(
      p1 <- ggseqplot::ggseqiplot(seq.rep) +
        scale_y_continuous(
          breaks = ybreaks,
          labels = labs,
          guide = guide_axis(check.overlap = TRUE),
          expand = expansion(add = c(0, 0))
        ) +
        labs(caption = glue::glue("Coverage = {coverage}; Criterion = {criterion}")) +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = ggtext::element_markdown(size = 11)
        )
    ) } else {
    suppressMessages(
      p1 <- ggseqiplot(seq.rep, border = border) +
        scale_y_continuous(
          breaks = ybreaks,
          labels = 1:nrow(seq.rep),
          guide = guide_axis(check.overlap = TRUE),
          expand = expansion(add = c(0, 0))
        ) +
        labs(caption = glue::glue("Coverage = {coverage}; Criterion = {criterion}")) +
        theme(axis.title.y = element_blank())
    )

    p2 <- rplotdata |>
      ggplot(aes(
        x = .data$value,
        y = "1",
        label = .data$id
      )) +
      ggrepel::geom_text_repel(
        size = 4.5,
        fontface = "bold",
        point.size = NA,
        direction = "y"
      ) +
      scale_y_discrete(
        labels = NULL,
        expand = expansion(add = 1.5)
      ) +
      facet_wrap(vars(.data$name), nrow = 2, scales = "fixed") +
      theme_minimal() +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank()
      )
  }

  if (stats == FALSE) {
    rplot <- p1
  } else if (colored.stats == FALSE) {
    p1 <- p1 + theme(axis.text.y = element_text(
      color = "black",
      size = 12,
      face = "bold"
    ))
    rplot <- p2 / p1 +
      plot_layout(heights = c(1, 2))
  } else {
    rplot <- p2 / p1 +
      plot_layout(heights = c(1, 2))
  }

  return(rplot)
}
