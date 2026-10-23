#' Decorated Parallel Coordinate Plot
#'
#' Function for rendering decorated parallel coordinate plots of state or event
#' sequences with \code{\link[ggplot2]{ggplot2}} \insertCite{wickham2016}{ggseqplot}
#' instead of base R's \code{\link[base]{plot}} function that is used by
#' \code{\link[TraMineR:seqpcplot]{TraMineR::seqpcplot}}
#' \insertCite{gabadinho2011,buergin2014}{ggseqplot}.
#'
#' @param seqdata State sequence object (class \code{stslist}) created with the
#' \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function or event sequence
#' object (class \code{seqelist}) created with the
#' \code{\link[TraMineR:seqecreate]{TraMineR::seqecreate}} function.
#' @param group A vector of the same length as the sequence data indicating group
#' membership. When not NULL, a distinct plot is generated for each level of group.
#' @param no.n specifies if number of (weighted) sequences is shown above the
#' plot (default is \code{FALSE}, i.e. the number is shown). If \code{TRUE} only
#' the group labels are shown (or no label at all if \code{group} is not specified).
#' @param linewidth Maximum width of the lines connecting the states/events, i.e.
#' the width of the line representing the most frequent transition; default is \code{1}.
#' The width of all other lines is scaled relative to this value.
#' @param facet_ncol Number of columns in faceted (i.e. grouped) plot
#' @param facet_nrow Number of rows in faceted (i.e. grouped) plot
#' @param ... additional arguments passed on to
#' \code{\link[TraMineR:seqpcplot]{TraMineR::seqpcplot}}, which computes the
#' coordinates of the plot, e.g. \code{order.align}, \code{ltype},
#' \code{embedding}, \code{filter}, \code{cpal}, \code{alphabet},
#' \code{weighted}, \code{grid.scale}, or \code{cex}.
#'
#' @return A decorated parallel coordinate plot created by using
#' \code{\link[ggplot2]{ggplot2}}. If stored as object the resulting list object
#' (of class gg and ggplot) also contains the data used for rendering the squares
#' (states/events) of the plot; the data of the lines are stored in the layer
#' rendering the lines.
#'
#' @export
#'
#' @details Decorated parallel coordinate plots \insertCite{buergin2014}{ggseqplot}
#' display each distinct sequence as a line connecting its successive states
#' (or events). The x-axis shows the position (or time) and the y-axis the
#' states (or events). The size of the squares and the width of the lines are
#' proportional to the (weighted) number of sequences following the respective
#' pattern. Patterns that are followed by several sequences are shifted slightly
#' (jittered) within the squares of the background grid to keep them distinguishable.
#'
#' The function calls \code{\link[TraMineR:seqpcplot]{TraMineR::seqpcplot}} with
#' \code{plot = FALSE} to compute the coordinates, sizes, and colors of all plot
#' elements. Hence, all arguments of
#' \code{\link[TraMineR:seqpcplot]{TraMineR::seqpcplot}} that affect the
#' computation (e.g., \code{order.align} or \code{filter}) can be passed via
#' \code{...}. The squares are rendered with \code{\link[ggplot2]{geom_rect}},
#' the lines with \code{\link[ggplot2]{geom_segment}}. If \code{group} is
#' specified the group-specific plots are produced by using
#' \code{\link[ggplot2]{facet_wrap}}.
#'
#' Note that \code{TraMineR::seqpcplot} computes the line widths relative to the size of the
#' graphics device, whereas \code{ggplot2} uses absolute line widths. The line
#' widths are therefore scaled relative to the most frequent transition and
#' can be adjusted with the \code{linewidth} argument.
#'
#' The data and specifications used for rendering the plot can be obtained by
#' storing the plot as an object. The appearance of the plot can be adjusted
#' just like with every other ggplot (e.g., by changing the theme or the scale
#' using \code{+} and the respective functions).
#'
#' @author Marcel Raab
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' library(TraMineR)
#'
#' # Use example data from TraMineR: biofam data set
#' data(biofam)
#' biofam.lab <- c("Parent", "Left", "Married", "Left+Marr",
#'                 "Child", "Left+Child", "Left+Marr+Child", "Divorced")
#'
#' # We use only the first 20 cases
#' biofam.seq <- seqdef(biofam[, 10:25], labels = biofam.lab,
#'                      weights = biofam$wp00tbgs)[1:20, ]
#'
#' # parallel coordinate plot of the state sequences (time on x-axis)
#' # with TraMineR::seqpcplot
#' seqpcplot(biofam.seq, order.align = "time")
#' # with ggseqplot
#' ggseqpcplot(biofam.seq, order.align = "time")
#'
#' # distinct successive states (order on x-axis), grouped by sex
#' ggseqpcplot(biofam.seq, group = biofam$sex[1:20], order.align = "first")
#' # ... without number of sequences
#' ggseqpcplot(biofam.seq, group = biofam$sex[1:20], order.align = "first",
#'             no.n = TRUE)
#'
#' # event sequences
#' biofam.eseq <- seqecreate(biofam.seq, tevent = "state")
#' ggseqpcplot(biofam.eseq, order.align = "first", alphabet = biofam.lab)
#'
#' @import ggplot2
#' @importFrom rlang .data
ggseqpcplot <- function(seqdata,
                        group = NULL,
                        no.n = FALSE,
                        linewidth = 1,
                        facet_ncol = NULL,
                        facet_nrow = NULL,
                        ...) {
  if (!inherits(seqdata, c("stslist", "seqelist"))) {
    stop("data are not stored as sequence object, use 'TraMineR::seqdef' or 'TraMineR::seqecreate' to create one")
  }

  # number of sequences (event sequence objects have no rows)
  nseq <- ifelse(inherits(seqdata, "stslist"), nrow(seqdata), length(seqdata))

  if (!is.null(group) && (length(group) != nseq)) {
    stop("length of group vector must match number of rows of seqdata")
  }

  if (!is.numeric(linewidth) || linewidth <= 0) {
    stop("`linewidth` must be a positive number")
  }

  if (!is.logical(no.n)) {
    stop("the argument `no.n` has to be an object of type logical")
  }

  if (!is.null(facet_ncol) && as.integer(facet_ncol) != facet_ncol) {
    stop("`facet_ncol` must be NULL or an integer.")
  }

  if (!is.null(facet_nrow) && as.integer(facet_nrow) != facet_nrow) {
    stop("`facet_nrow` must be NULL or an integer.")
  }

  if (inherits(group, "haven_labelled")) {
    group_name <- deparse(substitute(group))
    group <- haven::as_factor(group)
    cli::cli_warn(c("i" = "group vector {.arg {group_name}} is of class {.cls haven_labelled} and has been converted into a factor"))
  }


  # compute coordinates, sizes, and colors with TraMineR ----------------------

  pc <- TraMineR::seqpcplot(seqdata, group = group, plot = FALSE, ...)

  # size of the squares in the background grid
  gridsize <- sqrt(pc$grid.scale)

  # shift of each trajectory within the grid squares (see TraMineR:::plot.seqpcplot)
  trajshift <- sqrt(pc$propid.use.max) * gridsize * pc$cex * pc$ngrid0 / pc$ngrid / 2

  # position within the grid square, given the jitter of a trajectory
  jitterpos <- function(jitter, traj) {
    -0.5 + jitter * gridsize + (1 - gridsize) / 2 + trajshift[traj]
  }

  # drawing order of trajectories (as in TraMineR)
  trajorder <- function(i) {
    order(pc$propid.use[, i], decreasing = (pc$lorder == "background"))
  }


  # data for each group (TraMineR stores group-specific columns n.1, n.2, ...)

  # labels of the facets: TraMineR's titles ("group, n = ...") or group names only
  if (no.n == TRUE) {
    grouplabs <- factor(pc$grouplev, levels = unique(pc$grouplev))
  } else {
    grouplabs <- factor(pc$main, levels = unique(pc$main))
  }

  squaredata <- purrr::map(pc$which, function(i) {
    pc$pts |>
      dplyr::mutate(
        n = .data[[pc$frqcols[i]]],
        width = .data[[pc$wdtcols[i]]] * pc$cex,
        color = as.vector(.data[[pc$colcols[i]]]),
        xcenter = .data$x + jitterpos(.data$xjitter, .data$traj),
        ycenter = .data$y + jitterpos(.data$yjitter, .data$traj),
        drawpos = match(.data$traj, trajorder(i)),
        group = grouplabs[i]
      ) |>
      dplyr::filter(.data$n > 0)
  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$drawpos)

  if (!is.null(pc$lns)) {
    linedata <- purrr::map(pc$which, function(i) {
      pc$lns |>
        dplyr::mutate(
          n = .data[[pc$frqcols[i]]],
          lwd = .data[[pc$lwdcols[i]]],
          color = as.vector(.data[[pc$colcols[i]]]),
          xstart = .data$x0 + jitterpos(.data$x0jitter, .data$traj),
          ystart = .data$y0 + jitterpos(.data$y0jitter, .data$traj),
          xend = .data$x1 + jitterpos(.data$x0jitter, .data$traj),
          yend = .data$y1 + jitterpos(.data$y0jitter, .data$traj),
          drawpos = match(.data$traj, trajorder(i)),
          group = grouplabs[i]
        ) |>
        dplyr::filter(.data$n > 0)
    }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(lwd = .data$lwd / max(.data$lwd) * linewidth) |>
      dplyr::arrange(.data$drawpos)
  } else {
    linedata <- NULL
  }

  # background grid
  griddata <- tidyr::expand_grid(group = grouplabs[pc$which],
                                 dplyr::as_tibble(pc$backrect))

  # squares for sequences without any event/state (lower left corner)
  nobsdata <- purrr::map(pc$which, function(i) {
    propnobs <- pc$propid.tot[pc$ntraj + 1, i]
    if (propnobs > 0) {
      maxpts <- which.max(pc$pts$n.1)
      size <- pc$pts$wdt.1[maxpts] *
        sqrt(propnobs / (pc$pts$n.1[maxpts] / pc$wid.group.tot[1]))
      dplyr::tibble(group = grouplabs[i],
                    xmin = 1 - (gridsize / 2 + size * pc$cex),
                    xmax = 1 - gridsize / 2,
                    color = pc$col.nobs[i])
    }
  }) |>
    dplyr::bind_rows()


  # plot ----------------------------------------------------------------------

  ggpcplot <- squaredata |>
    ggplot() +
    geom_rect(data = griddata,
              aes(xmin = .data$xgrid - gridsize / 2,
                  xmax = .data$xgrid + gridsize / 2,
                  ymin = .data$ygrid - gridsize / 2,
                  ymax = .data$ygrid + gridsize / 2),
              fill = pc$grid.fill, color = pc$grid.border,
              linewidth = pc$grid.lwd / 2)

  if (nrow(nobsdata) > 0) {
    ggpcplot <- ggpcplot +
      geom_rect(data = nobsdata,
                aes(xmin = .data$xmin, xmax = .data$xmax,
                    ymin = .data$xmin, ymax = .data$xmax,
                    fill = .data$color),
                color = NA)
  }

  ggpcplot <- ggpcplot +
    geom_rect(aes(xmin = .data$xcenter - .data$width / 2,
                  xmax = .data$xcenter + .data$width / 2,
                  ymin = .data$ycenter - .data$width / 2,
                  ymax = .data$ycenter + .data$width / 2,
                  fill = .data$color),
              color = NA)

  if (!is.null(linedata)) {
    ggpcplot <- ggpcplot +
      geom_segment(data = linedata,
                   aes(x = .data$xstart, y = .data$ystart,
                       xend = .data$xend, yend = .data$yend,
                       color = .data$color, linewidth = .data$lwd),
                   lineend = "butt")
  }

  ggpcplot <- ggpcplot +
    scale_fill_identity() +
    scale_color_identity() +
    scale_linewidth_identity() +
    scale_x_continuous(breaks = seq_len(pc$nx),
                       labels = pc$xlevs,
                       expand = expansion(add = 0)) +
    scale_y_continuous(breaks = seq_len(pc$ny),
                       labels = pc$ylevs,
                       expand = expansion(add = 0)) +
    # same scale on both axes keeps the squares quadratic (as in TraMineR)
    coord_fixed(xlim = pc$xlim + c(-.1, .1), ylim = pc$ylim + c(-.1, .1)) +
    facet_wrap(~ .data$group,
               ncol = facet_ncol,
               nrow = facet_nrow) +
    labs(x = pc$xlab, y = pc$ylab) +
    theme_minimal() +
    theme(
      # no grid lines: the squares of the background grid take this role
      panel.grid = element_blank(),
      panel.spacing = unit(2, "lines"),
      strip.text.x = element_text(margin = margin(b = 10, t = 0)),
      plot.margin = margin(15, 15, 10, 15)
    )

  # without groups there is no label left to show if no.n = TRUE
  if (no.n == TRUE && is.null(group)) {
    ggpcplot <- ggpcplot +
      theme(strip.text.x = element_blank())
  }

  return(ggpcplot)
}
