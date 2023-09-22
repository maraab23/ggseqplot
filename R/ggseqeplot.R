#' Sequence Entropy Plot
#'
#' Function for plotting the development of cross-sectional entropies across
#' sequence positions with \code{\link[ggplot2]{ggplot2}} \insertCite{wickham2016}{ggseqplot}
#' instead of base R's \code{\link[base]{plot}} function that is used by
#' \code{\link[TraMineR:seqplot]{TraMineR::seqplot}} \insertCite{gabadinho2011}{ggseqplot}.
#' Other than in \code{\link[TraMineR:seqHtplot]{TraMineR::seqHtplot}} group-specific entropy
#' lines are displayed in a common plot.
#'
#' @param seqdata State sequence object (class \code{stslist}) created with the \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function.
#' @param group If grouping variable is specified plot shows one line for each group
#' @param weighted Controls if weights (specified in \code{\link[TraMineR:seqdef]{TraMineR::seqdef}}) should be used. Default is \code{TRUE}, i.e. if available weights are used
#' @param with.missing Specifies if missing states should be considered when computing the entropy index (default is \code{FALSE}).
#' @param linewidth Specifies the with of the entropy line; default is \code{1}
#' @param gr.linetype Specifies if line type should vary by group; hence only relevant if
#' group argument is specified; default is \code{FALSE}
#' @param linecolor Specifies color palette for line(s); default is \code{"Okabe-Ito"} which contains up to 9 colors (first is black).
#' if more than 9 lines should be rendered, user has to specify an alternative color palette
#'
#' @return A line plot of entropy values at each sequence position. If stored as object the resulting list
#' object also contains the data (long format) used for rendering the plot.
#'
#'
#' @export
#'
#' @details The function uses \code{\link[TraMineR:seqstatd]{TraMineR::seqstatd}}
#' to compute entropies. This requires that the input data (\code{seqdata})
#' are stored as state sequence object (class \code{stslist}) created with the
#'  \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function.
#'
#' The entropy values are plotted with \code{\link[ggplot2]{geom_line}}. The data
#' and specifications used for rendering the plot can be obtained by storing the
#' plot as an object. The appearance of the plot can be adjusted just like with
#' every other ggplot (e.g., by changing the theme or the scale using \code{+} and
#' the respective functions).
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
#' # sequences sorted by age in 2000 and grouped by sex
#' # with TraMineR::seqplot (entropies shown in two separate plots)
#' seqHtplot(actcal.seq, group = actcal$sex)
#' # with ggseqplot (entropies shown in one plot)
#' ggseqeplot(actcal.seq, group = actcal$sex)
#' ggseqeplot(actcal.seq, group = actcal$sex, gr.linetype = TRUE)
#'
#' # manual color specification
#' ggseqeplot(actcal.seq, linecolor = "darkgreen")
#' ggseqeplot(actcal.seq, group = actcal$sex,
#'            linecolor = c("#3D98D3FF", "#FF363CFF"))
#' @import ggplot2
#' @importFrom rlang .data
ggseqeplot <- function(seqdata,
                       group = NULL,
                       weighted = TRUE,
                       with.missing = FALSE,
                       linewidth = 1,
                       linecolor = "Okabe-Ito",
                       gr.linetype = FALSE) {
  if (!inherits(seqdata, "stslist")) {
    stop("data are not stored as sequence object, use 'TraMineR::seqdef' to create one")
  }


  if (!is.null(group) & (length(group) != nrow(seqdata))) {
    stop("length of group vector must match number of rows of seqdata")
  }


  if (!is.logical(weighted) | !is.logical(with.missing)) {
    stop("the arguments `weighted`, and `with.missing` have to be objects of type logical")
  }

  if (is.null(attributes(seqdata)$weights)) weighted <- FALSE

  if (is.null(group)) group <- 1

  grsize <- length(unique(group))

  if (length(linecolor) == 1 && linecolor == "Okabe-Ito") {
    cpal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
              "#0072B2", "#D55E00", "#CC79A7", "#999999")
  }

  if (length(linecolor) != grsize && linecolor != "Okabe-Ito") {
    stop("Length of specified color palette must match the number of distinct groups")
  }

  if (grsize >= 10 && linecolor == "Okabe-Ito") {
   stop("group vector with 10 or more distinct values
   you have to manually specify a palette (`linecolor` argument) with the correct number
   of colors (or - possibly better - reduce number of groups to a reasonable size)")
  }

  if (length(linecolor) == 1 && linecolor == "Okabe-Ito") {
    linecolor <- cpal[1:grsize]
  }


  eplotdata <- purrr::map(
    unique(group),
    ~ TraMineR::seqstatd(seqdata[group == .x, ],
      weighted = weighted,
      with.missing = with.missing
    )$Entropy |>
      dplyr::as_tibble(rownames = "k") |>
      dplyr::mutate(group = .x, .before = 1)
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(k = factor(.data$k, levels = unique(.data$k))) |>
    dplyr::mutate(x = factor(as.integer(.data$k)), .after = .data$k) |>
    dplyr::rename(entropy = .data$value) |>
    dplyr::mutate(group = as.factor(group))


  kbreaks <- 1:(length(attributes(seqdata)$names))

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
  klabels <- attributes(seqdata)$names[xbrks]

  if (gr.linetype == TRUE) {
    ggeplot <- eplotdata |>
      ggplot(aes(x = .data$x, y = .data$entropy, group = .data$group)) +
      geom_line(aes(color = .data$group,
                    linetype = .data$group),
                linewidth =  linewidth)
  } else {
    ggeplot <- eplotdata |>
      ggplot(aes(x = .data$x, y = .data$entropy, group = .data$group)) +
      geom_line(aes(color = .data$group),
                linewidth =  linewidth)
  }

  ggeplot <- ggeplot +
    scale_y_continuous(
      expand = expansion(add = 0),
      limits = c(0, 1),
      breaks = seq(0, 1, by = .2)
    ) +
    scale_color_manual(
      values = linecolor,
      guide = ifelse(grsize == 1, "none", "legend")
    ) +
    scale_linetype(guide = ifelse(grsize == 1, "none", "legend")) +
    scale_x_discrete(
      expand = expansion(add = .15),
      breaks = kbreaks,
      labels = klabels,
      guide = guide_axis(check.overlap = TRUE)
    ) +
    labs(x = "", y = "Entropy") +
    theme_minimal() +
    theme(
      axis.title.y = element_text(vjust = +3),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 11),
      panel.grid.major.x = element_blank(),
      axis.line.x = element_line(linewidth = .3),
      axis.ticks = element_line(linewidth = .3)
    )

  ggeplot <- ggeplot +
    theme(plot.margin = margin(15, 15, 10, 15))

  return(ggeplot)
}
