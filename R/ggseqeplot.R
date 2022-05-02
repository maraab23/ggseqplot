#' Sequence Entropy Plot
#'
#' Function for plotting the development of cross-sectional entropies across sequence positions with \code{\link[ggplot2]{ggplot2}} instead of base
#' R's \code{\link[base]{plot}} function that is used by \code{\link[TraMineR:seqplot]{TraMineR::seqplot}}. Other than in \code{\link[TraMineR:seqHtplot]{TraMineR::seqHtplot}} group-specific entropy
#' lines are displayed in a common plot (just like in \code{\link[TraMineRextras:seqplot.tentrop]{TraMineRextras::seqplot.tentrop}}.
#'
#' @param seqdata State sequence object (class \code{stslist}) created with the \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function.
#' @param group If grouping variable is specified plot shows one line for each group
#' @param weighted Controls if weights (specified in \\code{\\link[TraMineR:seqdef]{TraMineR::seqdef}}) should be used. Default is \code{TRUE}, i.e. if available weights are used
#' @param with.missing Specifies if missing states should be considered when computing the entropy index (default is \code{FALSE}).
#' @param linewidth Specifies the with of the entropy line; default is \code{1}
#'
#' @return A line plot of entropy values at each sequence position. If stored as object the resulting list
#' object also contains the data (long format) used for rendering the plot
#' @export
#'
#' @details The function uses \code{\link[TraMineR:seqstatd]{TraMineR::seqstatd}} to compute entropies. Obviously this requires that the
#' input data (\code{seqdata}) is stored as state sequence object (class \code{stslist}) created with the \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function.
#'
#' @examples
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' # Examples from TraMineR::seqplot
#'
#' library(TraMineR)
#' library(ggplot2)
#'
#' # actcal data set
#' data(actcal)
#'
#' # We use only a sample of 300 cases
#' set.seed(1)
#' actcal <- actcal[sample(nrow(actcal),300),]
#' actcal.lab <- c("> 37 hours", "19-36 hours", "1-18 hours", "no work")
#' actcal.seq <- seqdef(actcal,13:24,labels=actcal.lab)
#'
#' # ex1 using weights
#' data(ex1)
#' ex1.seq <- seqdef(ex1, 1:13, weights=ex1$weights)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#'
#' # sequences sorted by age in 2000 and grouped by sex
#' # with TraMineR::seqplot (entropies shown in two separate plots)
#' seqHtplot(actcal.seq, group=actcal$sex)
#' # with ggseqplot (entropies shown in one plot)
#' ggseqeplot(actcal.seq, group=actcal$sex)
#'
#' # sequences of unequal length with missing state, and weights
#' seqHtplot(ex1.seq)
#' ggseqeplot(ex1.seq)
#'
#' @import ggplot2
ggseqeplot <- function(seqdata,
                       group = NULL,
                       weighted = TRUE,
                       with.missing = FALSE,
                       linewidth = 1) {


  if (!inherits(seqdata, "stslist"))
    stop("data is not a sequence object, use 'TraMineR::seqdef' to create one")


  if (!is.null(group) & (length(group) != nrow(seqdata)))
    stop("length of group vector must match number of rows of seqdata")


  if(!is.logical(weighted) | !is.logical(with.missing))
    stop("the arguments `weighted`, and `with.missing` have to be objects of type logical")

  if (is.null(attributes(seqdata)$weights)) weighted <- FALSE

  if (is.null(group)) group <- 1

  cpal <- c("#4E79A7FF", "#F28E2BFF", "#E15759FF",
            "#76B7B2FF", "#59A14FFF", "#EDC948FF",
            "#B07AA1FF", "#FF9DA7FF", "#9C755FFF",
            "#BAB0ACFF")


  grsize <- length(unique(group))


  eplotdata <- purrr::map(unique(group),
                          ~TraMineR::seqstatd(seqdata[group == .x,],
                                              weighted = weighted,
                                              with.missing = with.missing)$Entropy |>
                            dplyr::as_tibble(rownames = "k") |>
                            dplyr::mutate(group = .x, .before = 1)) |>
    dplyr::bind_rows() |>
    dplyr::mutate(k = factor(.data$k, levels = unique(.data$k))) |>
    dplyr::mutate(x = factor(as.integer(.data$k)), .after = .data$k) |>
    dplyr::rename(entropy = .data$value) |>
    dplyr::mutate(group = as.factor(group))


  kbreaks <- 1:(length(attributes(seqdata)$names))

  xbrks <- pretty(1:length(kbreaks))
  xbrks[1] <- 1
  xbrks[length(xbrks)] <- length(kbreaks)

  if (xbrks[length(xbrks)] == xbrks[length(xbrks)-1]+1) {
    xbrks <- xbrks[xbrks != xbrks[length(xbrks)-1]]
  }

  if (xbrks[1] == xbrks[2]-1) {
    xbrks <- xbrks[xbrks != xbrks[2]]
  }

  kbreaks <- kbreaks[xbrks]
  klabels <- attributes(seqdata)$names[xbrks]


  eplotdata |> ggplot(aes(x= .data$x, y= .data$entropy, group=.data$group)) +
    geom_line(aes(color = .data$group,
                  linetype = .data$group),
              size=linewidth) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, by = .2)) +
    scale_color_manual(values= `if`(grsize ==1,"black",cpal),
                       guide = ifelse(grsize ==1,"none", "legend")) +
    scale_linetype(guide = ifelse(grsize ==1,"none", "legend")) +
    scale_x_discrete(
      breaks = kbreaks,
      labels = klabels,
      guide = guide_axis(check.overlap = TRUE)) +
    labs(x = "", y = "Entropy") +
    theme_minimal() +
    theme(axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=11))
}
