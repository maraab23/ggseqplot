#' Modal State Sequence Plot
#'
#' Function for rendering modal state sequence plot with
#' \code{\link[ggplot2]{ggplot2}} \insertCite{wickham2016}{ggseqplot} instead
#' of base R's \code{\link[base]{plot}} function that is used by
#' \code{\link[TraMineR:seqplot]{TraMineR::seqplot}} \insertCite{gabadinho2011}{ggseqplot}.
#'
#' @eval shared_params()
#' @param barwidth specifies width of bars (default is \code{NULL}); valid range: (0, 1]
#' @param no.n specifies if number of (weighted) sequences is shown (default is \code{TRUE})
#' @param with.missing Specifies if missing states should be considered when computing the state distributions (default is \code{FALSE}).
#' @param border if \code{TRUE} bars are plotted with black outline; default is \code{FALSE} (also accepts \code{NULL})
#' @eval shared_facet()
#'
#' @details The function uses \code{\link[TraMineR:seqmodst]{TraMineR::seqmodst}}
#' to obtain the modal states and their prevalence. This requires that the
#' input data (\code{seqdata}) are stored as state sequence object (class \code{stslist})
#' created with the \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function.
#'
#' The data on the modal states and their prevalences are reshaped to be plotted with
#'  \code{\link[ggplot2:geom_bar]{ggplot2::geom_bar}}. The data
#' and specifications used for rendering the plot can be obtained by storing the
#' plot as an object. The appearance of the plot can be adjusted just like with
#' every other ggplot (e.g., by changing the theme or the scale using \code{+} and
#' the respective functions).
#'
#' @return A modal state sequence plot. If stored as object the resulting list
#' object also contains the data (long format) used for rendering the plot
#' @export
#' @import ggplot2
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
#' # modal state sequence plot; grouped by sex
#' # with TraMineR::seqplot
#' seqmsplot(actcal.seq, group = actcal$sex)
#' # with ggseqplot
#' ggseqmsplot(actcal.seq, group = actcal$sex)
#' # with ggseqplot and some layout changes
#' ggseqmsplot(actcal.seq, group = actcal$sex, no.n = TRUE, border = FALSE, facet_nrow = 2)
#'
#' @importFrom rlang .data
ggseqmsplot <- function(seqdata,
                        no.n = FALSE,
                        barwidth = NULL,
                        group = NULL,
                        weighted = TRUE,
                        with.missing = FALSE,
                        border = FALSE,
                        facet_ncol = NULL,
                        facet_nrow = NULL) {
  if (!inherits(seqdata, "stslist")) {
    stop("data is not a sequence object, use 'TraMineR::seqdef' to create one")
  }

  if (!is.null(group) & (length(group) != nrow(seqdata))) {
    stop("length of group vector must match number of rows of seqdata")
  }

  if (is.null(border)) border <- FALSE

  if (!is.logical(weighted) | !is.logical(with.missing) |
      !is.logical(border) | !is.logical(no.n)) {
    stop("the arguments `no.n`, `weighted`, `with.missing`, and `border` have to be objects of type logical")
  }


  if (is.null(attributes(seqdata)$weights)) weighted <- FALSE

  if (is.factor(group)) {
    group <- forcats::fct_drop(group)
    grinorder <- levels(group)
  } else {
    grinorder <- factor(unique(group))
  }
  if (is.null(group)) grinorder <- factor(1)

  if (is.null(group)) group <- 1


  if (!is.null(facet_ncol) && as.integer(facet_ncol) != facet_ncol) {
    stop("`facet_ncol` must be NULL or an integer.")
  }

  if (!is.null(facet_nrow) && as.integer(facet_nrow) != facet_nrow) {
    stop("`facet_nrow` must be NULL or an integer.")
  }

  if (!is.null(barwidth) && (barwidth <= 0 | barwidth > 1)) {
    stop("`barwidth` must be NULL or a value in the range (0, 1]")
  }


  msplotdata <- purrr::map(grinorder,
                           ~TraMineR::seqmodst(seqdata[group == .x,],
                                               weighted = weighted,
                                               with.missing = with.missing) |>
                             dplyr::as_tibble() |>
                             dplyr::mutate(group = .x, .before = 1))

  msplotdata <- purrr::map(msplotdata,
                           ~tidyr::pivot_longer(.x,
                                                cols = -1,
                                                values_to = "state",
                                                names_to = "xlab") |>
                             dplyr::mutate(value = as.numeric(attributes(.x)$Frequencies)) |>
                             dplyr::mutate(x = factor(dplyr::row_number()),
                                           .before = 2)) |>
    dplyr::bind_rows()


  msplotdata <- msplotdata |>
    dplyr::mutate(state = factor(.data$state,
                                 levels = TraMineR::alphabet(seqdata),
                                 labels = attributes(seqdata)$labels),
                  state = forcats::fct_na_value_to_level(.data$state,
                                                          level = "Missing"
                  ),
                  state = forcats::fct_drop(.data$state, "Missing"), # shouldn't be necessary
                  state = forcats::fct_rev(.data$state),
                  xlab = factor(.data$xlab))


  xandgrouplabs <- xandgrouplab(seqdata = seqdata,
                                weighted = weighted,
                                no.n = no.n,
                                group = group,
                                grinorder = grinorder,
                                ylabprefix = "Rel. Freq.")
  grouplabspec <- xandgrouplabs[[1]]
  ylabspec <- xandgrouplabs[[2]]


  msplotdata <- msplotdata |>
    dplyr::full_join(grouplabspec, by = "group")


  if("Missing" %in% msplotdata$state == TRUE) {
    cpal <- c(attributes(seqdata)$cpal,
              attributes(seqdata)$missing.color)
  } else {
    cpal <- attributes(seqdata)$cpal
  }

  cpal <- rev(cpal)


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


  if (border == FALSE) {
    ggmsplot <- msplotdata |>
      ggplot(aes(fill = .data$state, y = .data$value, x = .data$x)) +
      geom_bar(stat="identity", width = barwidth)
  } else {
    ggmsplot <- msplotdata |>
      ggplot(aes(fill = .data$state, y = .data$value, x = .data$x)) +
      geom_bar(stat="identity",
               width = barwidth,
               color = "black")
  }

  ggmsplot <- ggmsplot +
    scale_fill_manual(drop = FALSE,
                      values = cpal) +
    scale_y_continuous(
      limits = c(0,1),
      expand = expansion(add = 0)
    ) +
    scale_x_discrete(
      expand = expansion(add = 0),
      breaks = kbreaks,
      labels = klabels,
      guide = guide_axis(check.overlap = TRUE)
    ) +
    labs(x = "", y = ylabspec) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(
      axis.title.y = element_text(vjust = +3),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.margin = margin(-0.2, 0, 0, -0.2, unit = "cm"),
      axis.line.x = element_line(linewidth = .3),
      axis.ticks = element_line(linewidth = .3)
    )


  grsize <- length(unique(msplotdata$group))

  if (grsize > 1) {
    ggmsplot <- ggmsplot +
      facet_wrap(~ .data$grouplab,
                 scales = "free_y",
                 ncol = facet_ncol,
                 nrow = facet_nrow
      ) +
      labs(x = "", y = "Rel. Freq.") +
      theme(
        panel.spacing = unit(2, "lines"),
        strip.text.x = element_text(margin = margin( b = 10, t = 0))
      )
  }

  ggmsplot <- ggmsplot +
    theme(plot.margin = margin(15, 15, 10, 15))

  return(ggmsplot)

}
