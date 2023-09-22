#' Mean time plot
#'
#' Function for rendering plot displaying the mean time spent in each state of
#' a state sequence object using \code{\link[ggplot2]{ggplot2}}
#' \insertCite{wickham2016}{ggseqplot} instead of base R's
#' \code{\link[base]{plot}} function that is used by
#' \code{\link[TraMineR:seqplot]{TraMineR::seqplot}}
#' \insertCite{gabadinho2011}{ggseqplot}.
#'
#' @eval shared_params()
#' @param no.n specifies if number of (weighted) sequences is shown
#' (default is \code{TRUE})
#' @param with.missing Specifies if missing states should be considered when
#' computing the state distributions (default is \code{FALSE}).
#' @param border if \code{TRUE} bars are plotted with black outline; default is \code{FALSE} (also accepts \code{NULL})
#' @param error.bar allows to add error bars either using the standard
#' deviation \code{"SD"} or the standard error \code{"SE"}; default plot is
#' without error bars
#' @param error.caption a caption is added if error bars are displayed; this
#' default behavior can be turned off by setting the argument to  \code{"FALSE"}
#' @param facet_scale Specifies if y-scale in faceted plot should be
#' \code{"fixed"} (default) or \code{"free_y"}
#' @eval shared_facet()
#'
#' @details The information on time spent in different states is obtained by an
#' internal call of \code{\link[TraMineR:seqmeant]{TraMineR::seqmeant}}. This
#' requires that the input data (\code{seqdata}) are stored as state sequence
#' object (class \code{stslist}) created with the
#' \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function. The resulting
#' output then is prepared to be plotted with
#'  \code{\link[ggplot2:geom_bar]{ggplot2::geom_bar}}. The data and
#' specifications used for rendering the plot can be obtained by storing the
#' plot as an object. The appearance of the plot can be adjusted just like with
#' every other ggplot (e.g., by changing the theme or the scale using \code{+}
#' and the respective functions).
#'
#' @return A mean time plot created by using \code{\link[ggplot2]{ggplot2}}.
#' If stored as object the resulting list object (of class gg and ggplot) also
#' contains the data used for rendering the plot
#' @export
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
#' seqmtplot(actcal.seq, group = actcal$sex)
#' # with ggseqplot
#' ggseqmtplot(actcal.seq, group = actcal$sex)
#' # with ggseqplot using additional arguments and some adjustments
#' ggseqmtplot(actcal.seq, no.n = TRUE, error.bar = "SE") +
#'  coord_flip() +
#'  theme(axis.text.y=element_blank(),
#'        axis.ticks.y = element_blank(),
#'        panel.grid.major.y = element_blank(),
#'        legend.position = "top")
#'
#' @importFrom rlang .data
ggseqmtplot <- function(seqdata,
                        no.n = FALSE,
                        group = NULL,
                        weighted = TRUE,
                        with.missing = FALSE,
                        border = FALSE,
                        error.bar = NULL,
                        error.caption = TRUE,
                        facet_scale = "fixed",
                        facet_ncol = NULL,
                        facet_nrow = NULL) {
  if (!inherits(seqdata, "stslist")) {
    stop("data are not stored as sequence object, use 'TraMineR::seqdef' to create one")
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

  if (is.null(group)) group <- 1

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

  xandgrouplabs <- xandgrouplab(seqdata = seqdata,
                                weighted = weighted,
                                no.n = no.n,
                                grinorder = grinorder,
                                group = group,
                                ylabprefix = "Mean time")
  grouplabspec <- xandgrouplabs[[1]]
  ylabspec <- xandgrouplabs[[2]]


  mtplotdata <- purrr::map(grinorder,
                           ~seqmeant(seqdata[group == .x,],
                                     serr=TRUE,
                                     weighted = weighted,
                                     with.missing = with.missing) |>
                             as.data.frame() |>
                             dplyr::mutate(group = .x,
                                           state =
                                             forcats::fct_inorder(
                                               ifelse(dplyr::row_number() <= length(alphabet(seqdata)),
                                                      alphabet(seqdata), "Missing")),
                                           labels = forcats::fct_inorder(
                                             ifelse(dplyr::row_number() <= length(alphabet(seqdata)),
                                                    attributes(seqdata)$labels, "Missing")))) |>
    dplyr::bind_rows() |>
    dplyr::filter(!(.data$state == "Missing" & .data$Mean == 0)) |>
    dplyr::mutate(labels = forcats::fct_drop(.data$labels),
                  state = forcats::fct_drop(.data$state)) |>
    dplyr::full_join(grouplabspec, by = "group")


  if ("Missing" %in% mtplotdata$state) {
    cpal <- c(
      attributes(seqdata)$cpal,
      attributes(seqdata)$missing.color
    )
  } else {
    cpal <- attributes(seqdata)$cpal
  }

  ggmtplot <- mtplotdata |>
    ggplot(aes(x = .data$state, fill = .data$labels)) +
    geom_bar(aes(y = .data$Mean), stat="identity",
             color = ifelse(border == TRUE, "black",
                            "transparent")) +
    scale_y_continuous(expand = expansion(add = 0)) +
    scale_fill_manual(drop = FALSE,
                      values = cpal) +
    labs(x = "", y = ylabspec) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.margin = margin(15, 15, 10, 15),
          legend.margin = margin(-0.2, 0, 0, -0.2, unit = "cm"))

  if (!is.null(error.bar)) {

    if (error.bar == "SE") {
      ggmtplot <- ggmtplot +
        geom_errorbar(aes(ymin = .data$Mean - .data$SE,
                          ymax= .data$Mean + .data$SE),
                      width=0.1, alpha=0.6, size=1)
    } else if (error.bar == "SD") {
      ggmtplot <- ggmtplot +
        geom_errorbar(aes(ymin = .data$Mean - .data$Stdev,
                          ymax = .data$Mean + .data$Stdev),
                      width=0.1, alpha=0.6, size=1)
    }

    captext <- glue::glue(
      'Note: error bars show standard {ifelse(error.bar == "SE",
    "errors", "deviations")}'
    )

    if (error.caption == TRUE & !is.null(error.bar)) {
      ggmtplot <- ggmtplot +
        labs(caption = captext)
    }
  }

  if (length(unique(group)) > 1) {
    ggmtplot <- ggmtplot +
      facet_wrap(~ .data$grouplab,
                 scales = facet_scale,
                 ncol = facet_ncol) +
      labs(y = "Mean time")
    theme(panel.spacing = unit(2, "lines"))
  }

  ggmtplot <- ggmtplot +
    theme(axis.title.y = element_text(vjust = +3),
          plot.margin = margin(15, 15, 10, 15),
          axis.line.x = element_line(linewidth = .3),
          axis.ticks = element_line(linewidth = .3))


  return(ggmtplot)
}
