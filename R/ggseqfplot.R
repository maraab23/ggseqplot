#' Sequence Frequency Plot
#'
#' Function for rendering sequence index plot of the most frequent sequences of
#' a state sequence object using \code{\link[ggplot2]{ggplot2}} \insertCite{wickham2016}{ggseqplot}
#' instead of base R's \code{\link[base]{plot}} function that is used by
#' \code{\link[TraMineR:seqfplot]{TraMineR::seqplot}} /
#' \code{\link[TraMineR:plot.stslist.freq]{TraMineR::plot.stslist.freq}} \insertCite{gabadinho2011}{ggseqplot}.
#'
#' @eval shared_params()
#' @param ranks specifies which of the most frequent sequences should be plotted;
#' default is the first ten (\code{1:10}); if set to 0 all sequences are displayed
#' @param border if \code{TRUE} bars are plotted with black outline; default is \code{FALSE} (also accepts \code{NULL})
#' @param proportional if \code{TRUE} (default), the sequence heights are
#' displayed proportional to their frequencies
#' @param ylabs defines appearance of y-axis labels; default (\code{"total"})
#' only labels min and max (i.e. cumulative relative frequency); if \code{"share"} labels indicate
#' relative frequency of each displayed sequence (note: overlapping labels are removed)
#' @eval shared_facet()
#'
#' @details The subset of displayed sequences is obtained by an internal call of
#' \code{\link[TraMineR:seqtab]{TraMineR::seqtab}}. The extracted sequences are plotted
#' by a call of \code{\link[ggseqplot:ggseqiplot]{ggseqiplot}} which uses
#' \code{\link[ggplot2:geom_rect]{ggplot2::geom_rect}} to render the sequences. The data
#' and specifications used for rendering the plot can be obtained by storing the
#' plot as an object. The appearance of the plot can be adjusted just like with
#' every other ggplot (e.g., by changing the theme or the scale using \code{+} and
#' the respective functions).
#'
#' Experienced ggplot2 users might notice the customized labeling of the
#' y-axes in the faceted plots (i.e. plots with specified \code{group} argument). This has
#' been achieved by utilizing the very helpful \code{\link[ggh4x]{ggh4x}} library.
#'
#' @return A sequence frequency plot created by using \code{\link[ggplot2]{ggplot2}}.
#' If stored as object the resulting list object (of class gg and ggplot) also
#' contains the data used for rendering the plot.
#' @export
#'
#' @author Marcel Raab
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso
#' \code{\link[ggseqplot:ggseqiplot]{ggseqiplot}}
#'
#' @examples
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' # Examples from TraMineR::seqplot
#'
#' library(TraMineR)
#' library(ggplot2)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' # Examples from TraMineR::seqplot
#'
#' # actcal data set
#' data(actcal)
#'
#' # We use only a sample of 300 cases
#' set.seed(1)
#' actcal <- actcal[sample(nrow(actcal), 300), ]
#' actcal.lab <- c("> 37 hours", "19-36 hours", "1-18 hours", "no work")
#' actcal.seq <- seqdef(actcal, 13:24, labels = actcal.lab)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' # sequence frequency plot
#' # with TraMineR::seqplot
#' seqfplot(actcal.seq)
#' # with ggseqplot
#' ggseqfplot(actcal.seq)
#' # with ggseqplot applying additional arguments and some layout changes
#' ggseqfplot(actcal.seq,
#'            group = actcal$sex,
#'            ranks = 1:5,
#'            ylabs = "share") +
#'   scale_x_discrete(breaks = 1:12,
#'                    labels = month.abb,
#'                    expand = expansion(add = c(0.2, 0)))
ggseqfplot <- function(seqdata,
                       group = NULL,
                       ranks = 1:10,
                       weighted = TRUE,
                       border = FALSE,
                       proportional = TRUE,
                       ylabs = "total",
                       facet_ncol = NULL,
                       facet_nrow = NULL) {

  if (!inherits(seqdata, "stslist")) {
    stop("data are not stored as sequence object, use 'TraMineR::seqdef' to create one")
  }

  if (!is.null(group) & (length(group) != nrow(seqdata))) {
    stop("length of group vector must match number of rows of seqdata")
  }

  if (is.null(border)) border <- FALSE

  if (!is.logical(weighted) | !is.logical(proportional) |
      !is.logical(border)) {
    stop("the arguments `weighted`, `proportional`, and `border` have to be
         objects of type logical")
  }

  if (is.null(attributes(seqdata)$weights)) weighted <- FALSE

  if (is.null(group)) group <- 1

  if (!is.null(facet_ncol) && as.integer(facet_ncol) != facet_ncol) {
    stop("`facet_ncol` must be NULL or an integer.")
  }

  if (!is.null(facet_nrow) && as.integer(facet_nrow) != facet_nrow) {
    stop("`facet_nrow` must be NULL or an integer.")
  }

  fplotdata <- purrr::map(sort(unique(group)),
                          ~seqtab(seqdata[group == .x,],
                                  weighted = weighted,
                                  idxs = ranks))

  group <- rep(sort(unique(group)), each = max(ranks))

  coverage <- purrr::map(fplotdata,
                         ~attributes(.x)$freq$Percent) |>
    unlist()


  set_class <- `class<-`

  fplotdata <- purrr::map(fplotdata,
                          ~.x |>
                            set_class(c("stslist", "data.frame"))) |>
    dplyr::bind_rows()


  # if (proportional == TRUE) {
  attributes(fplotdata)$weights <- coverage
  # }

  # group specific scales

  ylb <- function(coverage = coverage,
                  fplotdata = fplotdata){

    totalcov <- round(sum(coverage),1)

    if (ylabs == "total") {

      if (proportional == TRUE) {
        ybreaks <- c(0,round(sum(coverage),1))
      } else {
        ybreaks <- c(0,length(coverage))+.5
      }

      labs <- paste0(c(0,round(sum(coverage),1)),"%")
    }

    if (ylabs == "share") {

      if (proportional == TRUE) {
        aux <- ggseqiplot(fplotdata)$data
        ybreaks <- (unique(aux$begin) + unique(aux$end)) / 2
      } else {
        ybreaks <- 1:length(coverage)
      }

      labs <- paste0(round(coverage,1),"%")
    }

    ylb <- list(ybreaks = ybreaks,
                ylabs = labs,
                totalcov = totalcov)

    return(ylb)
  }

  ylb <- purrr::map(sort(unique(group)),
                    ~ylb(coverage = coverage[group == .x],
                         fplotdata = fplotdata[group == .x,]))

  scales <- purrr::map(1:length(unique(group)),
                       ~scale_y_continuous(
                         expand = expansion(mult = .01),
                         breaks = ylb[[.x]]$ybreaks,
                         labels = ylb[[.x]]$ylabs,
                         guide = guide_axis(check.overlap = TRUE)))

  if (length(unique(group)) == 1) {
    suppressMessages(
      ggfplot <- ggseqiplot(fplotdata,
                            border = border,
                            weighted = proportional,
                            facet_ncol = NULL,
                            facet_nrow = NULL) +
        scales +
        labs(y = ifelse(ylabs == "total",
                        "Relative frequency (total)",
                        "Relative frequency (per sequence)"))
    )
  } else {
    suppressMessages(
      ggfplot <- ggseqiplot(fplotdata,
                            border = border,
                            group = group,
                            weighted = proportional,
                            facet_ncol = facet_ncol,
                            facet_nrow = facet_nrow) +
        ggh4x::facetted_pos_scales(y = scales) +
        labs(y = ifelse(ylabs == "total",
                        "Relative frequency (total)",
                        "Relative frequency (per sequence)"))
    )

    if (ylabs == "total") {
      ggfplot$data$grouplab <- sub("\\n.*", "",
                                   ggfplot$data$grouplab)
      ggfplot$data$grouplab <- sub("[ (].*", "",
                                   ggfplot$data$grouplab)
    } else {

      grouplab <- purrr::imap(sort(unique(group)),
                              ~glue::glue("{.x}
                        (total coverage = {ylb[[.y]]$totalcov}%)")) |>
        unlist()

      grouplab <- dplyr::tibble(group = sort(unique(group)),
                                grouplab = grouplab)

      ggfplot$data <- dplyr::full_join(dplyr::select(ggfplot$data, -.data$grouplab),
                                       grouplab, by = "group")


    }

  }

  return(ggfplot)


}
