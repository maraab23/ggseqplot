#' Sequence Transition Rate Plot
#'
#' Function for plotting transition rate matrix of sequence states internally
#' computed by \code{\link[TraMineR:seqtrate]{TraMineR::seqtrate}} \insertCite{gabadinho2011}{ggseqplot}.
#' Plot is generated using \code{\link[ggplot2]{ggplot2}} \insertCite{wickham2016}{ggseqplot}.
#'
#' @eval shared_params()
#' @param dss specifies if transition rates are computed for STS or DSS (default) sequences
#' @param no.n specifies if number of (weighted) sequences is shown in grouped (faceted) graph
#' @param with.missing Specifies if missing state should be considered when computing the transition rates (default is \code{FALSE}).
#' @param labsize Specifies the font size of the labels within the tiles (if not specified ggplot2's default is used)
#' @param axislabs specifies if sequence object's long "labels" (default) or the state names from its "alphabet" attribute should be used.
#' @param x_n.dodge allows to print the labels of the x-axis in multiple rows to avoid overlapping.
#' @eval shared_facet()
#'
#' @details The transition rates are obtained by an internal call of
#' \code{\link[TraMineR:seqtrate]{TraMineR::seqtrate}}.
#' This requires that the input data (\code{seqdata})
#' are stored as state sequence object (class \code{stslist}) created with
#' the \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function.
#' As STS based transition rates tend to be dominated by high values on the diagonal, it might be
#' worthwhile to examine DSS sequences instead (\code{dss = TRUE})). In this case the resulting
#' plot shows the transition rates between episodes of distinct states.
#'
#' In any case (DSS or STS) the transitions rates are reshaped into a a long data format
#' to enable plotting with \code{\link[ggplot2]{ggplot2}}. The resulting output then is
#' prepared to be plotted with  \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
#' The data and specifications used for rendering the plot can be obtained by storing the
#' plot as an object. The appearance of the plot can be adjusted just like with
#' every other ggplot (e.g., by changing the theme or the scale using \code{+} and
#' the respective functions).
#'
#' @return A tile plot of transition rates.
#' @export
#'
#' @author Marcel Raab
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' # Use example data from TraMineR: biofam data set
#' data(biofam)
#'
#' # We use only a sample of 300 cases
#' set.seed(10)
#' biofam <- biofam[sample(nrow(biofam),300),]
#' biofam.lab <- c("Parent", "Left", "Married", "Left+Marr",
#'                 "Child", "Left+Child", "Left+Marr+Child", "Divorced")
#' biofam.seq <- seqdef(biofam, 10:25, labels=biofam.lab, weights = biofam$wp00tbgs)
#'
#' # Basic transition rate plot (with adjusted x-axis labels)
#' ggseqtrplot(biofam.seq, x_n.dodge = 2)
#'
#' # Transition rate with group variable (with and without weights)
#' ggseqtrplot(biofam.seq, group=biofam$sex, x_n.dodge = 2)
#' ggseqtrplot(biofam.seq, group=biofam$sex, x_n.dodge = 2, weighted = FALSE)
#'
#' @import ggplot2
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
ggseqtrplot <- function(seqdata,
                        dss = TRUE,
                        group = NULL,
                        no.n = FALSE,
                        weighted = TRUE,
                        with.missing = FALSE,
                        labsize = NULL,
                        axislabs = "labels",
                        x_n.dodge = 1,
                        facet_ncol = NULL,
                        facet_nrow = NULL){


  if (!inherits(seqdata, "stslist"))
    stop("data are not stored as sequence object, use 'TraMineR::seqdef' to create one")


  if (!is.null(group) & (length(group) != nrow(seqdata)))
    stop("length of group vector must match number of rows of seqdata")


  if(!is.logical(weighted) | !is.logical(with.missing))
    stop("the arguments `weighted` and `with.missing` have to be objects of type logical")

  if (is.null(attributes(seqdata)$weights)) weighted <- FALSE

  if (is.null(labsize)) labsize <- GeomLabel$default_aes$size

  if (!is.null(labsize) & (length(labsize) > 1 | !is.numeric(labsize))) {
    stop("labsize must be a single number")
  }

  if (is.factor(group)) {
    group <- forcats::fct_drop(group)
    grinorder <- levels(group)
  } else {
    grinorder <- factor(unique(group))
  }
  if (is.null(group)) grinorder <- factor(1)

  if (is.null(group)) group <- 1

  if (weighted == FALSE) {
    weights <- 1
  } else {
    weights <- attributes(seqdata)$weights
  }

  grn <- dplyr::tibble(idx = 1:nrow(seqdata),
                       grouplab = group,
                       weights = weights) |>
    dplyr::group_by(.data$grouplab) |>
    dplyr::summarise(n = dplyr::n(),
                     n_wgt = sum(weights)) |>
    dplyr::mutate(group = dplyr::row_number(), .before = 1)

  if (weighted == TRUE) {
    grn$grlab <- glue::glue("{grn$grouplab} \n({grn$n} sequences; weighted n={round(grn$n_wgt,2)})")
  } else {
    grn$grlab <- glue::glue("{grn$grouplab} (n={grn$n})")
  }

  if (axislabs == "labels") {
    aux <- attributes(seqdata)$labels
  } else if (axislabs == "alphabet") {
    aux <- attributes(seqdata)$alphabet
  } else {
    stop('the arguments `axislabs` has to be either "labels" or "alphabet".')
  }

  if (with.missing == TRUE) {
    axislabs <- append(aux, ifelse(axislabs == "labels","Missing", "NA"))
  } else {
    axislabs <- aux
  }


  if (dss == TRUE) {
    transmat <- purrr::map(grinorder,
                           ~TraMineR::seqtrate(TraMineR::seqdss(seqdata[group == .x,]),
                                               weighted = weighted,
                                               with.missing = with.missing))
  } else {
    transmat <- purrr::map(grinorder,
                           ~TraMineR::seqtrate(seqdata[group == .x,],
                                               weighted = weighted,
                                               with.missing = with.missing))
  }


  trplotdata <- purrr::map(transmat, function(x) {
    dplyr::as_tibble(x,
                     .name_repair = ~as.character(1:nrow(x)))  |>
      dplyr::mutate(origin = dplyr::row_number()) |>
      tidyr::pivot_longer(1:nrow(x),
                          names_to = "destination",
                          names_transform = list(destination = as.integer))
  }
  )

  trplotdata <- purrr::imap(trplotdata,
                            ~.x |>
                              dplyr::mutate(group = .y, .before = 1)) |>
    dplyr::bind_rows()


  suppressMessages(
    trplotdata <- dplyr::full_join(grn, trplotdata, by = "group")
  )

  ggtrplot <- trplotdata |>
    dplyr::mutate(value = dplyr::na_if(.data$value, 0)) |>
    ggplot(aes(x = .data$destination,
               y = .data$origin,
               fill = .data$value)) +
    geom_tile(color = "black", alpha = .9) +
    geom_text(aes(label= ifelse(is.na(.data$value), "",
                                sprintf(.data$value, fmt = '%#.2f'))),
              size = labsize) +
    colorspace::scale_fill_continuous_sequential(palette = "heat 2",
                                                 na.value = "transparent") +
    scale_x_continuous(name=expression('State at'~italic("t + 1")),
                       breaks = 1:nrow(transmat[[1]]),
                       labels = axislabs,
                       guide = guide_axis(n.dodge = x_n.dodge),
                       expand = expansion(add = c(.1, .1))) +
    scale_y_continuous(name=expression('State at'~italic("t")),
                       breaks = 1:nrow(transmat[[1]]),
                       labels = axislabs,
                       expand = expansion(add = c(.1,.1))) +
    coord_equal() +
    theme_minimal()  +
    theme(legend.position = "none",
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          axis.title.x = element_text(margin = margin(10, 0, 0, 0)))


  grsize <- length(unique(trplotdata$group))

  if (no.n == TRUE) ggtrplot$data$grlab <- ggtrplot$data$grouplab

  if (grsize > 1) {
    ggtrplot <- ggtrplot +
      facet_wrap(~forcats::fct_inorder(.data$grlab),
                 ncol = facet_ncol,
                 nrow = facet_nrow) +
      theme(panel.spacing = unit(2, "lines"),
            strip.text.x = element_text(margin = margin( b = 10, t = 0)))
  }

  ggtrplot <- ggtrplot +
    theme(plot.margin = margin(15, 15, 10, 15))

  return(ggtrplot)

}
