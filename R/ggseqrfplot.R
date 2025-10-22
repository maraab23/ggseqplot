#' Relative Frequency Sequence Plot
#'
#' Function for rendering sequence index plots with \code{\link[ggplot2]{ggplot2}}
#' instead of base R's \code{\link[base]{plot}} function that is used by
#' \code{\link[TraMineR:seqrfplot]{TraMineR::seqrfplot}}. Note that \code{ggseqrfplot}
#' uses \code{\link[patchwork]{patchwork}} to combine the different components of
#' the plot. The function and the documentation draw heavily from
#' \code{\link[TraMineR:seqrf]{TraMineR::seqrf}}.
#'
#' @param seqrfobject object of class \code{seqrf} generated with
#' \code{\link[TraMineR:seqrf]{TraMineR::seqrf}}. Default is \code{NULL};
#' either \code{seqrfobject} or \code{seqdata} and \code{diss} have to specified
#' @param seqdata State sequence object (class \code{stslist}) created with the
#' \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function. \code{seqdata} is
#' ignored if \code{seqrfobject} is specified.
#' @param weighted Controls if weights (specified in
#' \code{\link[TraMineR:seqdef]{TraMineR::seqdef}}) should be used. Default is
#' \code{TRUE}, i.e. if available weights are used.
#' @param k integer specifying the number of frequency groups. When \code{NULL},
#' \code{k} is set as the minimum between 100 and the sum of weights over 10.
#' \code{k} is ignored if \code{seqrfobject} is specified.
#' @param diss pairwise dissimilarities between sequences in \code{seqdata}
#' (see \code{\link[TraMineR:seqdist]{TraMineR::seqdist}}). \code{diss} is
#' ignored if \code{seqrfobject} is specified.
#' @param sortv optional sorting vector of length \code{nrow(diss)} that may be
#' used to compute the frequency groups. If \code{NULL}, the original data order
#' is used. If \code{mds} (default), the first MDS factor of \code{diss}
#' (\code{diss^2} when \code{squared=TRUE}) is used. Ties are randomly ordered.
#' Also allows for the usage of the string inputs:
#' \code{"from.start"} or \code{"from.end"} (see \code{\link{ggseqiplot}}).
#' \code{sortv} is ignored if \code{seqrfobject} is specified.
#' @param grp.meth Character string. One of \code{"prop"}, \code{"first"},
#' and \code{"random"}. Grouping method. See details. \code{grp.meth} is ignored
#' if \code{seqrfobject} is specified.
#' @param squared Logical. Should medoids (and computation of \code{sortv} when
#' applicable) be based on squared dissimilarities? (default is \code{FALSE}).
#' \code{squared} is ignored if \code{seqrfobject} is specified.
#' @param pow Dissimilarity power exponent (typically 1 or 2) for computation of
#' pseudo R2 and F. When \code{NULL}, pow is set as 1 when \code{squared = FALSE},
#' and as 2 otherwise. \code{pow} is ignored if \code{seqrfobject} is specified.
#' @param border if \code{TRUE} bars of index plot are plotted with black outline;
#' default is \code{FALSE} (also accepts \code{NULL})
#' @param ylab character string specifying title of y-axis. If \code{NULL} axis
#' title is "Frequency group"
#' @param yaxis Controls if a y-axis is plotted. When set as \code{TRUE}, index
#' of frequency groups is displayed.
#' @param which.plot character string specifying which components of relative
#' frequency sequence plot should be displayed. Default is \code{"both"}. If set
#' to \code{"medoids"} only the index plot of medoids is shown.
#' If \code{"diss.to.med"} only the box plots of the group-specific distances to
#' the medoids are shown.
#' @param quality specifies if representation quality is shown as figure caption;
#' default is \code{TRUE}
#' @param box.color specifies color of boxplot borders; default is "black
#' @param box.fill  specifies fill color of boxplots; default is "white"
#' @param box.alpha specifies alpha value of boxplot fill color; default is 1
#' @param outlier.color,outlier.fill,outlier.shape,outlier.size,outlier.stroke,outlier.alpha
#'   parameters to change the appearance of the outliers. Uses defaults of
#'   \code{\link[ggplot2:geom_boxplot]{ggplot2::geom_boxplot}}
#' @param outlier.jitter.height if greater than 0 outliers are jittered vertically.
#' If greater than .375 height is automatically adjusted to be aligned with the box width.
#'
#' @details This function renders relative frequency sequence plots using either an internal
#' call of \code{\link[TraMineR:seqrf]{TraMineR::seqrf}} or by using an object of
#' class \code{"seqrf"} generated with \code{\link[TraMineR:seqrf]{TraMineR::seqrf}}.
#'
#' For further details on the technicalities we refer to the excellent documentation
#' of \code{\link[TraMineR:seqrf]{TraMineR::seqrf}}. A detailed account of
#' relative frequency index plot can be found in the original contribution by
#' \insertCite{fasang2014;textual}{ggseqplot}.
#'
#' \code{ggseqrfplot} renders the medoid sequences extracted by
#' \code{\link[TraMineR:seqrf]{TraMineR::seqrf}} with an internal call of
#' \code{ggseqiplot}. For the box plot depicting the distances to the medoids
#' \code{ggseqrfplot} uses \code{\link[ggplot2]{geom_boxplot}} and
#' \code{\link[ggplot2]{geom_jitter}}. The latter is used for plotting the outliers.
#'
#' Note that \code{ggseqrfplot} renders in the box plots analogous to the those
#' produced by \code{\link[TraMineR:seqrfplot]{TraMineR::seqrfplot}}. Actually,
#' the box plots produced with \code{\link[TraMineR:seqrfplot]{TraMineR::seqrfplot}}
#' and  \code{\link[ggplot2:geom_boxplot]{ggplot2::geom_boxplot}}
#' might slightly differ due to differences in the underlying computations of
#' \code{\link[grDevices:boxplot.stats]{grDevices::boxplot.stats}} and
#' \code{\link[ggplot2:stat_boxplot]{ggplot2::stat_boxplot}}.
#'
#' Note that \code{ggseqrfplot} uses \code{\link[patchwork]{patchwork}} to combine
#' the different components of the plot. If you want to adjust the appearance of
#' the composed plot, for instance by changing the plot theme, you should consult
#' the documentation material of \code{\link[patchwork]{patchwork}}.
#'
#' At this point \code{ggseqrfplot} does not support a grouping option. For
#' plotting multiple groups, I recommend to produce group specific seqrfobjects or
#' plots and to arrange them in a common plot using \code{\link[patchwork]{patchwork}}.
#' See Example 6 in the vignette for further details:
#' \code{vignette("ggseqplot", package = "ggseqplot")}
#'
#' @return A relative frequency sequence plot using \code{\link[ggplot2]{ggplot}}.
#' @export
#' @importFrom patchwork plot_layout
#' @importFrom rlang .data
#'
#' @author Marcel Raab
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' library(TraMineR)
#' library(ggplot2)
#' library(patchwork)
#'
#' # From TraMineR::seqprf
#' # Defining a sequence object with the data in columns 10 to 25
#' # (family status from age 15 to 30) in the biofam data set
#' data(biofam)
#' biofam.lab <- c("Parent", "Left", "Married", "Left+Marr",
#'   "Child", "Left+Child", "Left+Marr+Child", "Divorced")
#'
#' # Here, we use only 100 cases selected such that all elements
#' # of the alphabet be present.
#' # (More cases and a larger k would be necessary to get a meaningful example.)
#' biofam.seq <- seqdef(biofam[501:600, 10:25], labels=biofam.lab,
#'                      weights=biofam[501:600,"wp00tbgs"])
#' diss <- seqdist(biofam.seq, method = "LCS")
#'
#'
#' # Using 12 groups and default MDS sorting
#' # and original method by Fasang and Liao (2014)
#'
#' # ... with TraMineR::seqrfplot (weights have to be turned off)
#' seqrfplot(biofam.seq, weighted = FALSE, diss = diss, k = 12,
#'           grp.meth="first", which.plot = "both")
#'
#' # ... with ggseqrfplot
#' ggseqrfplot(biofam.seq, weighted = FALSE, diss = diss, k = 12, grp.meth="first")
#'
#' # Arrange sequences by a user specified sorting variable:
#' # time spent in parental home; has ties
#' parentTime <- seqistatd(biofam.seq)[, 1]
#' b.srf <- seqrf(biofam.seq, diss=diss, k=12, sortv=parentTime)

#' # ... with ggseqrfplot (and some extra annotation using patchwork)
#' ggseqrfplot(seqrfobject = b.srf) +
#'   plot_annotation(title = "Sorted by time spent in parental home",
#'                   theme = theme(plot.title = element_text(hjust = 0.5, size = 18)))
ggseqrfplot <- function(seqdata = NULL,
                        diss = NULL,
                        k = NULL,
                        sortv = "mds",
                        weighted = TRUE,
                        grp.meth = "prop",
                        squared = FALSE,
                        pow = NULL,
                        seqrfobject = NULL,
                        border = FALSE,
                        ylab = NULL,
                        yaxis = TRUE,
                        which.plot = "both",
                        quality = TRUE,
                        box.color = NULL,
                        box.fill = NULL,
                        box.alpha = NULL,
                        outlier.jitter.height = 0,
                        outlier.color = NULL,
                        outlier.fill = NULL,
                        outlier.shape = 19,
                        outlier.size = 1.5,
                        outlier.stroke = 0.5,
                        outlier.alpha = NULL) {

  # for the displayed message
  ignored_params <- c("k", "sortv", "weighted", "grp.meth", "squared", "pow")
  if (inherits(seqrfobject, "seqrf") && inherits(seqdata, "stslist")) {
    cli::cli_inform(c(
      "i" = "You specified a {.code seqrfobject} & {.code seqdata};",
      " " = "the latter as well as the potentially specified parameters",
      " " = "{.code {ignored_params}} will be ignored;",
      "i" = "The plot will be rendered for the {.field seqrfobject}."
    ))
  }

  if (!is.null(seqdata) && !inherits(seqdata, "stslist") && !inherits(seqdata, "seqrf") && inherits(seqrfobject, "seqrf")) {
    cli::cli_inform(c(
      "i" = "You specified {.code seqdata} which are not stored as sequence object",
      " " = "and a valid {.code seqrfobject}; the {.code seqdata};",
      " " = "as well as the potentially specified parameters",
      " " = "{.code {ignored_params}} will be ignored;",
      "i" = "The plot will be rendered for the {.field seqrfobject}."
    ))
  }

  if (inherits(seqdata, "stslist") && !is.null(seqrfobject) && !inherits(seqrfobject, "seqrf")) {
    cli::cli_inform(c(
      "i" = "You specified a {.code seqrfobject} & {.code seqdata};",
      " " = "the {.code seqrfobject} is not of class {.cls seqref} and will be ignored;",
      "i" = "The plot will be rendered for the {.field seqrfobject} if the other parameters are specified correctly."
    ))
  }

  if (!is.null(seqdata) && !inherits(seqdata, "stslist") && !inherits(seqdata, "seqrf") && !inherits(seqrfobject, "seqrf")) {
    stop(
    "you specified seqdata which are not stored as sequence object and no valid seqrfobject;
use 'TraMineR::seqdef' to create a sequence object of class 'stslist' or specify a valid seqrfobject)"
    )
  }


  if (!is.null(seqdata) && !inherits(seqdata, "stslist") && inherits(seqdata, "seqrf")) {
    stop(
    "you specified seqdata which are of class 'seqrf';
probably you forgot to type 'seqrfobject = '"
    )
  }

  if (is.null(seqrfobject) && (is.null(seqdata) || !inherits(seqdata, "stslist"))) {
    stop(
      "no seqrfobject specified & seqdata are either not specified or not
stored as sequence object; use 'TraMineR::seqdef' to create one"
      )
  }

  if (!inherits(seqrfobject, "seqrf") && is.null(diss)) {
    stop(
      "no seqrfobject specified & diss = NULL; provide a dissimilarity matrix
provide a dissimilarity matrix ('diss')"
      )
  }

  border <- border %||% FALSE

  if (!is.logical(yaxis) || !is.logical(quality)) {
    stop("the arguments `yaxis`, and `quality`  have to be objects of type logical")
  }

  if (which.plot %in% c("both", "medoids", "diss.to.med") == FALSE) {
    stop('`which.plot` must take one of the following values: "both", "medoids", "diss.to.med"')
  }



  if (!inherits(seqrfobject, "seqrf")) {
    seqrfobject <- TraMineR::seqrf(seqdata = seqdata,
                                   diss = diss,
                                   k = k,
                                   sortv = sortv,
                                   weights = NULL,
                                   weighted = weighted,
                                   grp.meth = grp.meth,
                                   squared = squared,
                                   pow = pow)
  }


  seqdata <- seqrfobject$seqtoplot

  k <- nrow(seqdata)

  ylab <- ylab %||% "Frequency group"

  ylabels <- pretty(1:k)
  ylabels[1] <- 1
  ylabels[length(ylabels)] <- k

  if (ylabels[length(ylabels)] == ylabels[length(ylabels) - 1] + 1) {
    ylabels <- ylabels[ylabels != ylabels[length(ylabels) - 1]]
  }

  if (ylabels[1] == ylabels[2] - 1) {
    ylabels <- ylabels[ylabels != ylabels[2]]
  }

  ybrks <- ylabels
  #ybrks <- seq(step / 2, n - step / 2, by = step)[ylabels]

  box.color <- box.color %||% "black"
  box.fill <- box.fill %||% "white"
  box.alpha <- box.alpha %||% 1

  outlier.color <- outlier.color %||% "black"
  outlier.fill <- outlier.fill %||% "transparent"
  outlier.alpha <- outlier.alpha %||% 1
  if (outlier.jitter.height > .375) outlier.jitter.height <- .375


  suppressMessages(
    p1 <- ggseqiplot(seqdata, border = border, weighted = FALSE) +
      labs(
        title = "Group medoids",
        y = ylab
      ) +
      theme(plot.title = element_text(hjust = 0.5))
  )

  ylabsbrks <- ggplot_build(p1)$layout$panel_params[[1]]$y$get_labels()


  if (which.plot == "medoids") {
    suppressMessages(
      p1 <- ggseqiplot(seqdata, border = border) +
        labs(y = ylab)
    )
  }

  dlist <- seqrfobject$rf$dist.list
  dweights <- seqrfobject$rf$weights.list
  g <- length(dlist)

  wtd.fivenum.tmr <- utils::getFromNamespace("wtd.fivenum.tmr", "TraMineR")

  if (inherits(seqrfobject,"seqrfprop")){
    dist.stat <- matrix(rep(NA,5*g), nrow=5)
    for (i in 1:g){
      dist.stat[,i] <- wtd.fivenum.tmr(dlist[[i]],dweights[[i]])
    }
  } else {
    dist.stat  <- sapply(dlist, stats::fivenum)
  }

  # use the names which are used in TraMineR function
  rownames(dist.stat) <- c("minimum", "lower-hinge", "median",
                           "upper-hinge", "maximum")

  boxdata <- dist.stat |>
    t() |>
    dplyr::as_tibble() |>
    dplyr::mutate(k = dplyr::row_number(), .before = 1) |>
    dplyr::mutate(aux = 1.5*(.data$`upper-hinge` - .data$`lower-hinge`),
                  minimum = .data$`lower-hinge` - .data$aux,
                  minimum = ifelse(.data$minimum < 0, 0, .data$minimum),
                  aux2 = .data$maximum,
                  maximum = .data$`upper-hinge` + .data$aux) |>
    dplyr::select(1:6)

  colnames(boxdata) <- c("k", "ymin", "lower", "middle", "upper", "ymax")


  dotdata <- purrr::imap(seqrfobject$rf$dist.list,
                         ~dplyr::tibble(k = .y, values = .x)) |>
    dplyr::bind_rows() |>
    dplyr::left_join(dplyr::select(boxdata, "k", "ymin", "ymax"), by = "k") |>
    dplyr::filter(.data$values < .data$ymin | .data$values > .data$ymax)


  boxdata <- purrr::imap(seqrfobject$rf$dist.list,
                         ~dplyr::tibble(k = .y, values = .x)) |>
    dplyr::bind_rows() |>
    dplyr::left_join(boxdata, by = "k") |>
    dplyr::mutate(aux_max = .data$values > .data$upper & .data$values <= .data$ymax,
                  aux_min = .data$values < .data$lower & .data$values >= .data$ymin,
                  aux_max = ifelse(.data$aux_max == TRUE, .data$values, .data$upper),
                  aux_min = ifelse(.data$aux_min == TRUE, .data$values, .data$lower)) |>
    dplyr::group_by(.data$k) |>
    dplyr::summarise(ymin = min(.data$aux_min),
                     ymax = max(.data$aux_max)) |>
    dplyr::left_join(dplyr::select(boxdata, -c("ymin","ymax")), by = "k") |>
    dplyr::relocate("ymax", .after = "upper")


  p2 <- ggplot() +
    geom_jitter(data = dotdata,
                aes(y = .data$k, x = .data$values),
                height = outlier.jitter.height,
                width = 0,
                color = outlier.color,
                fill = outlier.fill,
                shape = outlier.shape,
                size = outlier.size,
                stroke = outlier.stroke,
                alpha = outlier.alpha) +
    geom_boxplot(data = boxdata,
                 aes(group = .data$k,
                     y = .data$k,
                     xmin = .data$ymin,
                     xlower = .data$lower,
                     xmiddle = .data$middle,
                     xupper = .data$upper,
                     xmax = .data$ymax),
                 stat= "identity",
                 orientation = 'y',
                 width = .75,
                 color = box.color, fill = box.fill, alpha = box.alpha) +
    scale_y_continuous(limits = range(0,k)+.5,
                       breaks = ylabsbrks,
                       expand = expansion(add = c(0, 0))) +
    labs(
      title = "Dissimilarities to medoid",
      x = "", y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_line(linewidth = .3),
      axis.ticks.x = element_line(linewidth = .3),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )


  if (which.plot == "diss.to.med") {
    p2 <- p2 + scale_y_continuous(
      limits = range(0,k)+.5,
      breaks = ylabsbrks,
      expand = expansion(add = 0)
    ) +
      labs(y = ylab, x = "", title = NULL) +
      theme_minimal() +
      theme(
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(linewidth = .3),
        axis.ticks.x = element_line(linewidth = .3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  }


  ggrfplot <- p1 + p2 +
    plot_layout(widths = c((1+sqrt(5))/2, 1),
                guides = "collect") &
    theme(legend.position = "bottom")

  if (yaxis == FALSE) {
    ggrfplot[[1]] <- ggrfplot[[1]] +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()
      )
  }


  if (which.plot == "medoids") ggrfplot <- p1
  if (which.plot == "diss.to.med") ggrfplot <- p2

  if (quality == TRUE) {
    ggrfplot <- ggrfplot + patchwork::plot_annotation(
      caption = paste(
        "Representation quality: R2 =", format(round(seqrfobject$rf$R2, 2), nsmall = 2),
        "and F =", format(round(seqrfobject$rf$Fstat, 2), nsmall = 2)
      )
    )
  }

  ggrfplot <- ggrfplot +
    theme(plot.margin = margin(15, 15, 10, 15))

  ggrfplot$rfsummary <- summary(seqrfobject)[c(1,3,4,5)]

  return(ggrfplot)
}
