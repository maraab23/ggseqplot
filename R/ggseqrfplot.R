#' Relative Frequency Sequence Plot
#'
#' Function for rendering sequence index plots with \code{\link[ggplot2]{ggplot2}} instead of base
#' R's \code{\link[base]{plot}} function that is used by \code{\link[TraMineRextras:seqplot.rf]{TraMineRextras::seqplot.rf}}.
#' Note that \code{ggseqrfplot} uses \code{\link[patchwork]{patchwork}} to combine the different components of the plot.
#' For further details on relative frequency sequence plots see documentation of \code{\link[TraMineRextras:seqplot.rf]{TraMineRextras::seqplot.rf}}.
#'
#' @param seqdata State sequence object (class \code{stslist}) created with the \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function.
#' @param k integer specifying the number of frequency groups
#' @param diss pairwise dissimilarities between sequences in \code{seqdata} (see \code{TraMineRextras::seqdist})
#' @param sortv optional sorting variable that may be used to compute the frequency groups. If  \code{NULL} (default), an MDS is used. Ties are randomly ordered.
#' @param ylab character string specifying title of y-axis. If \code{NULL} axis title is "Frequency group"
#' @param yaxis Controls if a y-axis is plotted. When set as \code{TRUE}, index of frequency groups is displayed.
#' @param which.plot character string specifying which components of relative frequency sequence plot should be displayed.
#' Default is \code{"both"}. If set to \code{"medoids"} only the index plot of medoids is shown. If \code{"diss.to.med"} only
#' the box plots of the group-specific distances to the medoids are shown.
#' @param quality specifies if representation quality is shown as figure caption; default is \code{TRUE}
#' @param box.color specifies color of boxplot borders; default is "black
#' @param box.fill  specifies fill color of boxplots; default is "white"
#' @param box.alpha specifies alpha value of boxplot fill color; default is 1
#'
#' @return
#' @export ggseqrfplot
#' @importFrom patchwork plot_layout
#'
#' @examples
#' ## From TraMineRextras::seqplot.rf
#' library(TraMineR)
#' library(TraMineRextras)
#' library(patchwork)
#' library(ggplot2)
#'
#' ## Defining a sequence object with the data in columns 10 to 25
#' ## (family status from age 15 to 30) in the biofam data set
#' data(biofam)
#' biofam.lab <- c("Parent", "Left", "Married", "Left+Marr",
#'                 "Child", "Left+Child", "Left+Marr+Child", "Divorced")
#' ## Here, we use only 100 cases selected such that all elements
#' ## of the alphabet be present.
#' ## (More cases and a larger k would be necessary to get a meaningful example.)
#' biofam.seq <- seqdef(biofam[501:600, ], 10:25, labels=biofam.lab)
#' diss <- seqdist(biofam.seq, method="LCS")
#'
#'
#' ## Using 12 groups and default MDS sorting
#' ## ... with TraMineRextras::seqplot.rf
#' seqplot.rf(biofam.seq, diss=diss, k=12)
#'
#' ## ... with ggseqrfplot
#' ggseqrfplot(biofam.seq, diss=diss, k=12)
#'
#'
#' ## With a user specified sorting variable
#' ## Here time spent in parental home
#' parentTime <- seqistatd(biofam.seq)[, 1]
#'
#' ## ... with TraMineRextras::seqplot.rf
#' seqplot.rf(biofam.seq, diss=diss, k=12, sortv=parentTime,
#'    main="Sorted by parent time")
#'
#' ## ... with ggseqrfplot
#' ggseqrfplot(biofam.seq, diss=diss, k=12, sortv=parentTime) +
#'   plot_annotation(title = "Sorted by parent time",
#'                   theme = theme(plot.title = element_text(hjust = 0.5,
#'                                                           size = 18)))
ggseqrfplot <- function(seqdata,
                        k = floor(nrow(seqdata)/10),
                        diss,
                        sortv = NULL,
                        ylab= NULL,
                        yaxis=TRUE,
                        box.color = NULL,
                        box.fill = NULL,
                        box.alpha = NULL,
                        which.plot="both",
                        quality=TRUE) {

  if (!is.logical(yaxis) | !is.logical(quality))
    stop("the arguments `yaxis`, and `quality`  have to be objects of type logical")

  if (which.plot %in% c("both", "medoids", "diss.to.med") == FALSE) {
    stop('`which.plot` must take one of the following values: "both", "medoids", "diss.to.med"')
  }


  seqdata <- rfplot.obj(seqdata, k=k, diss=diss, sortv=sortv)

  if (is.null(ylab)) ylab <- "Frequency group"

  n <- nrow(seqdata[[1]])
  step <- n/k

  ylabels <- pretty(1:k)
  ylabels[1] <- 1
  ylabels[length(ylabels)] <- k

  if (ylabels[length(ylabels)] == ylabels[length(ylabels)-1]+1) {
    ylabels <- ylabels[ylabels != ylabels[length(ylabels)-1]]
  }
  if (ylabels[1] == ylabels[2]-1) {
    ylabels <- ylabels[ylabels != ylabels[2]]
  }

  ybrks <- seq(step/2, n - step/2, by=step)[ylabels]

  if (is.null(box.color)) box.color <- "black"
  if (is.null(box.fill)) box.fill <- "white"
  if (is.null(box.alpha)) box.alpha <- 1


  # suppressMessages(
  #   p1 <- ggseqiplot(seqdata[[1]], sortv=seqdata[[2]], border = border) +
  #     scale_y_continuous(labels = NULL,
  #                        breaks = NULL,
  #                        expand = expansion(add = c(0, 0))) +
  #     labs(title = "Sequence medoids",
  #          y = "") +
  #     theme(plot.title = element_text(hjust = 0.5))
  # )

  suppressMessages(
    p1 <- ggseqiplot(seqdata[[1]], sortv=seqdata[[2]]) +
      labs(title = "Sequence medoids",
           y = ylab) +
      scale_y_continuous(breaks = ybrks,
                         labels = ylabels,
                         expand = expansion(add = c(0, 0))) +
      theme(plot.title = element_text(hjust = 0.5))
  )

  # suppressMessages(
  #   p1 <- ggseqiplot(seqdata[[1]], sortv=seqdata[[2]], border = border) +
  #     scale_y_continuous(breaks = ybrks,
  #                        labels = ylabels,
  #                        expand = expansion(add = c(0, 0))) +
  #     labs(y = "Frequency group")
  # )

  if (which.plot == "medoids") {
    suppressMessages(
      p1 <- ggseqiplot(seqdata[[1]], sortv=seqdata[[2]]) +
        scale_y_continuous(breaks = ybrks,
                           labels = ylabels,
                           expand = expansion(add = c(0, 0))) +
        labs(y = ylab)
    )
  }

  boxdata <- dplyr::tibble(kgr = seqdata[[2]], values = seqdata[[3]])

  p2 <- boxdata |>
    ggplot(aes(y = .data$kgr, x = .data$values, group = .data$kgr)) +
    geom_boxplot(color = box.color, fill = box.fill, alpha = box.alpha)  +
    scale_y_continuous(expand = expansion(add = c(0, 0))) +
    labs(title = "Dissimilarities to medoid",
         x = "", y = "") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank())

  if (which.plot == "diss.to.med") {
    p2 <- boxdata |>
      ggplot(aes(y = .data$kgr, x = .data$values, group = .data$kgr)) +
      geom_boxplot(color = box.color, fill = box.fill, alpha = box.alpha)  +
      scale_y_continuous(breaks = ylabels,
                         labels = ylabels,
                         expand = expansion(add = c(0, 0))) +
      labs(y = ylab, x = "") +
      theme_minimal()
  }


  seqrfplot <- p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

  if (yaxis==FALSE) {
    seqrfplot[[1]] <- seqrfplot[[1]] +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank())
  }


  if (which.plot == "medoids") seqrfplot <- p1
  if (which.plot == "diss.to.med") seqrfplot <- p2

  if (quality == TRUE) {
    seqrfplot <- seqrfplot + patchwork::plot_annotation(
      caption = paste("Representation quality: R2 =",
                      round(as.numeric(seqdata["R2"]),2),
                      "and F =", round(as.numeric(seqdata["Fstat"]),2))
    )
  }

  return(seqrfplot)

}
