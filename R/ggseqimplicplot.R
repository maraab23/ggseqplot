#' Implicative Statistic Plot
#'
#' Function for plotting the implicative statistic of states by group and
#' sequence position computed with
#' \code{\link[TraMineRextras:seqimplic]{TraMineRextras::seqimplic}}. The plot is
#' rendered with \code{\link[ggplot2]{ggplot2}} \insertCite{wickham2016}{ggseqplot}
#' instead of base R's \code{\link[base]{plot}} function that is used by
#' \code{TraMineRextras::plot.seqimplic}.
#'
#' @param seqdata State sequence object (class \code{stslist}) created with the
#' \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function. Ignored if
#' \code{seqimplicobject} is specified.
#' @param group A vector of the same length as the sequence data indicating group
#' membership. A distinct plot is generated for each level of group. Ignored if
#' \code{seqimplicobject} is specified.
#' @param seqimplicobject Object of class \code{seqimplic} generated with
#' \code{\link[TraMineRextras:seqimplic]{TraMineRextras::seqimplic}}. Either
#' \code{seqimplicobject} or \code{seqdata} and \code{group} have to be specified.
#' @param weighted Controls if weights (specified in \code{\link[TraMineR:seqdef]{TraMineR::seqdef}})
#' should be used. Default is \code{TRUE}, i.e. if available weights are used.
#' Ignored if \code{seqimplicobject} is specified.
#' @param with.missing Specifies if missing states should be considered when
#' computing the implicative statistic (default is \code{FALSE}).
#' Ignored if \code{seqimplicobject} is specified.
#' @param conf.level Confidence level(s) shown as horizontal reference lines;
#' default is \code{0.95}. Several levels can be specified (e.g. \code{c(0.95, 0.99)});
#' \code{NULL} suppresses the reference lines.
#' @param linewidth Specifies the width of the lines; default is \code{1}
#' @param facet_ncol Number of columns in faceted (i.e. grouped) plot
#' @param facet_nrow Number of rows in faceted (i.e. grouped) plot
#'
#' @return A line plot of the implicative statistic created by using
#' \code{\link[ggplot2]{ggplot2}}. If stored as object the resulting list object
#' (of class gg and ggplot) also contains the data (long format) used for
#' rendering the plot.
#'
#' @export
#'
#' @details The implicative statistic measures how strongly membership in a
#' group implies being in a certain state at a given sequence position. The
#' function uses \code{\link[TraMineRextras:seqimplic]{TraMineRextras::seqimplic}}
#' to compute the statistic. Hence, the package 'TraMineRextras' has to be
#' installed. Alternatively, an object of class \code{seqimplic} can be passed
#' to the function.
#'
#' Like in \code{TraMineRextras::plot.seqimplic}, the (sign-reversed)
#' implicative statistic is shown for each state and position as line plot,
#' with a separate plot for each group. Values below 0 are not shown. Values
#' above the dotted reference line(s) indicate that the implication is
#' significant at the respective confidence level.
#'
#' The statistic is reshaped into a long data format and plotted with
#' \code{\link[ggplot2]{geom_line}}. The group-specific plots are produced by using
#' \code{\link[ggplot2]{facet_wrap}}. The data and specifications used for
#' rendering the plot can be obtained by storing the plot as an object. The
#' appearance of the plot can be adjusted just like with every other ggplot
#' (e.g., by changing the theme or the scale using \code{+} and the respective
#' functions).
#'
#' @author Marcel Raab
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' library(TraMineR)
#'
#' # Use example data from TraMineR: mvad data set
#' data(mvad)
#' mvad.seq <- seqdef(mvad, 17:86)
#'
#' if (requireNamespace("TraMineRextras", quietly = TRUE)) {
#'   # typical states of pupils with/without 5 or more GCSEs
#'   # with TraMineRextras
#'   mvad.si.gcse5eq <- TraMineRextras::seqimplic(mvad.seq, group = mvad$gcse5eq)
#'   plot(mvad.si.gcse5eq, lwd = 3, conf.level = c(0.95, 0.99))
#'
#'   # with ggseqplot (using the seqimplic object ...)
#'   ggseqimplicplot(seqimplicobject = mvad.si.gcse5eq,
#'                   conf.level = c(0.95, 0.99))
#'
#'   # ... or using the sequence data and the group vector)
#'   ggseqimplicplot(mvad.seq, group = mvad$gcse5eq,
#'                   conf.level = c(0.95, 0.99), facet_ncol = 1)
#' }
#'
#' @import ggplot2
#' @importFrom rlang .data
ggseqimplicplot <- function(seqdata = NULL,
                            group = NULL,
                            seqimplicobject = NULL,
                            weighted = TRUE,
                            with.missing = FALSE,
                            conf.level = 0.95,
                            linewidth = 1,
                            facet_ncol = NULL,
                            facet_nrow = NULL) {
  if (is.null(seqimplicobject)) {
    if (!requireNamespace("TraMineRextras", quietly = TRUE)) {
      stop("package 'TraMineRextras' is required for computing the implicative statistic, please install it")
    }

    if (!inherits(seqdata, "stslist")) {
      stop("data are not stored as sequence object, use 'TraMineR::seqdef' to create one")
    }

    if (is.null(group)) {
      stop("a group vector has to be specified")
    }

    if (length(group) != nrow(seqdata)) {
      stop("length of group vector must match number of rows of seqdata")
    }

    if (!is.logical(weighted) || !is.logical(with.missing)) {
      stop("the arguments `weighted` and `with.missing` have to be objects of type logical")
    }

    if (inherits(group, "haven_labelled")) {
      group_name <- deparse(substitute(group))
      group <- haven::as_factor(group)
      cli::cli_warn(c("i" = "group vector {.arg {group_name}} is of class {.cls haven_labelled} and has been converted into a factor"))
    }

    # seqimplic warns if missing values are discarded; we keep this warning
    seqimplicobject <- TraMineRextras::seqimplic(seqdata,
                                                 group = group,
                                                 with.missing = with.missing,
                                                 weighted = weighted)
  } else if (!inherits(seqimplicobject, "seqimplic")) {
    stop("seqimplicobject is not of class seqimplic, use 'TraMineRextras::seqimplic' to create one")
  }

  if (!is.null(conf.level) &&
      (!is.numeric(conf.level) || any(conf.level <= 0 | conf.level >= 1))) {
    stop("`conf.level` must be NULL or a numeric vector with values between 0 and 1")
  }

  if (!is.null(facet_ncol) && as.integer(facet_ncol) != facet_ncol) {
    stop("`facet_ncol` must be NULL or an integer.")
  }

  if (!is.null(facet_nrow) && as.integer(facet_nrow) != facet_nrow) {
    stop("`facet_nrow` must be NULL or an integer.")
  }


  # reshape the indices (array: group x state x position) into long format ----

  positions <- dimnames(seqimplicobject$indices)[[3]]

  implicplotdata <- as.data.frame.table(seqimplicobject$indices,
                                        responseName = "value",
                                        stringsAsFactors = FALSE) |>
    dplyr::as_tibble() |>
    dplyr::rename(group = "Var1", state = "Var2", k = "Var3") |>
    dplyr::mutate(
      # like in TraMineRextras: sign-reversed indices, negative values not shown
      value = -.data$value,
      value = dplyr::if_else(.data$value < 0, NA_real_, .data$value),
      group = factor(.data$group, levels = seqimplicobject$levels),
      state = factor(.data$state,
                     levels = seqimplicobject$alphabet,
                     labels = seqimplicobject$labels),
      k = factor(.data$k, levels = positions),
      x = as.integer(.data$k)
    )


  # reference lines for the confidence levels ---------------------------------

  if (!is.null(conf.level)) {
    if (seqimplicobject$type == "intensity") {
      conflines <- conf.level
    } else {
      conflines <- stats::qnorm(conf.level)
    }

    confdata <- dplyr::tibble(
      y = conflines,
      label = paste("Conf.", format(conf.level))
    )
  }


  # x-axis breaks and labels (as in the other ggseq*plot functions) -----------

  kbreaks <- seq_along(positions)

  xbrks <- pretty(seq_along(kbreaks))
  xbrks[1] <- 1
  xbrks[length(xbrks)] <- length(kbreaks)

  if (xbrks[length(xbrks)] == xbrks[length(xbrks) - 1] + 1) {
    xbrks <- xbrks[xbrks != xbrks[length(xbrks) - 1]]
  }
  if (xbrks[1] == xbrks[2] - 1) {
    xbrks <- xbrks[xbrks != xbrks[2]]
  }

  kbreaks <- kbreaks[xbrks]
  klabels <- positions[xbrks]


  # plot ----------------------------------------------------------------------

  ggimplicplot <- implicplotdata |>
    ggplot(aes(x = .data$x, y = .data$value,
               color = .data$state, group = .data$state)) +
    geom_line(linewidth = linewidth, na.rm = TRUE)

  if (!is.null(conf.level)) {
    ggimplicplot <- ggimplicplot +
      geom_hline(data = confdata,
                 aes(yintercept = .data$y),
                 linetype = "dotted", color = "grey40") +
      geom_text(data = confdata,
                aes(x = length(positions), y = .data$y, label = .data$label),
                inherit.aes = FALSE,
                hjust = 1, vjust = -0.5, size = 3, color = "grey40")
  }

  ggimplicplot <- ggimplicplot +
    scale_color_manual(values = seqimplicobject$cpal, drop = FALSE) +
    # extra space at the top for the labels of the confidence levels
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, .1))) +
    scale_x_continuous(
      expand = expansion(add = .15),
      breaks = kbreaks,
      labels = klabels,
      guide = guide_axis(check.overlap = TRUE)
    ) +
    # clip = "off": labels of the confidence levels are never cut off
    coord_cartesian(clip = "off") +
    facet_wrap(~ .data$group,
               ncol = facet_ncol,
               nrow = facet_nrow) +
    labs(x = "", y = "Implication") +
    theme_minimal() +
    theme(
      axis.title.y = element_text(vjust = +3),
      axis.line.x = element_line(linewidth = .3),
      axis.ticks = element_line(linewidth = .3),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(2, "lines"),
      strip.text.x = element_text(margin = margin(b = 10, t = 0)),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.margin = margin(-0.2, 0, 0, -0.2, unit = "cm"),
      plot.margin = margin(15, 15, 10, 15)
    )

  return(ggimplicplot)
}
