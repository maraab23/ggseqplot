#' Sequence Distribution Plot
#'
#' Function for rendering state distribution plots with \code{\link[ggplot2]{ggplot2}}
#' \insertCite{wickham2016}{ggseqplot} instead of base R's \code{\link[base]{plot}}
#' function that is used by \code{\link[TraMineR:seqplot]{TraMineR::seqplot}}
#' \insertCite{gabadinho2011}{ggseqplot}.
#'
#' @eval shared_params()
#' @param no.n specifies if number of (weighted) sequences is shown (default is \code{TRUE})
#' @param with.missing Specifies if missing states should be considered when computing the state distributions (default is \code{FALSE}).
#' @param border if \code{TRUE} bars are plotted with black outline; default is \code{FALSE} (also accepts \code{NULL})
#' @param with.entropy add line plot of cross-sectional entropies at each sequence position
#' @param linetype The linetype for the entropy subplot (\code{with.entropy==TRUE}) can be specified with an integer (0-6) or name (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash); ; default is \code{"dashed"}
#' @param linecolor Specifies the color of the entropy line if \code{with.entropy==TRUE}; default is \code{"black"}
#' @param linewidth Specifies the width of the entropy line if \code{with.entropy==TRUE}; default is \code{1}
#' @eval shared_facet()
#'
#' @return A sequence distribution plot created by using \code{\link[ggplot2]{ggplot2}}.
#' If stored as object the resulting list object (of class gg and ggplot) also
#' contains the data used for rendering the plot.
#'
#' @export
#'
#' @details Sequence distribution plots visualize the distribution of all states
#' by rendering a series of stacked bar charts at each position of the sequence.
#' Although this type of plot has been used in the life course studies for several
#' decades (see \insertCite{blossfeld1987;textual}{ggseqplot} for an early application),
#' it should be noted that the size of the different bars in stacked bar charts
#' might be difficult to compare - particularly if the alphabet comprises many
#' states  \insertCite{wilke2019}{ggseqplot}. Moreover, it is important to keep
#' in mind that this plot type does not visualize individual trajectories;
#' instead it displays aggregated distributional information (repeated cross-sections).
#' For a more detailed discussion of this  type of sequence visualization see,
#' for example, \insertCite{brzinsky-fay2014;textual}{ggseqplot},
#' \insertCite{fasang2014;textual}{ggseqplot}, and \insertCite{raab2022;textual}{ggseqplot}.
#'
#' The function uses \code{\link[TraMineR:seqstatd]{TraMineR::seqstatd}} to obtain state
#' distributions (and entropy values). This requires that the input data (\code{seqdata})
#' are stored as state sequence object (class \code{stslist}) created with
#' the \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function. The state distributions
#' are reshaped into a a long data format to enable plotting with \code{\link[ggplot2]{ggplot2}}.
#' The stacked bars are rendered by calling \code{\link[ggplot2]{geom_bar}}; if \code{entropy = TRUE}
#' entropy values are plotted with \code{\link[ggplot2]{geom_line}}. The data
#' and specifications used for rendering the plot can be obtained by storing the
#' plot as an object. The appearance of the plot can be adjusted just like with
#' every other ggplot (e.g., by changing the theme or the scale using \code{+} and
#' the respective functions).
#'
#'
#' @author Marcel Raab
#'
#' @references
#'   \insertAllCited{}
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
#' # state distribution plots; grouped by sex
#' # with TraMineR::seqplot
#' seqdplot(actcal.seq, group = actcal$sex)
#' # with ggseqplot
#' ggseqdplot(actcal.seq, group = actcal$sex)
#' # with ggseqplot applying a few additional arguments, e.g. entropy line
#' ggseqdplot(actcal.seq, group = actcal$sex,
#'            no.n = TRUE, with.entropy = TRUE, border = TRUE)
#'
#' # make use of ggplot functions for modifying the plot
#' ggseqdplot(actcal.seq) +
#'   scale_x_discrete(labels = month.abb) +
#'   labs(title = "State distribution plot", x = "Month") +
#'   guides(fill = guide_legend(title = "Alphabet")) +
#'   theme_classic() +
#'   theme(plot.title = element_text(size = 30,
#'                                   margin = margin(0, 0, 20, 0)),
#'     plot.title.position = "plot")
#'
#' @import ggplot2
ggseqdplot <- function(seqdata,
                       no.n = FALSE,
                       group = NULL,
                       weighted = TRUE,
                       with.missing = FALSE,
                       border = FALSE,
                       with.entropy = FALSE,
                       linetype = "dashed",
                       linecolor = "black",
                       linewidth = 1,
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

  statefreqs <- purrr::map(
    unique(group),
    ~ TraMineR::seqstatd(seqdata[group == .x, ],
                         weighted = weighted,
                         with.missing = with.missing
    )$Frequencies |>
      dplyr::as_tibble(rownames = "state") |>
      dplyr::mutate(group = .x, .before = 1)
  ) |>
    dplyr::bind_rows()

  if (with.entropy == TRUE) {
    stateentropy <- purrr::map(
      unique(group),
      ~ TraMineR::seqstatd(seqdata[group == .x, ],
                           weighted = weighted,
                           with.missing = with.missing
      )$Entropy |>
        dplyr::as_tibble(rownames = "k") |>
        dplyr::mutate(group = .x, .before = 1)
    ) |>
      dplyr::bind_rows()
  }


  xandgrouplabs <- xandgrouplab(seqdata = seqdata,
                                weighted = weighted,
                                no.n = no.n,
                                group = group,
                                ylabprefix = "Rel. Freq.")
  grouplabspec <- xandgrouplabs[[1]]
  ylabspec <- xandgrouplabs[[2]]


  suppressMessages(
    dplotdata <- statefreqs |>
      dplyr::rename_with(
        ~ glue::glue("k{1:(ncol(statefreqs)-2)}"),
        -(1:2)
      ) |>
      dplyr::mutate(
        state = factor(.data$state,
                       levels = TraMineR::alphabet(seqdata),
                       labels = attributes(seqdata)$labels
        ),
        state = forcats::fct_explicit_na(.data$state,
                                         na_level = "Missing"
        ),
        state = forcats::fct_rev(.data$state)
      ) |>
      tidyr::pivot_longer(
        cols = -(1:2),
        names_to = "k",
        names_prefix = "k",
        names_transform = list(k = as.integer)
      ) |>
      dplyr::mutate(k = factor(.data$k, labels = colnames(statefreqs)[-(1:2)])) |>
      dplyr::mutate(x = factor(as.integer(.data$k)), .after = .data$k) |>
      dplyr::full_join(grouplabspec)
  )

  if (with.entropy == TRUE) {
    suppressMessages(
      eplotdata <- stateentropy |>
        dplyr::mutate(k = factor(.data$k, levels = unique(.data$k))) |>
        dplyr::rename(entropy = .data$value)
    )
  }


  if ("Missing" %in% dplotdata$state == TRUE) {
    cpal <- c(
      attributes(seqdata)$cpal,
      attributes(seqdata)$missing.color
    )
  } else {
    cpal <- attributes(seqdata)$cpal
  }

  cpal <- rev(cpal)

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


  if (with.entropy == TRUE) {
    suppressMessages(
      dplotdata <- dplyr::full_join(dplotdata, eplotdata, by = c("group", "k"))
    )
  }


  # plot

  if (border == FALSE) {
    ggdplot <- dplotdata |>
      ggplot(aes(fill = .data$state, y = .data$value, x = .data$x)) +
      geom_bar(
        stat = "identity",
        width = 1
      )
  } else {
    ggdplot <- dplotdata |>
      ggplot(aes(fill = .data$state, y = .data$value, x = .data$x)) +
      geom_bar(
        stat = "identity",
        width = 1, color = "black"
      )
  }

  ggdplot <- ggdplot +
    scale_fill_manual(values = cpal) +
    scale_y_continuous(expand = expansion(add = c(.01, 0))) +
    scale_x_discrete(
      expand = expansion(add = .15),
      breaks = kbreaks,
      labels = klabels,
      guide = guide_axis(check.overlap = TRUE)
    ) +
    labs(x = "", y = ylabspec) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(
      axis.title.y = element_text(vjust = +3),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.margin = margin(-0.2, 0, 0, -0.2, unit = "cm")
    )


  grsize <- length(unique(dplotdata$group))

  if (grsize > 1) {
    ggdplot <- ggdplot +
      facet_wrap(~ .data$grouplab,
                 scales = "free_y",
                 ncol = facet_ncol,
                 nrow = facet_nrow
      ) +
      labs(x = "", y = "Rel. Freq.") +
      theme(panel.spacing = unit(2, "lines"),
            strip.text.x = element_text(margin = margin( b = 10, t = 0)))
  }

  if (with.entropy == TRUE) {
    ggdplot <- ggdplot +
      geom_line(aes(x = .data$x, y = .data$entropy, color = linecolor),
                group = 1, size = linewidth, linetype = linetype
      ) +
      scale_color_identity(guide = "legend", name = NULL, labels = "Entropy")
  }


  ggdplot <- ggdplot +
    theme(plot.margin = margin(15, 15, 10, 15))

  return(ggdplot)
}
