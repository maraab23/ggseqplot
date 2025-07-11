#' Sequence Distribution Plot
#'
#' Function for rendering state distribution plots with \code{\link[ggplot2]{ggplot2}}
#' \insertCite{wickham2016}{ggseqplot} instead of base R's \code{\link[base]{plot}}
#' function that is used by \code{\link[TraMineR:seqplot]{TraMineR::seqplot}}
#' \insertCite{gabadinho2011}{ggseqplot}.
#'
#' @param seqdata State sequence object (class \code{stslist}) created with the \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function.
#' @param weighted Controls if weights (specified in \code{\link[TraMineR:seqdef]{TraMineR::seqdef}}) should be used. Default is \code{TRUE}, i.e. if available weights are used
#' @param group A vector of the same length as the sequence data indicating group membership. When not NULL, a distinct plot is generated for each level of group.
#' @param no.n specifies if number of (weighted) sequences is shown (default is \code{TRUE})
#' @param dissect if \code{"row"} or \code{"col"} are specified separate distribution plots instead of a stacked plot are displayed;
#' \code{"row"} and \code{"col"} display the distributions in one row or one column respectively; default is \code{NULL}
#' @param with.missing Specifies if missing states should be considered when computing the state distributions (default is \code{FALSE}).
#' @param border if \code{TRUE} bars are plotted with black outline; default is \code{FALSE} (also accepts \code{NULL})
#' @param with.entropy add line plot of cross-sectional entropies at each sequence position
#' @param linetype The linetype for the entropy subplot (\code{with.entropy==TRUE}) can be specified with an integer (0-6) or name (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash); ; default is \code{"dashed"}
#' @param linecolor Specifies the color of the entropy line if \code{with.entropy==TRUE}; default is \code{"black"}
#' @param linewidth Specifies the width of the entropy line if \code{with.entropy==TRUE}; default is \code{1}
#' @param facet_ncol Number of columns in faceted (i.e. grouped) plot
#' @param facet_nrow Number of rows in faceted (i.e. grouped) plot
#' @param ... if group is specified additional arguments of \code{\link[ggplot2:facet_wrap]{ggplot2::facet_wrap}}
#' such as \code{"labeller"} or \code{"strip.position"} can be used to change the appearance of the plot. Does
#' not work if \code{dissect} is used
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
#' states  \insertCite{wilke2019}{ggseqplot}. This issue can be addressed by breaking down
#' the aggregated  distribution specifying the \code{dissect} argument.  Moreover, it
#' is important to keep in mind that this plot type does not visualize individual
#' trajectories; instead it displays aggregated distributional information
#' (repeated cross-sections). For a more detailed discussion of this  type of
#' sequence visualization see, for example, \insertCite{brzinsky-fay2014;textual}{ggseqplot},
#' \insertCite{fasang2014;textual}{ggseqplot}, and \insertCite{raab2022;textual}{ggseqplot}.
#'
#' The function uses \code{\link[TraMineR:seqstatd]{TraMineR::seqstatd}} to obtain state
#' distributions (and entropy values). This requires that the input data (\code{seqdata})
#' are stored as state sequence object (class \code{stslist}) created with
#' the \code{\link[TraMineR:seqdef]{TraMineR::seqdef}} function. The state distributions
#' are reshaped into a a long data format to enable plotting with \code{\link[ggplot2]{ggplot2}}.
#' The stacked bars are rendered by calling \code{\link[ggplot2]{geom_bar}}; if \code{entropy = TRUE}
#' entropy values are plotted with \code{\link[ggplot2]{geom_line}}. If the \code{group} or the
#' \code{dissect} argument are specified the sub-plots are produced by using
#' \code{\link[ggplot2]{facet_wrap}}. If both are specified the plots are rendered with
#' \code{\link[ggplot2]{facet_grid}}.
#'
#' The data and specifications used for rendering the plot can be obtained by storing the
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
#' # Use example data from TraMineR: actcal data set
#' data(actcal)
#'
#' # We use only a sample of 300 cases
#' set.seed(1)
#' actcal <- actcal[sample(nrow(actcal), 300), ]
#' actcal.lab <- c("> 37 hours", "19-36 hours", "1-18 hours", "no work")
#' actcal.seq <- seqdef(actcal, 13:24, labels = actcal.lab)
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
#' # break down the stacked plot to ease comparisons of distributions
#' ggseqdplot(actcal.seq, group = actcal$sex, dissect = "row")
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
#' @importFrom rlang .data
ggseqdplot <- function(seqdata,
                       no.n = FALSE,
                       group = NULL,
                       dissect = NULL,
                       weighted = TRUE,
                       with.missing = FALSE,
                       border = FALSE,
                       with.entropy = FALSE,
                       linetype = "dashed",
                       linecolor = "black",
                       linewidth = 1,
                       facet_ncol = NULL,
                       facet_nrow = NULL,
                       ...) {
  if (!inherits(seqdata, "stslist")) {
    stop("data are not stored as sequence object, use 'TraMineR::seqdef' to create one")
  }

  if (!is.null(dissect) && with.entropy == TRUE) {
    cli::cli_warn(c(
      "!" = "You tried to render a disaggregated dplot using `dissect`, while also setting `with.entropy` to `TRUE`",
      "i" = "As the state-specific distrubution plots would repeatedly show the same entropy line, `with.entropy = TRUE` is ignored."
    ))
    with.entropy <- FALSE
  }

  if (!is.null(group) && (length(group) != nrow(seqdata))) {
    stop("length of group vector must match number of rows of seqdata")
  }

  border <- border %||% FALSE

  if (!is.logical(weighted) || !is.logical(with.missing) ||
      !is.logical(border) || !is.logical(no.n)) {
    stop("the arguments `no.n`, `weighted`, `with.missing`, and `border` have to be objects of type logical")
  }

  if (is.null(attributes(seqdata)$weights)) weighted <- FALSE


  group <- group %||% 1

  if (!is.null(facet_ncol) && as.integer(facet_ncol) != facet_ncol) {
    stop("`facet_ncol` must be NULL or an integer.")
  }

  if (!is.null(facet_nrow) && as.integer(facet_nrow) != facet_nrow) {
    stop("`facet_nrow` must be NULL or an integer.")
  }

  if (inherits(group, "haven_labelled")) {
    group_name <- deparse(substitute(group))
    group <- haven::as_factor(group)
    cli::cli_warn(c("i" = "group vector {.arg {group_name}} is of class {.cls haven_labelled} and has been converted into a factor"))
  }

  if (is.factor(group)) {
    group <- forcats::fct_drop(group)
    grinorder <- levels(group)
  } else {
    grinorder <- factor(sort(unique(group)))
  }

  statefreqs <- purrr::map(
    grinorder,
    ~ TraMineR::seqstatd(seqdata[group == .x, ],
                         weighted = weighted,
                         with.missing = with.missing
    )$Frequencies |>
      dplyr::as_tibble(rownames = "state") |>
      dplyr::mutate(group = .x, .before = 1)
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(group = factor(.data$group, levels = grinorder))

  if (with.entropy == TRUE) {
    stateentropy <- purrr::map(
      grinorder,
      ~ TraMineR::seqstatd(seqdata[group == .x, ],
                           weighted = weighted,
                           with.missing = with.missing
      )$Entropy |>
        dplyr::as_tibble(rownames = "k") |>
        dplyr::mutate(group = .x, .before = 1)
    ) |>
      dplyr::bind_rows() |>
      dplyr::mutate(group = factor(.data$group, levels = grinorder))
  }


  xandgrouplabs <- xandgrouplab(seqdata = seqdata,
                                weighted = weighted,
                                no.n = no.n,
                                group = group,
                                grinorder = grinorder,
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
        state = forcats::fct_na_value_to_level(.data$state,
                                                level = "Missing"
        ),
        state = forcats::fct_drop(.data$state, "Missing"), # shouldn't be necessary
        state = forcats::fct_rev(.data$state)
      ) |>
      tidyr::pivot_longer(
        cols = -(1:2),
        names_to = "k",
        names_prefix = "k",
        names_transform = list(k = as.integer)
      ) |>
      dplyr::mutate(k = factor(.data$k, labels = colnames(statefreqs)[-(1:2)])) |>
      dplyr::mutate(x = factor(as.integer(.data$k)), .after = "k") |>
      dplyr::full_join(grouplabspec)
  )

  if (with.entropy == TRUE) {
    suppressMessages(
      eplotdata <- stateentropy |>
        dplyr::mutate(k = factor(.data$k, levels = unique(.data$k))) |>
        dplyr::rename(entropy = "value")
    )
  }


  if ("Missing" %in% dplotdata$state) {
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
        width = 1,
        show.legend = T
      )
  } else {
    ggdplot <- dplotdata |>
      ggplot(aes(fill = .data$state, y = .data$value, x = .data$x)) +
      geom_bar(
        stat = "identity",
        width = 1, color = "black",
        show.legend = TRUE
      )
  }

  ggdplot <- ggdplot +
    scale_fill_manual(values = cpal) +
    scale_y_continuous(expand = expansion(add = 0)) +
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
      axis.line.x = element_line(linewidth = .3),
      axis.ticks = element_line(linewidth = .3),
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
                 nrow = facet_nrow,
                 ...
      ) +
      labs(x = "", y = "Rel. Freq.") +
      theme(panel.spacing = unit(2, "lines"),
            strip.text.x = element_text(margin = margin( b = 10, t = 0)))
  }

  if (with.entropy == TRUE) {
    ggdplot <- ggdplot +
      geom_line(aes(x = .data$x, y = .data$entropy, color = linecolor),
                group = 1, linewidth = linewidth, linetype = linetype
      ) +
      scale_color_identity(guide = "legend", name = NULL, labels = "Entropy")
  }


  if (grsize == 1 && !is.null(dissect)) {
    suppressMessages(
      ggdplot <- ggdplot +
        {if(dissect == "row")facet_wrap(~rev(.data$state), nrow = 1)} +
        {if(dissect == "col")facet_wrap(~rev(.data$state), ncol = 1)} +
        scale_y_continuous(limits = c(0,1),
                           expand = expansion(add = 0)) +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              panel.spacing = unit(2, "lines"),
              strip.text = element_blank())
    )
  }


  if (grsize > 1 && !is.null(dissect)) {
    suppressMessages(
      ggdplot <- ggdplot +
        {if(dissect == "row")facet_grid(vars(.data$grouplab), vars(rev(.data$state)), switch = "y")} +
        {if(dissect == "col")facet_grid(vars(.data$state), vars(.data$grouplab), switch = "y")} +
        scale_y_continuous(limits = c(0,1),
                           expand = expansion(add = 0)) +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              panel.spacing = unit(2, "lines"),
              strip.placement = "outside") +
        {if(dissect == "col")theme(strip.text.y = element_blank())} +
        {if(dissect == "row")theme(strip.text.x = element_blank())}
    )
  }


  ggdplot <- ggdplot +
    theme(plot.margin = margin(15, 15, 10, 15))

  return(ggdplot)
}
