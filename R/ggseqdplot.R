#' Render sequence distribution plot with ggplot2
#'
#' Function for rendering state distribution plots with \code{{ggplot2}} instead of base
#' R's \code{plot} function that is used by \code{TraMineR::seqplot}.
#'
#' @param seqdata seqdata State sequence object (class \code{stslist}) created with the \code{TraMineR::seqdef} function.
#' @param group Grouping variable of length equal to the number of sequences.
#' When not NULL, a distinct plot is generated for each level of group.
#' @param weighted Specifies if weights defined when generating the sequence object should be used for computing the state distributions. Default is \code{TRUE}, i.e. if available weights are used
#' @param with.missing Specifies if missing states should be considered when computing the state distributions.
#' @param border if \code{TRUE} (default) bars are plotted with black outline
#' @param with.entropy add line plot of cross-sectional entropies at each sequence position
#' @param linetype The linetype for the entropy subplot (\code{with.entropy==TRUE}) can be specified with an integer (0-6) or name (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash); ; default is \code{"dashed"}
#' @param linecolor Specifies the color of the entropy line if \code{with.entropy==TRUE}; default is \code{"black"}
#' @param linewidth Specifies the with of the entropy line if \code{with.entropy==TRUE}; default is \code{1}
#'
#' @return A sequence distribution plot. If stored as object the resulting list
#' object also contains the data (long format) used for rendering the plot
#' @export
#'
#' @details The function uses \code{TraMineR::seqstatd} to obtain state distributions. Obviously this requires that the
#' input data (\code{seqdata}) is stored as state sequence object (class \code{stslist}) created with the \code{TraMineR::seqdef} function.
#'
#' @examples
#'
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
#' actcal <- actcal[sample(nrow(actcal),300),]
#' actcal.lab <- c("> 37 hours", "19-36 hours", "1-18 hours", "no work")
#' actcal.seq <- seqdef(actcal,13:24,labels=actcal.lab)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' # state distribution plots; grouped by sex
#' # with TraMineR::seqplot
#' seqdplot(actcal.seq, group=actcal$sex)
#' # with ggseqplot
#' ggseqdplot(actcal.seq, group=actcal$sex)
#' # with ggseqplot and weights turned off
#' ggseqdplot(actcal.seq, group=actcal$sex, weighted = FALSE)
#'
#' # make use of ggplot functions for modifying the plot
#' ggseqdplot(actcal.seq) +
#'   scale_x_discrete(labels = month.abb) +
#'   labs(title = "State distribution plot",
#'        x = "Month") +
#'   guides(fill=guide_legend(title="Alphabet")) +
#'   theme_classic() +
#'   theme(plot.title = element_text(size = 30,
#'                                   margin=margin(0,0,20,0)),
#'         plot.title.position = "plot")
#'
#' @import ggplot2
ggseqdplot <- function(seqdata,
                       group = NULL,
                       weighted = TRUE,
                       with.missing = FALSE,
                       border = TRUE,
                       with.entropy = FALSE,
                       linetype = "dashed",
                       linecolor = "black",
                       linewidth = 1) {

  if (!inherits(seqdata, "stslist"))
    stop("data is not a sequence object, use 'TraMineR::seqdef' to create one")


  if (!is.null(group) & (length(group) != nrow(seqdata)))
    stop("length of group vector must match number of rows of seqdata")


  if(!is.logical(weighted) | !is.logical(with.missing) | !is.logical(border))
    stop("the arguments `weighted`, `with.missing`, and `border` have to be objects of type logical")

  if (is.null(attributes(seqdata)$weights)) weighted <- FALSE

  if (is.null(group)) group <- 1

  statefreqs <- purrr::map(unique(group),
                           ~TraMineR::seqstatd(seqdata[group == .x,],
                                               weighted = weighted,
                                               with.missing = with.missing)$Frequencies |>
                             dplyr::as_tibble(rownames = "state") |>
                             dplyr::mutate(group = .x, .before = 1)) |>
    dplyr::bind_rows()



  if (with.entropy == TRUE) {
    stateentropy <- purrr::map(unique(group),
                               ~TraMineR::seqstatd(seqdata[group == .x,],
                                                   weighted = weighted,
                                                   with.missing = with.missing)$Entropy |>
                                 dplyr::as_tibble(rownames = "k") |>
                                 dplyr::mutate(group = .x, .before = 1)) |>
      dplyr::bind_rows()
  }


  ylabspec <- purrr::map(unique(group),
                  ~attributes(TraMineR::seqstatd(seqdata[group == .x,],
                                                 weighted = weighted,
                                                 with.missing = with.missing))$nbseq) |>
    unlist()


  if (length(ylabspec) == 1 & weighted == TRUE) {
    ylabspec <- glue::glue("Rel. Freq. (weighted n={round(ylabspec,2)})")
  } else if (length(ylabspec) == 1 & weighted == FALSE) {
    ylabspec <- glue::glue("Rel. Freq. (n={ylabspec})")
  } else if (weighted == TRUE) {
    ylabspec <- glue::glue("{unique(group)} (weighted n={round(ylabspec,2)})")
  } else {
    ylabspec <- glue::glue("{unique(group)} (n={ylabspec})")
  }

  grouplabspec <- dplyr::tibble(group = unique(group),
                                grouplab = ylabspec)


  suppressMessages(
  dplotdata <- statefreqs |>
    dplyr::rename_with(~ glue::glue("k{1:(ncol(statefreqs)-2)}"),
                       -(1:2)) |>
    dplyr::mutate(state = factor(.data$state,
                                 levels = TraMineR::alphabet(seqdata),
                                 labels = attributes(seqdata)$labels),
                  state = forcats::fct_explicit_na(.data$state,
                                                   na_level="Missing"),
                  state = forcats::fct_rev(.data$state)) |>
    tidyr::pivot_longer(cols = -(1:2),
                        names_to = "k",
                        names_prefix = "k",
                        names_transform = list(k = as.integer)) |>
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


  if("Missing" %in% dplotdata$state == TRUE) {
    cpal <- c(attributes(seqdata)$cpal,
              attributes(seqdata)$missing.color)
  } else {
    cpal <- attributes(seqdata)$cpal
  }

  cpal <- rev(cpal)

  # kbreaks <- pretty(1:length(attributes(seqdata)$names))
  # kbreaks <- kbreaks[kbreaks != 0]
  # klabels <- attributes(seqdata)$names[kbreaks]

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


  if (with.entropy == TRUE) {
    suppressMessages(
      dplotdata <- dplyr::full_join(dplotdata,eplotdata,by=c("group","k"))
    )
  }


  # plot

  if (border == FALSE) {
    ggdplot <- dplotdata |>
      ggplot(aes(fill = .data$state, y = .data$value, x = .data$x)) +
      geom_bar(stat = "identity",
               width = 1)
  } else {
    ggdplot <- dplotdata |>
      ggplot(aes(fill = .data$state, y = .data$value, x = .data$x)) +
      geom_bar(stat = "identity",
               width = 1, color = "black")
  }

  ggdplot <- ggdplot +
    scale_fill_manual(values = cpal) +
    scale_y_continuous(expand = expansion(add = c(.01,0))) +
    scale_x_discrete(expand = expansion(add = .15),
                     breaks = kbreaks,
                     labels = klabels,
                     guide = guide_axis(check.overlap = TRUE)) +
    labs(x = "", y = ylabspec) +
    guides(fill = guide_legend(reverse=TRUE)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.margin = margin(-0.2,0,0,-0.2, unit="cm"))


  grsize <- length(unique(dplotdata$group))

  if (grsize > 1) {
    ggdplot <- ggdplot +
      facet_wrap(~.data$grouplab,
                 scales = "free_y",
                 ncol = 2) +
      labs(x = "", y = "Rel. Freq.") +
      theme(panel.spacing = unit(2, "lines"))
  }

  if (with.entropy==TRUE) {
    ggdplot <- ggdplot +
      geom_line(aes(x = .data$x, y=.data$entropy,  color = linecolor),
                group = 1, size= linewidth, linetype = linetype) +
      scale_color_identity(guide = "legend", name = NULL, labels = "Entropy")
  }


  return(ggdplot)

}
