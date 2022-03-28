#' Render sequence index plot with ggplot2
#'
#' Function for rendering sequence index plots with \code{{ggplot2}} instead of base
#' R's \code{plot} function that is used by \code{TraMineR::seqplot}.
#'
#' @param seqdata State sequence object (class \code{stslist}) created with the \code{TraMineR::seqdef} function.
#' @param group Grouping variable for rendering faceted plots.
#' When not NULL, a distinct plot is generated for each level of group.
#' @param sortv Vector of numerical values for sorting the sequences
#' @param weighted Controls if weights should be used
#' @param border if \code{TRUE} bars are plotted with black outline
#'
#' @return A sequence index plot. If stored as object the resulting list object
#' also contains the data (spell format) used for rendering the plot.
#' @export
#'
#' @details The function uses \code{TraMineR::seqformat} to reshape \code{seqdata} stored in wide format into
#' a spell/episode format. If \code{border=TRUE} the data are reshaped into the long format, i.e. for every sequence
#' each row in the data represents one specific sequence position. For example, if we have 5 sequences of length 10,
#' the long file will have 50 rows. In the case of sequences of unequal length not every sequence will contribute
#' the same number of rows to the long data.
#'
#' The reshaped data are used as input for rendering the index plot using ggplot2's \code{geom_rect}.
#' \code{ggseqiplot} uses \code{geom_rect} instead of \code{geom_tile} because this allows for a straight forward
#' implementation of weights. If weights are specified for \code{seqdata} and \code{weighted=TRUE} the sequence height
#' corresponds to its weight.
#'
#' Note that the default aspect ratio of \code{ggseqiplot} is different from \code{TraMineR::seqIplot}. This is most
#' obvious when \code{border=TRUE}. You can change the ratio either by adding code to \code{ggseqiplot}
#' or by specifying the ratio when saving the code with \code{ggsave}.
#'
#' @author Marcel Raab
#'
#'
#' @examples
#'
#' library(TraMineR)
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
#' # ex1 using weights
#' data(ex1)
#' ex1.seq <- seqdef(ex1, 1:13, weights=ex1$weights)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#'
#' # sequences sorted by age in 2000 and grouped by sex
#' # with TraMineR::seqplot
#' seqIplot(actcal.seq, group=actcal$sex,sortv=actcal$age00)
#' # with ggseqplot
#' ggseqiplot(actcal.seq, group=actcal$sex, sortv=actcal$age00)
#'
#' # sequences of unequal length with missing state, and weights
#' seqIplot(ex1.seq)
#' ggseqiplot(ex1.seq)
#'
#' # ... turn weights off add border
#' seqIplot(ex1.seq, weighted = FALSE, border = TRUE)
#' ggseqiplot(ex1.seq, weighted = FALSE, border = TRUE)
#'
#' @import ggplot2
ggseqiplot <- function(seqdata,
                       group = NULL,
                       sortv = NULL,
                       weighted = TRUE,
                       border = FALSE) {

  if (!inherits(seqdata, "stslist"))
    stop("data is not a sequence object, use 'seqdef' function to create one")

  if (!is.logical(weighted) | !is.logical(border))
    stop("the arguments `weighted` or `border` have to be objects of type logical")


  if (exists("weights", where = attributes(seqdata)) == TRUE &
      weighted == TRUE) {
    weights <-  attributes(seqdata)$weights
  } else {
    weights <- rep(1, nrow(seqdata))
  }


  auxid <- dplyr::tibble(id = as.character(attributes(seqdata)$row.names)) %>%
    dplyr::mutate(idnew = dplyr::row_number(),
                  weight = weights,
                  group = NA)

  if (is.null(group) == FALSE) auxid$group <- group


  if (is.null(sortv) == FALSE) {
    auxid <- auxid %>%
      dplyr::mutate(sortv = {{ sortv }}) %>%
      dplyr::arrange(sortv) %>%
      dplyr::mutate(idnew = dplyr::row_number())
  } else {
    auxid$sortv <- auxid$idnew
  }

  suppressMessages(
    spelldata <- TraMineR::seqformat(seqdata, to = "SPELL") %>%
      dplyr::full_join(auxid) %>%
      dplyr::select(.data$idnew, dplyr::everything()) %>%
      dplyr::mutate(states = factor(.data$states,
                                    levels = TraMineR::alphabet(seqdata),
                                    labels = attributes(seqdata)$labels),
                    states = forcats::fct_explicit_na(.data$states,
                                                      na_level="Missing")) %>%
      dplyr::group_by(.data$idnew) %>%
      dplyr::mutate(spell = dplyr::row_number(), .after = 1) %>%
      dplyr::as_tibble() %>%
      dplyr::rename(left = .data$begin, right = .data$end)
  )


  suppressMessages(
    dt <- spelldata %>%
      dplyr::arrange(sortv) %>%
      dplyr::select(.data$idnew, .data$weight, group) %>%
      dplyr::distinct(.data$idnew, .keep_all = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(begin = 0, end = cumsum(.data$weight)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-.data$weight, -.data$group) %>%
      dplyr::full_join(spelldata, by = "idnew")
  )


  dt2 <- dt %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(begin = ifelse(dplyr::row_number()==1,0,NA)) %>%
    dplyr::arrange(.data$spell, .data$idnew) %>%
    dplyr::mutate(begin = ifelse(is.na(.data$begin),.data$end - .data$weight, .data$begin),
                  left = .data$left - 1) %>%
    dplyr::arrange(.data$idnew, .data$spell) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(left = ifelse(is.na(.data$left),0,.data$left),
                  right = ifelse(is.na(.data$right),0,.data$right))


  if ("Missing" %in% dt2$states == TRUE) {
    cpal <- c(attributes(seqdata)$cpal,
              attributes(seqdata)$missing.color)
  } else {
    cpal <- attributes(seqdata)$cpal
  }


  kbreaks <- .5:(length(attributes(seqdata)$names)-.5)
  klabels <- attributes(seqdata)$names


  if (length(kbreaks) > 15) {
    kbreaks <- kbreaks[seq(1, length(kbreaks),2)]
    klabels <- klabels[seq(1, length(klabels),2)]
  }

  if (length(kbreaks) > 6 & is.null(group) == FALSE) {
    kbreaks <- kbreaks[seq(1, length(kbreaks),2)]
    klabels <- klabels[seq(1, length(klabels),2)]
  }


  if (border == FALSE) {
    suppressMessages(
      ggiplot <- dt2 %>%
        ggplot() +
        geom_rect(aes(xmin = .data$left, xmax = .data$right,
                      ymin = .data$begin, ymax = .data$end,
                      fill = .data$states, colour = .data$states)) +
        scale_fill_manual(values = cpal)  +
        scale_color_manual(values = cpal)  +
        theme_minimal() +
        theme(legend.position = "bottom",
              legend.title = element_blank())
    )
  } else {
    suppressMessages(
      ggiplot <- dt2 %>%
        dplyr::mutate(aux = .data$right - .data$left,
                      aux2 = .data$aux,
                      aux = ifelse(.data$aux==0, 1, .data$aux)) %>%
        tidyr::uncount(.data$aux) %>%
        dplyr::select(-.data$aux) %>%
        dplyr::group_by(.data$idnew) %>%
        dplyr::mutate(left = dplyr::row_number() - 1,
                      right = ifelse(.data$aux2==0,.data$left, .data$left + 1)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-.data$aux2) %>%
        ggplot() +
        geom_rect(aes(xmin = .data$left, xmax = .data$right,
                      ymin = .data$begin, ymax = .data$end,
                      fill = .data$states), colour = "black") +
        scale_fill_manual(values = cpal)  +
        scale_color_manual(values = cpal)  +
        theme_minimal() +
        theme(legend.position = "bottom",
              legend.title = element_blank())
    )
  }


  if (is.null(group) == FALSE) {
    suppressMessages(
      ggiplot <- ggiplot +
        facet_wrap(~.data$group, scales = "free_y") +
        theme(panel.spacing = unit(2, "lines"))
    )
  }

  suppressMessages(
    ggiplot <- ggiplot +
      scale_x_continuous(breaks = kbreaks,
                         labels= klabels,
                         expand = expansion(add = c(0.2, 0))) +
      scale_y_continuous(expand = expansion(add = c(0, 0)))
  )

  return(ggiplot)
}