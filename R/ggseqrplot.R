#' Representative Sequence plot
#'
#' Function for rendering representative sequence plots with
#' \code{\link[ggplot2]{ggplot2}} \insertCite{wickham2016}{ggseqplot} instead of base
#' R's \code{\link[base]{plot}} function that is used by
#' \code{\link[TraMineR:seqplot]{TraMineR::seqplot}} \insertCite{gabadinho2011}{ggseqplot}.
#'
#' @eval shared_params()
#' @inheritParams TraMineR::seqrep
#' @param diss pairwise dissimilarities between sequences in \code{seqdata} (see \code{TraMineR::seqdist})
#' @param border if \code{TRUE} bars are plotted with black outline; default is \code{FALSE} (also accepts \code{NULL})
#' @param proportional if \code{TRUE} (default), the sequence heights are
#' displayed proportional to the number of represented sequences
#' @param stats if \code{TRUE} (default), mean discrepancy in each subset
#' defined by all sequences attributed to one representative sequence and the
#' mean distance to this representative sequence are displayed.
#' @param colored.stats specifies if representatives in stats plot should be
#' color coded; only recommended if number of representatives is small;
#' if set to \code{NULL} (default) colors are used if n rep. <= 10;
#' use \code{TRUE} or \code{FALSE} to change manually
#' @param facet_ncol specifies the number of columns in the plot (relevant if !is.null(group))
#'
#' @details
#' The representative sequence plot displays a set of distinct sequences as sequence index plot.
#' The set of representative sequences is extracted from the sequence data by an internal call of
#' \code{\link[TraMineR:seqrep]{TraMineR::seqrep}} according to the criteria listed in the
#' arguments section above.
#'
#' The extracted sequences are plotted by a call of \code{\link[ggseqplot:ggseqiplot]{ggseqiplot}} which uses
#' \code{\link[ggplot2:geom_rect]{ggplot2::geom_rect}} to render the sequences. If \code{stats = TRUE} the
#' index plots are complemented by information on the "quality" of the representative sequences.
#' For further details on representative sequence plots see \insertCite{gabadinho2011a;textual}{ggseqplot}
#' and the documentation of \code{\link[TraMineR:plot.stslist.rep]{TraMineR::plot.stslist.rep}},
#' \code{\link[TraMineR:seqplot]{TraMineR::seqplot}}, and \code{\link[TraMineR:seqrep]{TraMineR::seqrep}}.
#'
#' Note that \code{ggseqrplot} uses \code{\link[patchwork]{patchwork}} to combine the different components
#' of the plot. If you want to adjust the appearance of the composed plot, for instance by changing the
#' plot theme, you should consult the documentation material of \code{\link[patchwork]{patchwork}}.
#'
#' @return A representative sequence plot using \code{\link[ggplot2]{ggplot}}.
#' @export
#' @importFrom patchwork plot_layout
#'
#' @author Marcel Raab
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' # Use examples from TraMineR
#' library(TraMineR)
#' # Defining a sequence object with the data in columns 10 to 25
#' # (family status from age 15 to 30) in the biofam data set
#' data(biofam)
#' # Use sample of 300 cases
#' set.seed(123)
#' biofam <- biofam[sample(nrow(biofam),150),]
#' biofam.lab <- c("Parent", "Left", "Married", "Left+Marr",
#' "Child", "Left+Child", "Left+Marr+Child", "Divorced")
#' biofam.seq <- seqdef(biofam, 10:25, labels=biofam.lab)
#'
#' # Computing the distance matrix
#' biofam.dhd <- seqdist(biofam.seq, method="DHD")
#'
#' # Representative sequence  plot (using defaults)
#' # ... with TraMineR::seqplot
#' seqrplot(biofam.seq, diss = biofam.dhd)
#'
#' # ... with ggseqrplot
#' ggseqrplot(biofam.seq, diss = biofam.dhd)
ggseqrplot <- function(seqdata,
                       diss,
                       group = NULL,
                       criterion = "density",
                       coverage = .25,
                       nrep = NULL,
                       pradius = 0.10,
                       dmax = NULL,
                       border = FALSE,
                       proportional = TRUE,
                       weighted = TRUE,
                       stats = TRUE,
                       colored.stats = NULL,
                       facet_ncol = NULL) {
  if (!inherits(seqdata, "stslist")) {
    stop("data are not stored as sequence object, use 'TraMineR::seqdef' to create one")
  }

  if (!is.null(nrep) && (nrep%%1!=0 | nrep == 0)) {
    stop("nrep has to be a positive whole number")
  }

  if (is.null(border)) border <- FALSE

  if (is.null(group)) group <- 1

  if (length(unique(group)) > 10 & is.null(facet_ncol)) {
    facet_ncol <- 5
  }

  if (length(unique(group)) <= 10 & is.null(facet_ncol)) {
    facet_ncol <- c(1,2,3,2,3,3,4,4,5,5)[length(unique(group))]
  }

  if ("haven_labelled" %in% class(group)) {
    group_name <- deparse(substitute(group))
    group <- haven::as_factor(group)
    cli::cli_warn(c("i" = "group vector {.arg {group_name}} is of class {.cls haven_labelled} and has been converted into a factor"))
  }


  seq.rep <- purrr::map(
    sort(unique(group)),
    ~ TraMineR::seqrep(seqdata[group == .x, ],
                       diss = diss[group == .x, group == .x],
                       criterion = criterion,
                       weighted = weighted,
                       coverage = coverage,
                       nrep = nrep,
                       pradius = pradius,
                       dmax = dmax
    ))

  nrow.aux <- purrr::map(seq.rep, nrow) |>
    unlist() |>
    max()

  if (is.null(colored.stats) & nrow.aux <=10) colored.stats <- TRUE
  if (is.null(colored.stats) & nrow.aux > 10) colored.stats <- FALSE

  if (proportional == TRUE) {
    for(i in 1:length(unique(group))) {
      wgt <- attributes(seq.rep[[i]])$Statistics[1:nrow(seq.rep[[i]]),2]
      attributes(seq.rep[[i]])$weights <- wgt
    }
  }

  aux <- purrr::map(seq.rep,
                    ~ ggseqiplot(.x)$data)

  ybreaks <- purrr::map(aux,
                        ~(unique(.x$begin) + unique(.x$end)) / 2)



  coverage <- purrr::map(seq.rep,
                         ~attributes(.x)$Statistics[nrow(.x) + 1, 4])

  coverage <- purrr::map(coverage, ~paste0(round(.x, 1), "%"))


  rplotdata <- purrr::map(seq.rep,
                          ~ attributes(.x)$Statistics |>
                            dplyr::transmute(
                              id = dplyr::row_number(),
                              `Discrepancy (mean dist. to center)` = .data$V,
                              `Mean dist. to representative seq.` = .data$MD
                            ) |>
                            dplyr::filter(.data$id != max(.data$id)) |>
                            tidyr::pivot_longer(cols = -"id"))


  rplotdata <- purrr::imap(sort(unique(group)),
                          ~rplotdata[[.y]] |>
                            dplyr::mutate(group = .x, .before = 1))


  if (colored.stats == TRUE & stats == TRUE) {
    p2 <- purrr::map(rplotdata,
                     ~ .x |>
                       ggplot(aes(
                         x = .data$value,
                         y = "1",
                         label = .data$id,
                         fill = factor(.data$id)
                       )) +
                       ggrepel::geom_label_repel(
                         size = 3, colour = "black",
                         alpha = .8, point.size = NA,
                         direction = "y"
                       ) +
                       scale_y_discrete(
                         labels = NULL,
                         expand = expansion(add = 1.5)
                       ) +
                       facet_wrap(vars(.data$name),
                                  nrow = 2,
                                  scales = "fixed"
                       ) +
                       theme_minimal() +
                       theme(
                         legend.position = "none",
                         strip.text = element_text(size = 10),
                         axis.title.y = element_blank(),
                         axis.title.x = element_blank(),
                         panel.grid.major.y = element_blank()
                       ))


    color <- purrr::map(p2,
                        ~ggplot_build(.x)$data[[1]] |>
                          dplyr::group_by(.data$label) |>
                          dplyr::summarise(col = dplyr::first(.data$fill)) |>
                          dplyr::pull(.data$col))

    labs <- purrr::map(color,
                       ~glue::glue("<b style='color:{.x}'>{1:length(.x)}</b>"))


    suppressMessages(
      p1 <- purrr::map2(seq.rep, 1:length(unique(group)),
                        ~ggseqplot::ggseqiplot(.x, border = border) +
                          scale_y_continuous(
                            breaks = ybreaks[[.y]],
                            labels = labs[[.y]],
                            guide = guide_axis(check.overlap = TRUE),
                            expand = expansion(add = 0)
                          ) +
                          labs(subtitle = glue::glue("Coverage = {coverage[[.y]]}")) +
                          theme(
                            axis.title.y = element_blank(),
                            axis.text.y = ggtext::element_markdown(size = 11),
                            plot.subtitle = element_text(hjust = 0.5)
                          ))
    ) } else {
      suppressMessages(
        p1 <- purrr::map2(seq.rep, 1:length(unique(group)),
                          ~ggseqiplot(.x, border = border) +
                            scale_y_continuous(
                              breaks = ybreaks[[.y]],
                              labels = 1:nrow(.x),
                              guide = guide_axis(check.overlap = TRUE),
                              expand = expansion(add = c(0, 0))
                            ) +
                            theme(axis.title.y = element_blank()))
      )


      if (length(unique(group)) > 1) {
        p1 <- purrr::map(1:length(unique(group)),
                         ~p1[[.x]] +
                           labs(subtitle = glue::glue("Coverage = {coverage[[.x]]}")) +
                           theme(plot.subtitle = element_text(hjust = 0.5)))
      } else {
        p1[[1]] <- p1[[1]] +
          labs(caption = glue::glue("Coverage = {coverage[[1]]}; Criterion = {criterion}"))
      }

      p2 <- purrr::map(rplotdata,
                       ~.x |>
                         ggplot(aes(
                           x = .data$value,
                           y = "1",
                           label = .data$id
                         )) +
                         ggrepel::geom_text_repel(
                           size = 4.5,
                           fontface = "bold",
                           point.size = NA,
                           direction = "y"
                         ) +
                         scale_y_discrete(
                           labels = NULL,
                           expand = expansion(add = 1.5)
                         ) +
                         facet_wrap(vars(.data$name), nrow = 2, scales = "fixed") +
                         theme_minimal() +
                         theme(
                           legend.position = "none",
                           strip.text = element_text(size = 12),
                           axis.title.y = element_blank(),
                           axis.title.x = element_blank(),
                           panel.grid.major.y = element_blank()
                         ))
    }

  if (length(unique(group)) > 1) {
    p2 <- purrr::map(1:length(unique(group)),
                     ~p2[[.x]] +
                       labs(title = sort(unique(group))[[.x]]) +
                       theme(plot.title = element_text(hjust = .5)))
  }


  patches <- vector(mode='character')

  for (i in 1:facet_ncol) {
    facx <- i - 1
    idx <- 1:facet_ncol+facet_ncol*facx

    if (min(idx) <= length(unique(group))) {
      idy <- idx[idx > length(unique(group))]
      idx <- idx[idx<=length(unique(group))]

      if (stats == TRUE) {
        patches <- paste0(patches, " + ", paste0("p2[[",idx,"]]", collapse = " + "))
        if (length(idy) > 0) {
          patches <- paste0(patches, " + ", paste0(rep("patchwork::plot_spacer()", length(idy)),
                                                   collapse = " + "))
        }
      }

      patches <- paste0(patches, " + ", paste0("p1[[",idx,"]]", collapse = " + "))
      if (length(idy) > 0) {
        patches <- paste0(patches, " + ", paste0( rep("patchwork::plot_spacer()", length(idy)),
                                                  collapse = " + "))
      }
    }
  }

  if (facet_ncol == 1 & length(unique(group)) > 1) {
    patches <- vector(mode='character')

    for (i in 1:length(unique(group))) {
      patches <- paste0(patches, " + ",
                        paste0("p2[[",i,"]] + p1[[",i,"]]", collapse = " + "))
    }

  }

  patches <- substring(patches, 4)

  prows <- ceiling(length(unique(group))/facet_ncol)

  heights <- as.character(rep(c(.75,1),prows))


  if (stats == TRUE) {
    patches <- glue::glue("{patches} + plot_layout(guides = 'collect', ncol = {facet_ncol}, heights = ")
    patches <- paste0(patches, "c(", paste0(heights, collapse = ", "), "))")
  } else {
    patches <- glue::glue("{patches} + plot_layout(guides = 'collect', ncol = {facet_ncol})")
  }

  patches <- glue::glue("{patches} + patchwork::plot_annotation(theme = theme(legend.position = 'bottom'))")



  if (stats == FALSE & length(unique(group)) > 1) {
    p1 <- purrr::map2(p1, sort(unique(group)),
                      ~ .x +
                        ggtitle(.y) +
                        theme(plot.title = element_text(hjust = 0.5,
                                                        size = 16)))
  }


  if (colored.stats == FALSE) {
    p1 <- purrr::map(p1,
                     ~.x +
                       theme(axis.text.y = element_text(
                         color = "black",
                         size = 12,
                         face = "bold"
                       )))
  }

    rplot <- eval(parse(text = patches))


  if (length(unique(group)) > 1) {
    rplot <- rplot +
      patchwork::plot_annotation(caption = paste("Criterion:", criterion))
  }

  if (length(unique(group)) > 6) {
      usethis::ui_info(glue::glue("You are trying to render a representative sequence plot for many groups.
      The resulting output (if produced at all) might be hard to decipher.
      Consider reducing the number of distinct groups."))
  }

  if (length(unique(group)) > 3) {
      usethis::ui_info(glue::glue("You are trying to render a representative sequence plot for many groups using just one column.
      The resulting output (if produced at all) might be hard to decipher.
      Consider reducing the number of distinct groups or increase facet_ncol."))
    }

  return(rplot)


}
