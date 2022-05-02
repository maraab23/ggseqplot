shared_params <- function() {
  c(
    '@param seqdata State sequence object (class \\code{stslist}) created with the \\code{\\link[TraMineR:seqdef]{TraMineR::seqdef}} function.',
    '@param weighted Controls if weights (specified in \\code{\\link[TraMineR:seqdef]{TraMineR::seqdef}}) should be used. Default is \\code{TRUE}, i.e. if available weights are used',
    '@param group Grouping variable of length equal to the number of sequences. When not NULL, a distinct plot is generated for each level of group.'
  )
}

shared_facet <- function() {
  c(
    '@param facet_ncol Number of columns in faceted (i.e. grouped) plot',
    '@param facet_nrow Number of rows in faceted (i.e. grouped) plot'
  )
}
