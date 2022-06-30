# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Examples from TraMineR::seqplot

library(TraMineR)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Examples from TraMineR::seqplot

# actcal data set
data(actcal)

# We use only a sample of 300 cases
set.seed(1)
actcal <- actcal[sample(nrow(actcal), 300), ]
actcal.lab <- c("> 37 hours", "19-36 hours", "1-18 hours", "no work")
actcal.seq <- seqdef(actcal, 13:24, labels = actcal.lab)

group <- actcal$sex

data(ex1)
ex1.seq <- seqdef(ex1, 1:13, weights = ex1$weights)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# test_that("Number of rows in plot data equals states*positions*groups", {
#   expect_equal(length(TraMineR::alphabet(actcal.seq)) * dim(actcal.seq)[2],
#                nrow(ggseqmtplot(actcal.seq)$data))
#
#   expect_equal(length(TraMineR::alphabet(actcal.seq)) * dim(actcal.seq)[2] *
#                  length(unique(group)),
#                nrow(ggseqmtplot(actcal.seq, group = group)$data))
# })

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("Executions stops if input data are not of class stslist", {
  expect_error(ggseqmtplot(actcal))
})

test_that("arguments are specified correctly (length, type, ...)", {
  expect_error(ggseqmtplot(actcal.seq, group = group, facet_ncol = 5.5))
  expect_error(ggseqmtplot(actcal.seq, group = group, facet_nrow = 5.5))
})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("Executions stops if group vector is not of same length as sequence data", {
  expect_error(
    ggseqmtplot(actcal.seq, weighted = group),
    "the arguments `no.n`, `weighted`, `with.missing`, and `border` have to be objects of type logical"
  )
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("Executions stops if logical arguments take wrong values", {
  expect_error(
    ggseqmtplot(actcal.seq, group = group[1:20]),
    "length of group vector must match number of rows of seqdata"
  )
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("check if output of ggseqmtplot is ggplot", {
  expect_s3_class(ggseqmtplot(actcal.seq), "ggplot")
  expect_s3_class(ggseqmtplot(actcal.seq, error.bar = "SE"), "ggplot")
  expect_s3_class(ggseqmtplot(actcal.seq, error.bar = "SD"), "ggplot")
  expect_s3_class(ggseqmtplot(actcal.seq, no.n= TRUE), "ggplot")
  expect_s3_class(ggseqmtplot(ex1.seq, with.missing = TRUE), "ggplot")
  expect_s3_class(ggseqmtplot(ex1.seq, group = c(1, 1, 1, 2, 2, 2, 2)), "ggplot")
  expect_s3_class(ggseqmtplot(actcal.seq, border = TRUE), "ggplot")
  expect_s3_class(ggseqmtplot(actcal.seq, border = NULL), "ggplot")
  expect_s3_class(ggseqmtplot(actcal.seq, group = group), "ggplot")
  expect_s3_class(ggseqmtplot(ex1.seq, weighted = FALSE), "ggplot")
  expect_s3_class(ggseqmtplot(actcal.seq, with.missing = TRUE), "ggplot")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# test_that("Colors from seqdata extracted correctly", {
#   p <- ggplot2::ggplot_build(ggseqmtplot(actcal.seq))
#
#   expect_equal(
#     attributes(actcal.seq)$cpal,
#     dplyr::pull(unique(p$data[[1]]["fill"]))
#   )
# })

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
