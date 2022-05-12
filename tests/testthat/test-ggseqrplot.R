# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Examples from TraMineR::seqplot

library(TraMineR)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Examples from TraMineR::seqplot

# actcal data set
data(biofam)
biofam.lab <- c(
  "Parent", "Left", "Married", "Left+Marr",
  "Child", "Left+Child", "Left+Marr+Child", "Divorced"
)
## Here, we use only 100 cases selected such that all elements
## of the alphabet be present.
## (More cases and a larger k would be necessary to get a meaningful example.)
biofam.seq <- seqdef(biofam[501:600, ], 10:25, labels = biofam.lab)
diss <- seqdist(biofam.seq, method = "LCS")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


test_that("arguments are specified correctly (length, type, ...)", {
  expect_error(ggseqrplot(biofam.seq, diss = diss, nrep = 5.5))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("check if output of ggseqrplot is ggplot", {
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss), "ggplot")
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss, nrep = 11), "ggplot")
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss,
                             colored.stats = "FALSE"), "ggplot")
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss, stats = FALSE), "ggplot")
  # expect_s3_class(ggseqrplot(biofam.seq, group = group, facet_nrow = 2), "ggplot")
  # expect_s3_class(ggseqrplot(biofam.seq, weighted = FALSE), "ggplot")
  # expect_s3_class(ggseqrplot(biofam.seq, sortv = "from.start"), "ggplot")
  # expect_s3_class(ggseqrplot(biofam.seq, group = group,
  #                            facet_scale = "fixed" ), "ggplot")
})
