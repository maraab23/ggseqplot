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


# test_that("Number of groups in (boxplot) plot data equals k", {
#   expect_length(unique(ggseqrfplot(biofam.seq, diss=diss)$data$kgr),
#                 floor(nrow(biofam.seq)/10))
# })

test_that("Executions stops if input data are not of class stslist", {
  expect_error(ggseqrfplot(biofam, diss = diss, k = 12))
})


test_that("arguments are specified correctly (length, type, ...)", {
  expect_error(ggseqrfplot(biofam.seq, diss = diss, yaxis = "none"))
  expect_error(ggseqrfplot(biofam.seq, diss = diss, which.plot = "medods"))
})



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


test_that("check if output of ggseqrfplot is ggplot", {
  expect_s3_class(ggseqrfplot(biofam.seq, diss = diss, k = 12), "ggplot")
  expect_s3_class(ggseqrfplot(biofam.seq, diss = diss, k = 11), "ggplot")
  expect_s3_class(ggseqrfplot(biofam.seq,
    diss = diss,
    which.plot = "medoids"
  ), "ggplot")
  expect_s3_class(ggseqrfplot(biofam.seq,
    diss = diss,
    which.plot = "diss.to.med"
  ), "ggplot")
  expect_s3_class(ggseqrfplot(biofam.seq, diss = diss, yaxis = FALSE), "ggplot")
  expect_s3_class(ggseqrfplot(biofam.seq, diss = diss, border = NULL), "ggplot")
  # expect_s3_class(ggseqrfplot(biofam.seq, group = group, facet_nrow = 2), "ggplot")
  # expect_s3_class(ggseqrfplot(biofam.seq, weighted = FALSE), "ggplot")
  # expect_s3_class(ggseqrfplot(biofam.seq, sortv = "from.start"), "ggplot")
  # expect_s3_class(ggseqrfplot(biofam.seq, group = group,
  #                            facet_scale = "fixed" ), "ggplot")
})
