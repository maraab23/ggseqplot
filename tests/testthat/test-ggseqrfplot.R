# Example data come from TraMineR
library(TraMineR)

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

biofam2.seq <- seqdef(biofam[501:600, 10:25], labels=biofam.lab,
                      weights=biofam[501:600,"wp00tbgs"])

diss <- seqdist(biofam.seq, method = "LCS")

parentTime <- seqistatd(biofam.seq)[, 1]
b.srf <- seqrf(biofam2.seq, diss=diss, k=12, sortv=parentTime)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# test_that("Number of groups in (boxplot) plot data equals k", {
#   expect_length(unique(ggseqrfplot(biofam.seq, diss=diss)$data$kgr),
#                 floor(nrow(biofam.seq)/10))
# })

test_that("Errors & messages work as expected", {
  expect_error(ggseqrfplot(biofam, diss = diss, k = 12))
  expect_error(ggseqrfplot(biofam.seq, k = 12))
  expect_error(ggseqrfplot(diss = diss, k = 12))
  expect_error(ggseqrfplot(biofam, diss = diss, k = 12))
  expect_message(ggseqrfplot(biofam.seq, diss = diss, k = 12,
                             seqrfobject = b.srf))
  expect_message(ggseqrfplot(biofam.seq, diss = diss, k = 12,
                             seqrfobject = biofam))
  expect_message(ggseqrfplot(biofam, diss = diss, k = 12, seqrfobject = b.srf))
  expect_error(ggseqrfplot(b.srf, diss = diss, k = 12))
  expect_error(ggseqrfplot(biofam, diss = diss, k = 12))
})


test_that("arguments are specified correctly (length, type, ...)", {
  expect_error(ggseqrfplot(biofam.seq, diss = diss, yaxis = "none"))
  expect_error(ggseqrfplot(biofam.seq, diss = diss, which.plot = "medods"))
})



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


test_that("check if output of ggseqrfplot is ggplot", {
  expect_s3_class(ggseqrfplot(biofam.seq, diss = diss, k = 12), "ggplot")
  expect_s3_class(ggseqrfplot(biofam.seq, diss = diss, k = 12,
                              weighted = FALSE, grp.meth = "first"), "ggplot")
  expect_s3_class(ggseqrfplot(seqrfobject = b.srf), "ggplot")
  expect_s3_class(ggseqrfplot(seqrfobject = b.srf,
                              outlier.jitter.height = .8), "ggplot")
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
