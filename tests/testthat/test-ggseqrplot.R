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
diss <- seqdist(biofam.seq, method = "LCS")
set.seed(123)
group <- sample(1:5, nrow(biofam.seq), replace = TRUE)
group2 <- sample(1:11, nrow(biofam.seq), replace = TRUE)

group_labelled <- haven::labelled(as.integer(biofam$sex[501:600]),
                                  labels = c("Male" = 1, "Female" = 2))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


test_that("arguments are specified correctly (length, type, ...)", {
  expect_error(ggseqrplot(biofam.seq, diss = diss, nrep = 5.5))
  expect_error(ggseqrplot(biofam, diss = diss))

})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("haven_labelled group is converted to factor with warning", {
  expect_warning(ggseqrplot(biofam.seq, diss = diss,
                            group = group_labelled))
})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("check if output of ggseqrplot is ggplot", {
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss), "ggplot")
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss, border = NULL), "ggplot")
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss, stats = FALSE,
                             group = group2, facet_ncol = NULL), "ggplot")
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss, stats = FALSE,
                             group = group, facet_ncol = 1), "ggplot")
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss,
                             facet_ncol = 3, stats = TRUE), "ggplot")
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss, nrep = 11), "ggplot")
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss,
                             colored.stats = "FALSE"), "ggplot")
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss, stats = FALSE), "ggplot")
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss, stats = FALSE,
                             group = group), "ggplot")
  expect_s3_class(ggseqrplot(biofam.seq, diss = diss, stats = FALSE,
                             group = group, colored.stats = FALSE), "ggplot")



  # expect_s3_class(ggseqrplot(biofam.seq, group = group, facet_nrow = 2), "ggplot")
  # expect_s3_class(ggseqrplot(biofam.seq, weighted = FALSE), "ggplot")
  # expect_s3_class(ggseqrplot(biofam.seq, sortv = "from.start"), "ggplot")
  # expect_s3_class(ggseqrplot(biofam.seq, group = group,
  #                            facet_scale = "fixed" ), "ggplot")
})
