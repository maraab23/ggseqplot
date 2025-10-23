# Example data come from TraMineR
library(TraMineR)

# actcal data set
data(actcal)

# We use only a sample of 300 cases
set.seed(1)
actcal <- actcal[sample(nrow(actcal), 300), ]
actcal.lab <- c("> 37 hours", "19-36 hours", "1-18 hours", "no work")
actcal.seq <- seqdef(actcal, 13:24, labels = actcal.lab)

group <- actcal$sex

group_labelled <- haven::labelled(as.integer(actcal$sex),
                                  labels = c("Male" = 6, "Female" = 7))

data(ex1)
ex1.seq <- seqdef(ex1, 1:13, weights = ex1$weights)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("Executions stops if input data are not of class stslist", {
  expect_error(ggseqfplot(actcal))
})

test_that("arguments are specified correctly (length, type, ...)", {
  expect_error(ggseqfplot(actcal.seq, group = group, facet_ncol = 5.5))
  expect_error(ggseqfplot(actcal.seq, group = group, facet_nrow = 5.5))
})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("Executions stops if group vector is not of same length as sequence data", {
  expect_error(
    ggseqfplot(actcal.seq, weighted = group)
  )
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("Executions stops if logical arguments take wrong values", {
  expect_error(
    ggseqfplot(actcal.seq, group = group[1:20]),
    "length of group vector must match number of rows of seqdata"
  )
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("haven_labelled group is converted to factor with warning", {
  expect_warning(ggseqfplot(actcal.seq,
                            group = group_labelled))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


test_that("check if output of ggseqfplot is ggplot", {
  expect_s3_class(ggseqfplot(actcal.seq, no.coverage = TRUE), "ggplot")
  expect_s3_class(ggseqfplot(actcal.seq,
                             group = group,
                             no.coverage = TRUE), "ggplot")
  expect_s3_class(ggseqfplot(actcal.seq, ylabs = "share"), "ggplot")
  expect_s3_class(ggseqfplot(actcal.seq, ylabs = "share",
                             proportional = FALSE,
                             group = group), "ggplot")
  expect_s3_class(ggseqfplot(actcal.seq,
                             proportional = FALSE,
                             group = group), "ggplot")
  expect_s3_class(ggseqfplot(actcal.seq, proportional = FALSE), "ggplot")
  expect_s3_class(ggseqfplot(actcal.seq, border = TRUE), "ggplot")
  expect_s3_class(ggseqfplot(actcal.seq, border = NULL), "ggplot")
  expect_s3_class(ggseqfplot(actcal.seq, ylabs = "total",
                             proportional = FALSE), "ggplot")
})
