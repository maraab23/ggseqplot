# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Examples from TraMineR::seqplot

library(TraMineR)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Examples from TraMineR::seqplot

# actcal data set
data(actcal)

# We use only a sample of 300 cases
set.seed(1)
actcal <- actcal[sample(nrow(actcal),300),]
actcal.lab <- c("> 37 hours", "19-36 hours", "1-18 hours", "no work")
actcal.seq <- seqdef(actcal,13:24,labels=actcal.lab)

group <- actcal$sex

# ex1 using weights
data(ex1)
ex1.seq <- seqdef(ex1, 1:13, weights=ex1$weights)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("Number of unique ids in plot data equals rows in seqdata", {
  expect_length(unique(ggseqiplot(actcal.seq)$data$idnew),nrow(actcal.seq))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


test_that("check if output of ggseqiplot is ggplot", {
  expect_s3_class(ggseqiplot(actcal.seq), "ggplot")
  expect_s3_class(ggseqiplot(actcal.seq, border = TRUE), "ggplot")
  expect_s3_class(ggseqiplot(actcal.seq, group = group, facet_nrow = 2), "ggplot")
  expect_s3_class(ggseqiplot(actcal.seq, weighted = FALSE), "ggplot")
  expect_s3_class(ggseqiplot(actcal.seq, sortv = "from.start"), "ggplot")
})
