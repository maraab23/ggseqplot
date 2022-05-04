# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Examples from TraMineR::seqplot

library(TraMineR)
library(ggplot2)

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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("Number of rows in plot data equals states*positions*groups", {
  expect_equal(length(TraMineR::alphabet(actcal.seq)) * dim(actcal.seq)[2],
               nrow(ggseqdplot(actcal.seq)$data))

  expect_equal(length(TraMineR::alphabet(actcal.seq)) * dim(actcal.seq)[2] *
                 length(unique(group)),
               nrow(ggseqdplot(actcal.seq, group = group)$data))
})

