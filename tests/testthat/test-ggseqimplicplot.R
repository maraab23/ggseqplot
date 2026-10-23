# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Example data come from TraMineR
library(TraMineR)
# biofam data set
data(biofam)

# We use only a sample of 300 cases
set.seed(10)
biofam <- biofam[sample(nrow(biofam), 300), ]
biofam.lab <- c("Parent", "Left", "Married", "Left+Marr",
                "Child", "Left+Child", "Left+Marr+Child", "Divorced")
biofam.seq <- seqdef(biofam, 10:25, labels = biofam.lab)

implic <- TraMineRextras::seqimplic(biofam.seq, group = biofam$plingu02)

group_labelled <- haven::labelled(
  as.integer(biofam$sex),
  labels = c("male" = 1, "female" = 2)
)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("Executions stops if input data are not of class stslist or seqimplic", {
  expect_error(ggseqimplicplot(biofam, group = biofam$sex))
  expect_error(ggseqimplicplot(seqimplicobject = biofam.seq))
})

test_that("Executions stops if group vector is missing or of wrong length", {
  expect_error(ggseqimplicplot(biofam.seq), "a group vector has to be specified")
  expect_error(
    ggseqimplicplot(biofam.seq, group = biofam$sex[1:20]),
    "length of group vector must match number of rows of seqdata"
  )
})

test_that("arguments are specified correctly (length, type, ...)", {
  expect_error(ggseqimplicplot(biofam.seq, group = biofam$sex, weighted = "yes"))
  expect_error(ggseqimplicplot(seqimplicobject = implic, conf.level = 95))
  expect_error(ggseqimplicplot(seqimplicobject = implic, facet_ncol = 1.5))
  expect_error(ggseqimplicplot(seqimplicobject = implic, facet_nrow = 1.5))
})

test_that("haven_labelled group is converted to factor with warning", {
  expect_warning(ggseqimplicplot(biofam.seq, group = group_labelled))
})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("check if output is ggplot", {
  expect_s3_class(ggseqimplicplot(seqimplicobject = implic), "ggplot")
  expect_s3_class(ggseqimplicplot(biofam.seq, group = biofam$plingu02), "ggplot")
  expect_s3_class(ggseqimplicplot(seqimplicobject = implic, conf.level = NULL), "ggplot")
  expect_s3_class(ggseqimplicplot(seqimplicobject = implic,
                                  conf.level = c(0.95, 0.99)), "ggplot")
})

test_that("plot data match the seqimplic object", {
  implicdata <- ggseqimplicplot(seqimplicobject = implic)$data
  expect_equal(nrow(implicdata), length(implic$indices))
  expect_equal(max(implicdata$value, na.rm = TRUE), max(-implic$indices, na.rm = TRUE))
  expect_true(all(implicdata$value >= 0, na.rm = TRUE))
})

test_that("plot renders without error", {
  expect_no_error(ggplotGrob(ggseqimplicplot(seqimplicobject = implic)))
  expect_no_error(ggplotGrob(ggseqimplicplot(seqimplicobject = implic, facet_ncol = 1)))
})
