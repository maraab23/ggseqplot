# biofam data set
data(biofam)
# We use only a sample of 300 cases
set.seed(10)
biofam <- biofam[sample(nrow(biofam), 300), ]
biofam.lab <- c(
  "Parent", "Left", "Married", "Left+Marr",
  "Child", "Left+Child", "Left+Marr+Child", "Divorced"
)
biofam.seq <- seqdef(biofam, 10:25, labels = biofam.lab, weights = biofam$wp00tbgs)
biofam2.seq <- seqdef(biofam, 10:25, labels = biofam.lab)
group <- biofam$sex

data(ex1)
ex1.seq <- seqdef(ex1, 1:13, weights = ex1$weights)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("arguments are specified correctly (length, type, ...)", {
  expect_error(ggseqeplot(biofam))
  expect_error(ggseqeplot(biofam.seq, group = biofam$birthyr))
  expect_error(ggseqeplot(biofam.seq, group = biofam$sex, linecolor = "green"))
  expect_error(ggseqeplot(biofam.seq, group = group[1:100]))
  expect_error(ggseqeplot(biofam.seq, weighted = group))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("check if output of ggseqtrplot is ggplot", {
  expect_s3_class(ggseqeplot(biofam.seq), "ggplot")
  expect_s3_class(ggseqeplot(biofam2.seq), "ggplot")
  expect_s3_class(ggseqeplot(ex1.seq), "ggplot")
  expect_s3_class(ggseqeplot(biofam.seq, group = biofam$sex,
                             gr.linetype = TRUE), "ggplot")
})
