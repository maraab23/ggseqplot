# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Example data come from TraMineR
library(TraMineR)
# biofam data set
data(biofam)
biofam.lab <- c("Parent", "Left", "Married", "Left+Marr",
                "Child", "Left+Child", "Left+Marr+Child", "Divorced")

# We use only the first 20 cases
biofam.seq <- seqdef(biofam[, 10:25], labels = biofam.lab,
                     weights = biofam$wp00tbgs)[1:20, ]
biofam.eseq <- seqecreate(biofam.seq, tevent = "state")

group <- biofam$sex[1:20]

group_labelled <- haven::labelled(
  as.integer(group),
  labels = c("male" = 1, "female" = 2)
)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("Executions stops if input data are not a sequence object", {
  expect_error(ggseqpcplot(biofam))
})

test_that("arguments are specified correctly (length, type, ...)", {
  expect_error(
    ggseqpcplot(biofam.seq, group = group[1:5]),
    "length of group vector must match number of rows of seqdata"
  )
  expect_error(ggseqpcplot(biofam.eseq, group = group[1:5]))
  expect_error(ggseqpcplot(biofam.seq, linewidth = -1))
  expect_error(ggseqpcplot(biofam.seq, no.n = "yes"))
  expect_error(ggseqpcplot(biofam.seq, group = group, facet_ncol = 1.5))
  expect_error(ggseqpcplot(biofam.seq, group = group, facet_nrow = 1.5))
})

test_that("haven_labelled group is converted to factor with warning", {
  expect_warning(ggseqpcplot(biofam.seq, group = group_labelled))
})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("check if output is ggplot", {
  expect_s3_class(ggseqpcplot(biofam.seq, order.align = "time"), "ggplot")
  expect_s3_class(ggseqpcplot(biofam.seq, order.align = "first"), "ggplot")
  expect_s3_class(ggseqpcplot(biofam.seq, group = group), "ggplot")
  expect_s3_class(ggseqpcplot(biofam.eseq, order.align = "first",
                              alphabet = biofam.lab), "ggplot")
  expect_s3_class(ggseqpcplot(biofam.eseq, order.align = "first",
                              alphabet = biofam.lab,
                              ltype = "non-embeddable"), "ggplot")
})

test_that("plot data match TraMineR::seqpcplot", {
  pc <- seqpcplot(biofam.seq, order.align = "time", plot = FALSE, seed = 1)
  pcdata <- ggseqpcplot(biofam.seq, order.align = "time", seed = 1)$data
  expect_equal(nrow(pcdata), sum(pc$pts$n.1 > 0))
  expect_equal(sort(unique(pcdata$color)), sort(unique(as.vector(pc$pts$col.1))))
  expect_equal(levels(pcdata$group), pc$main)
})

test_that("no.n removes the number of sequences from the facet labels", {
  expect_match(levels(ggseqpcplot(biofam.seq)$data$group), "n = ")
  expect_equal(levels(ggseqpcplot(biofam.seq, group = group, no.n = TRUE)$data$group),
               levels(factor(group)))
  expect_s3_class(ggseqpcplot(biofam.seq, no.n = TRUE)$theme$strip.text.x,
                  "element_blank")
  expect_no_error(ggplotGrob(ggseqpcplot(biofam.seq, no.n = TRUE)))
})

test_that("plot renders without error", {
  expect_no_error(ggplotGrob(ggseqpcplot(biofam.seq, order.align = "time")))
  expect_no_error(ggplotGrob(ggseqpcplot(biofam.seq, group = group, facet_ncol = 1)))
})
