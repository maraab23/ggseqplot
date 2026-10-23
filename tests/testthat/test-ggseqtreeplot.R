# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Example data come from TraMineR
library(TraMineR)
# mvad data set
data(mvad)

# We use only a sample of 300 cases
set.seed(1)
mvad <- mvad[sample(nrow(mvad), 300), ]
mvad.seq <- seqdef(mvad[, 17:86], xtstep = 6)
mvad.diss <- seqdist(mvad.seq, method = "HAM")
mvad.tree <- seqtree(mvad.seq ~ male + Grammar + funemp + gcse5eq,
                     data = mvad, diss = mvad.diss, R = 100, max.depth = 3)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("Executions stops if input is not a tree", {
  expect_error(ggseqtreeplot(mvad.seq))
})

test_that("Executions stops if seqdata does not match tree", {
  expect_error(
    ggseqtreeplot(mvad.tree, seqdata = mvad.seq[1:20, ]),
    "number of sequences in seqdata must match the number of cases used to grow the tree"
  )
  expect_error(ggseqtreeplot(mvad.tree, seqdata = mvad))
})

test_that("arguments are specified correctly (length, type, ...)", {
  expect_error(ggseqtreeplot(mvad.tree, type = "rf"))
  expect_error(ggseqtreeplot(mvad.tree, only.leaf = "yes"))
  expect_error(ggseqtreeplot(mvad.tree, with.legend = 1))
  expect_error(ggseqtreeplot(mvad.tree, title.size = "large"))
  expect_error(ggseqtreeplot(mvad.tree, type = "mt", common.y = "yes"))
  expect_warning(ggseqtreeplot(mvad.tree, type = "d", common.y = TRUE))
  expect_error(ggseqtreeplot(mvad.tree, quality.multiline = "yes"))
})

test_that("global quality is rendered as tag", {
  expect_match(ggseqtreeplot(mvad.tree)$labels$tag, "^Global quality: Pseudo F")
  expect_match(ggseqtreeplot(mvad.tree, quality.multiline = TRUE)$labels$tag,
               "^Global quality\nPseudo F")
  expect_null(ggseqtreeplot(mvad.tree, with.quality = FALSE)$labels$tag)
})

test_that("node.layers are added to the node plots", {
  expect_no_error(ggplotGrob(ggseqtreeplot(
    mvad.tree,
    node.layers = theme(plot.caption = element_text(size = 6))
  )))
  expect_no_error(ggplotGrob(ggseqtreeplot(
    mvad.tree,
    node.layers = list(scale_x_discrete(breaks = c(1, 25, 49)),
                       theme(axis.text.y = element_blank()))
  )))
})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("check if output is ggplot", {
  expect_s3_class(ggseqtreeplot(mvad.tree), "ggplot")
  expect_s3_class(ggseqtreeplot(mvad.tree, type = "i", only.leaf = TRUE), "ggplot")
  expect_s3_class(ggseqtreeplot(mvad.tree, type = "f"), "ggplot")
  expect_s3_class(ggseqtreeplot(mvad.tree, type = "ms"), "ggplot")
  expect_s3_class(ggseqtreeplot(mvad.tree, type = "mt"), "ggplot")
  expect_s3_class(ggseqtreeplot(mvad.tree, with.legend = FALSE,
                                with.quality = FALSE), "ggplot")
  expect_s3_class(ggseqtreeplot(mvad.tree, type = "i",
                                sortv = seq_len(nrow(mvad.seq))), "ggplot")
})

test_that("node data match the tree", {
  nodedata <- ggseqtreeplot(mvad.tree)$data
  expect_equal(nodedata$n[1], nrow(mvad.seq))
  expect_equal(nrow(nodedata), length(unique(mvad.tree$fitted[, 1])) * 2 - 1)
  expect_equal(sum(nodedata$n[nodedata$leaf]), nrow(mvad.seq))
})

test_that("common.y gives all node plots the same y range", {
  ymax <- function(plot) {
    layer_data(plot, 1)$ymax |> max()
  }
  nodeplots <- purrr::map(
    ggseqtreeplot(mvad.tree)$data$ind,
    ~ ggseqmtplot(mvad.seq[.x, ])
  )
  expect_gt(length(unique(purrr::map_dbl(nodeplots, ymax))), 1)
  expect_equal(nodeplotymax(nodeplots[[1]]), ymax(nodeplots[[1]]))
  expect_no_error(ggplotGrob(ggseqtreeplot(mvad.tree, type = "mt", common.y = TRUE)))
  expect_no_error(ggplotGrob(ggseqtreeplot(mvad.tree, type = "mt", common.y = TRUE,
                                           error.bar = "SE")))
})

test_that("plot renders without error", {
  expect_no_error(ggplotGrob(ggseqtreeplot(mvad.tree)))
  expect_no_error(ggplotGrob(ggseqtreeplot(mvad.tree, only.leaf = TRUE)))
})
