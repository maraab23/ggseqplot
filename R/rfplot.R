# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Revised function ot TraMineRExtra's seqplot.rf
# Code extracts information necessary to create the two plots manually
# Source: TraMineRextras_0.6.3
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rfplot.obj <- function(seqdata, k = floor(nrow(seqdata) / 10), diss, sortv = NULL,
                       ylab = NA, yaxis = FALSE, main = NULL, which.plot = "both", ...) {
  return(seqplot.rf_internal(seqdata,
    k = k, diss = diss, sortv = sortv,
    ylab = ylab, yaxis = yaxis, main = main, which.plot = which.plot,
    ...
  ))
}
seqplot.rf_internal <- function(seqdata, k = floor(nrow(seqdata) / 10), diss, sortv = NULL,
                                use.hclust = FALSE, hclust_method = "ward.D", use.quantile = FALSE,
                                ylab = NA, yaxis = FALSE, main = NULL, which.plot = "both", ...) {
  message(" [>] Using k=", k, " frequency groups")


  # Extract medoid, possibly weighted
  gmedoid.index <- TraMineR::disscenter(diss, medoids.index = "first")

  gmedoid.dist <- diss[, gmedoid.index] # Extract distance to general medoid

  ## Vector where distance to k medoid will be stored
  kmedoid.dist <- rep(0, nrow(seqdata))
  # index of the k-medoid for each sequence
  kmedoid.index <- rep(0, nrow(seqdata))
  # calculate qij - distance to frequency group specific medoid within frequency group
  if (is.null(sortv) && !use.hclust) {
    sortv <- stats::cmdscale(diss, k = 1)
  }

  ng <- nrow(seqdata) %/% k
  r <- nrow(seqdata) %% k
  n.per.group <- rep(ng, k)
  if (r > 0) {
    n.per.group[order(stats::runif(r))] <- ng + 1
  }
  mdsk <- rep(1:k, n.per.group)
  mdsk <- mdsk[rank(sortv, ties.method = "random")]

  # following block not necessary as use.hclust=FALSE -> sot
  # else{
  #   hh <- stats::hclust(stats::as.dist(diss), method=hclust_method)
  #   mdsk <- factor(stats::cutree(hh, k))
  #   medoids <- TraMineR::disscenter(diss, group=mdsk, medoids.index="first")
  #   medoids <- medoids[levels(mdsk)]
  #   #ww <- xtabs(~mdsk)
  #   mds <- stats::cmdscale(diss[medoids, medoids], k=1)
  #   mdsk <- as.integer(factor(mdsk, levels=levels(mdsk)[order(mds)]))
  # }
  kun <- length(unique(mdsk))
  if (kun != k) {
    warning(" [>] k value was adjusted to ", kun)
    k <- kun
    mdsk <- as.integer(factor(mdsk, levels = sort(unique(mdsk))))
  }
  # sortmds.seqdata$mdsk<-c(rep(1:m, each=r+1),rep({m+1}:k, each=r))
  ## pmdse <- 1:k
  # pmdse20<-1:20

  ## for each k
  for (i in 1:k) {
    ## Which individuals are in the k group
    ind <- which(mdsk == i)
    if (length(ind) == 1) {
      kmedoid.dist[ind] <- 0
      ## Index of the medoid sequence for each seq
      kmedoid.index[ind] <- ind
    } else {
      dd <- diss[ind, ind]
      ## Indentify medoid
      kmed <- TraMineR::disscenter(dd, medoids.index = "first")
      ## Distance to medoid for each seq
      kmedoid.dist[ind] <- dd[, kmed]
      ## Index of the medoid sequence for each seq
      kmedoid.index[ind] <- ind[kmed]
    }
    ## Distance matrix for this group
  }

  ## Assign the medoid sequence to each sequence
  seqtoplot <- seqdata[kmedoid.index, ]

  ## Correct weights to their original weights (otherwise we use the medoid weights)
  attr(seqtoplot, "weights") <- NULL

  # if (which.plot=="both"){
  #   opar <- par(mfrow=c(1,2), oma=c(3,0,(!is.null(main))*3,0), mar=c(1, 1, 2, 0))
  #   on.exit(par(opar))
  #   seqIplot(seqtoplot, with.legend=FALSE, sortv=mdsk, yaxis=yaxis, main="Sequences medoids", ...)
  # }
  #
  # if (which.plot=="medoids")
  #   seqIplot(seqtoplot, sortv=mdsk, yaxis=yaxis, ylab=ylab, main=paste(main,"Sequences medoids", sep=": "), ...)
  ## seqIplot(seqtoplot, with.legend=FALSE, sortv=mdsk)

  heights <- stats::xtabs(~mdsk) / nrow(seqdata)
  at <- (cumsum(heights) - heights / 2) / sum(heights) * length(heights)
  # if(!yaxis){
  #   par(yaxt="n")
  # }
  # if (which.plot == "both") {
  #   boxplot(kmedoid.dist~mdsk, horizontal=TRUE, width=heights, frame=FALSE,
  #           main="Dissimilarities to medoid",
  #           ylim=range(as.vector(diss)), at=at, ylab=ylab)
  # }
  # if (which.plot == "diss.med") {
  #   boxplot(kmedoid.dist~mdsk, horizontal=TRUE, width=heights, frame=FALSE,
  #           main=paste(main,"Dissimilarities to medoid",sep=": "),
  #           ylim=range(as.vector(diss)), at=at, ylab=ylab)
  # }

  # calculate R2
  R2 <- 1 - sum(kmedoid.dist^2) / sum(gmedoid.dist^2)
  # om K=66 0.5823693


  # calculate F
  ESD <- R2 / (k - 1) # averaged explained variance
  USD <- (1 - R2) / (nrow(seqdata) - k) # averaged explained variance
  Fstat <- ESD / USD

  message(" [>] Pseudo/median-based-R2: ", format(R2))
  message(" [>] Pseudo/median-based-F statistic: ", format(Fstat))
  ## cat(sprintf("Representation quality: R2=%0.2f F=%0.2f", R2, Fstat))
  # if (which.plot=="both") {
  #   title(main=main, outer=TRUE)
  #   title(sub=sprintf("Representation quality: R2=%0.2f and F=%0.2f", R2, Fstat), outer=TRUE, line=2)
  # }

  newlist <- list(
    seqtoplot = seqtoplot,
    mdsk = mdsk,
    kmedoid.dist = kmedoid.dist,
    heights = heights,
    diss = diss,
    at = at,
    R2 = R2,
    Fstat = Fstat,
    kkk = kmedoid.index
  )

  # return(kmedoid.index)
}
