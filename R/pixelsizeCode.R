#'
#'     Code for studying effect of pixel size
#'
#'     Copyright (c) Adrian Baddeley, 2022-2024
#' 

## compute vector of pixel sizes appropriate to box of given width
chooseEps <- function(width, epsmin=0.1, epsmax=Inf, dimmin=8) {
  dimmax <- ceiling(width/epsmin)
  dimmin <- max(dimmin, floor(width/epsmax))
  stopifnot(dimmin <= dimmax)
  eps <- width/(dimmin:dimmax)
  return(eps)
}

## code to generate a plot file in standard folder
plotit <- function(fname, expr, w=6, h=6) {
  if(!dir.exists("pix-auto")) dir.create("pix-auto")
  plotfile <- paste0("pix-auto/", fname, ".pdf")
  pdf(plotfile, width=w, height=w)
  eval(expr)
  dev.off()
}

## code to plot the Murchison data (entire, or subregion)
showdata <- function(MUR=mur30, feature="greenstone", show.faults=FALSE,
                     ..., 
                     shadecol="lightgreen", linecol="black", pointcol="red",
                     lwd=2, pch=3, cex=1.5) {
  featcha <- MUR[[feature]]
  with(MUR, {
    plot(Frame(gold), type="n", main="")
    plot(featcha, border=NA, col=shadecol, add=TRUE)
    if(show.faults)
      plot(faults, lwd=lwd, col=linecol, add=TRUE)
    plot(gold, pch=pch, cols=pointcol, cex=cex, add=TRUE)
    plot(Frame(gold), add=TRUE)
  })
}

##################################################
## Code to run main experiment, with given settings
###################################################

doit <- function(tag, epsmin=0.1, epsmax=4, dimmin=100, ...,
                 MUR=mur30, feature="greenstone", featuretext=feature,
                 ylabels=c("symbols", "text")) {
  if(!dir.exists("data-auto")) dir.create("data-auto")
  datafile <- paste0("data-auto/", tag, ".rda")
  reload.or.compute(datafile, {
    eps <- chooseEps(shortside(Frame(MUR$gold)),
                     epsmin=epsmin, epsmax=epsmax, dimmin=dimmin)
    neps <- length(eps)
    chat <- numeric(neps)
    evid <- NULL
    pstate <- list()
    cat(paste("Evaluating contrast for", neps, "values of pixel width, from",
              min(eps), "to", max(eps), "... "))
    Gold <- MUR$gold
    Feature <- MUR[feature]
    for(i in seq_along(eps)) {
      ## do WofE calculation for pixel width eps[i];
      ## return only the results, not the data
      w.i <- wofe(Gold, Feature,
                  pixelsize=eps[i],
                  ...,
                  secretexit=TRUE)
      ## extract estimate of contrast C
      chat[i] <- w.i$param["C", feature]
      ## extract the evidence table and flatten it
      evid.i <- unlist(w.i$et)
      if(is.null(evid)) {
        evid <- matrix(, neps, length(evid.i))
        colnames(evid) <- names(evid.i)
      }
      evid[i,] <- evid.i
      ## continue
      pstate <- progressreport(i, neps, state=pstate,
                               formula=(time ~ I(i^2.75)), savehistory=TRUE)
    }
    ## After this calculation,
    ##   'chat' is the vector of WofE contrasts for each pixel size. 
    ##   'evid' is a matrix with one row for each pixel size and 12 columns
    ##          of which the last 4 columns contain the pixel counts
    ##          evid[, "weight1"] = m(B \cap D)
    ##          evid[, "weight2"] = m(\not B \cap D),
    ##          evid[, "weight3"] = m(B \cap \not D)
    ##          evid[, "weight4"] = m(\not B \cap \not D)

    ## 
    nDB <- evid[, "weight1"]
    nD  <- evid[, "weight1"] + evid[, "weight2"]
    nB  <- evid[, "weight1"] + evid[, "weight3"]
    npixels <- evid[, "weight1"] + evid[,"weight2"] + evid[, "weight3"] + evid[,"weight4"]

    ## Also compute 'exact/true' values (limits for infinitesimal pixels)
    w0 <- wofe(Gold, Feature, ..., pixelsize=0)
    e0 <- w0$et
    weight <- e0[["weight"]]
    inB <- e0[[feature]] == "TRUE"
    inD <- e0[["X"]] == "TRUE"
    exact <- list(
      ## WofE contrast
      C = w0$param["C", feature],
      ## number of deposits in feature
      nDB = weight[inD & inB],
      ## total number of deposits 
      nD = sum(weight[inD]),
      ## fraction of area occupied by feature
      fB = weight[inB & !inD]/sum(weight[!inD])
    )
  })

  ## fix omission in earlier computations
  if(is.null(exact$nDB)) exact$nDB <- npoints(Gold[MUR[[feature]]])

  ## determine type of y axis label
  ylabels <- match.arg(ylabels)

  ## plot WofE contrast against pixel size
  plotit(tag, {
    plot(eps, chat, log="x", xlab="pixel width (km)", ylab="contrast")
    abline(h=exact$C, col="green", lwd=2)
  })

  ## plot number of deposit pixels against pixel size
  plotit(paste0(tag, "nD"), {
    plot(eps, nD, log="x",
         type="l",
         xlab="pixel width (km)",
         ylab=switch(ylabels,
                     text="number of deposit pixels",
                     symbols=expression(n[D])))
    abline(h=exact$nD, col="green", lwd=3)
    lines(eps, nD)
  })
  ## plot number of deposit pixels in feature against pixel size
  plotit(paste0(tag, "nDB"), {
    plot(eps, nDB, log="x", type="l",
         xlab="pixel width (km)",
         ylab=switch(ylabels,
                     text=paste("number of deposit pixels in", featuretext),
                     symbols=expression(n[D*B])))
    abline(h=exact$nDB, col="green", lwd=3)
  })

  ## plot fraction of pixels in greenstone against pixel size
  plotit(paste0(tag, "fB"), {
    plot(eps, nB/npixels, log="x", type="l",
         xlab="pixel width (km)",
         ylab=switch(ylabels,
                     text=paste("fraction of pixels in", featuretext),
                     symbols=expression(n[B]/n)))
    lines(eps, nB/npixels)
    abline(h=exact$fB, col="green", lwd=2)
  })
}

