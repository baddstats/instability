#'
#'   evidenceTable.R
#'
#'   Given a point pattern X and a list of evidence layers,
#'   compile a table of the unique conditions of evidence
#'   and the corresponding counts or areas.
#'
#'   author: Adrian Baddeley
#'   Copyright (c) Adrian Baddeley 2021-2024

evidenceTable <- local({

  inout <- function(W, P, fac=TRUE) {
    result <- inside.owin(P, w=W)
    if(fac) result <- factor(result, levels=c(TRUE, FALSE))
    return(result)
  }

  logique <- factor(c(TRUE, FALSE), levels=c(TRUE, FALSE))

  vrai <- logique[1]
  faux <- logique[2]
  
  evidenceTable <- function(X, Blist, ...,
                            pixelsize=0,
                            as.columns=FALSE,
                            presenceRule = c("centre",
                                             "somewhere",
                                             "majority",
                                             "everywhere"),
                            respectDeposits=FALSE,
                            vote = c("first", "last", "any", "all"),
                            domain=Window(X)) {
    if(!is.ppp(X)) stop("X should be a point pattern")
    if(!(is.list(Blist) && all(sapply(Blist, is.owin))))
      stop("Blist should be a list of windows, representing evidence layers")
    presenceRule <- match.arg(presenceRule)
    vote <- match.arg(vote)

    ## clip all data (features and windows) to the same spatial domain
    if(!missing(domain)) {
      stopifnot(is.owin(domain))
      X <- X[domain]
    } 
    Blist <- lapply(Blist,
                    function(x, w) { intersect.owin(x, w) },
                    w=domain)
    ## assign names
    nlayers <- length(Blist)
    if(sum(nzchar(names(Blist))) < nlayers)
      names(Blist) <- LETTERS[seq_len(nlayers)]

    if(pixelsize == 0) {
      ## classify points according to evidence layers
      Pfac <- lapply(Blist, inout, P=X)
      Pfac <- as.data.frame(c(list(weight=1), Pfac, list(X=vrai)))
      ## construct the Venn diagram formed by the evidence layers
      DD <- do.call(venn.tess, append(Blist, list(window=domain, labels=TRUE)))
      ## compute area of each piece
      areas <- tile.areas(DD)
      ## assemble table of results in correct format
      Afac <- as.list(as.data.frame(marks(DD)))
      names(Afac) <- names(Blist)
      Afac <- as.data.frame(c(list(weight=areas), Afac, list(X=faux)))
      ## combine and tabulate
      fac <- rbind(Pfac, Afac)
      tab <- tapplysum(fac[,1], fac[,-1,drop=FALSE], do.names=TRUE)
    } else {
      ## pixellate the evidence layers and deposit locations
      Xpix <- pixellate(X, W=domain, eps=pixelsize, ..., padzero=FALSE,
                        savemap=respectDeposits)
      Xim <- (Xpix > 0) 
      ## Determine criterion for presence of feature
      op <- switch(presenceRule,
                   centre     = "sample",   # centre of pixel falls inside
                   somewhere  = "cover",    # pixel intersects feature
                   majority   = "majority", # > 50% of pixel is inside feature
                   everywhere = "inside")   # pixel is wholly inside feature
      ## convert all windows to masks using same raster as Xim
      Bmasks <- lapply(Blist, owin2mask, xy=Xim, op=op)
      Bimages <- solapply(Bmasks, as.im, na.replace=0)
      BXlistPix <- as.solist(append(Bimages, list(X=Xim)))
      ## evaluate the evidence and response at each pixel 
      dd <- pairs(BXlistPix, plot=FALSE, drop=TRUE)
      ## convert to presence/absence
      ## dd <- as.data.frame(dd > 0)
      dd <- do.call(data.frame, lapply(dd, as.logical))
      names(dd) <- names(BXlistPix)
      ##
      if(respectDeposits) {
        ## extract pixel data for 'barren' pixels only
        depositpixel <- dd[,"X"]
        ddA <- dd[!depositpixel, , drop=FALSE]
        ## for 'presence', use exact status of each deposit
        Xmap <- attr(Xpix, "map")
        switch(vote,
               first = {
                 ##  Status determined by first deposit in pixel
                 Xfirst <- X[!duplicated(Xmap)]
                 depositdata <- lapply(Blist, inout, P=Xfirst, fac=FALSE)
               },
               last = {
                 ## Status determined by last deposit in pixel
                 Xlast <- X[rev(!duplicated(Xmap[nrow(Xmap):1, ]))]
                 depositdata <- lapply(Blist, inout, P=Xlast, fac=FALSE)
               },
               any = {
                 ## Status is TRUE if any deposits fall in feature
                 ## Determine whether deposits lie inside/outside each feature
                 alldepositdata <- lapply(Blist, inout, P=X, fac=FALSE)
                 ## Group deposits by containing pixel
                 g <- uniquemap(Xmap)
                 hasany <- by(as.data.frame(alldepositdata),
                              factor(g),
                              function(x) apply(x, 2, all, simplify=FALSE),
                              simplify=FALSE)
                 depositdata <- Reduce(rbind, hasany)
               },
               all = {
                 ## Status is TRUE if all deposits fall in feature
                 ## Determine whether deposits lie inside/outside each feature
                 alldepositdata <- lapply(Blist, inout, P=X, fac=FALSE)
                 ## Group deposits by containing pixel
                 g <- uniquemap(Xmap)
                 hasany <- by(as.data.frame(alldepositdata),
                              factor(g),
                              function(x) apply(x, 2, all, simplify=FALSE),
                              simplify=FALSE)
                 depositdata <- Reduce(rbind, hasall)
               })
        ddP <- data.frame(as.data.frame(depositdata), list(X=vrai))
        ## combine
        dd <- rbind(ddA, ddP)
      }
      ## compile the table: count pixels falling in each combination of layers
      fac <- lapply(as.list(dd), factor, levels=list(TRUE, FALSE))
      tab <- do.call(table, fac)
    }
    if(as.columns) {
      u <- rep(list(logique), nlayers+1)
      names(u) <- c(names(Blist), "X")
      g <- do.call(expand.grid, u)
      h <- as.data.frame(as.list(g))
      tab <- cbind(h, data.frame(weight=as.numeric(tab)))
    }
    return(tab)
  }

  evidenceTable
})
