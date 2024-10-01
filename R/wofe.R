#'
#'  wofe.R
#'
#'  WofE calculation
#'
#'   author: Adrian Baddeley
#'   Copyright (c) Adrian Baddeley 2021-2024

wofe <- function(X, Blist, ..., pixelsize=0, Xname,
                 presenceRule = c("centre",
                                  "somewhere", "majority", "everywhere"),
                 respectDeposits = FALSE,
                 domain=Window(X)) {
  require(spatstat.utils)
  if(missing(Xname)) Xname <- short.deparse(substitute(X))
  presenceRule <- match.arg(presenceRule)
  wofeEngine(X=X, Blist=Blist, ..., pixelsize=pixelsize, Xname=Xname,
             presenceRule=presenceRule, respectDeposits=respectDeposits,
             domain=domain)
}

wofeEngine <- function(X, Blist, ..., pixelsize, Xname,
                       presenceRule, respectDeposits, domain,
                       secretexit = FALSE) {
  et <- evidenceTable(X, Blist,
                      ...,
                      as.columns=TRUE,
                      pixelsize=pixelsize, 
                      presenceRule=presenceRule,
                      respectDeposits=isTRUE(respectDeposits),
                      domain=domain)
  nc <- ncol(et) # n layers, then X, then weights
  weights <- et[,nc]
  npred <- nc-2
  ## ensure conditions are logical-valued
  et[,1:(nc-1)] <- apply(et[,1:(nc-1),drop=FALSE], 2, as.logical)
  ## deposits
  deposits <- et[, nc-1]
  oD <- sum(weights[deposits])/sum(weights[!deposits]) ## odds, or intensity
  ##
  predictors <- et[, 1:npred, drop=FALSE]
  ##
  pBgivenD <- apply(predictors[deposits, , drop=FALSE], 2,
                    function(x, w) { sum(w[x])/sum(w) },
                    w=weights[deposits])
  pBgivenNotD <- apply(predictors[!deposits, , drop=FALSE], 2,
                       function(x, w) { sum(w[x])/sum(w) },
                       w=weights[!deposits])
  weightBplus <- log(pBgivenD/pBgivenNotD)
  weightBminus <- log((1-pBgivenD)/(1-pBgivenNotD))
  contrastB <- weightBplus - weightBminus
  ##
  param <- rbind(Wplus=weightBplus, Wminus=weightBminus, C=contrastB)
  colnames(param) <- colnames(et)[1:npred]
  ##
  result <- list(Xname=Xname, pixelsize=pixelsize,
                 et=et, param=param,
                 presenceRule=presenceRule,
                 respectDeposits=respectDeposits,
                 logpriorodds=log(oD))
  if(!secretexit) {
    result <- append(list(X=X, W=Window(X), Blist=Blist),
                     result)
    class(result) <- "wofe"
  }
  return(result)
}


print.wofe <- function(x, ...) {
  splat("Weights of evidence analysis")
  splat("Point pattern response:", x$Xname)
  splat("Fitted weights:")
  print(x$param)
  pix <- x$pixelsize
  unites <- with(summary(unitname(x$X)), paste(plural, explain))
  splat("Calculation using",
        if(pix == 0) "exact areas" else paste("pixels of size", pix, unites))
  if(pix > 0) {
    rule <- x$presenceRule %orifnull% "centre"
    splat("Discretisation rule:", sQuote(rule))
    respect <- x$respectDeposits %orifnull% FALSE
    splat("Respect deposits?", if(respect) "yes" else "no")
  }
  return(invisible(NULL))
}

predict.wofe <- function(object, ..., type=c("response", "weights")) {
  type <- match.arg(type)
  eps <- object$pixelsize
  if(eps == 0) {
    Zlist <- lapply(object$Blist,
                    function(B, W, ...) as.im(as.function(B), W=W, ...),
                    W=object$W, ...)
  } else {
    Zlist <- lapply(object$Blist,
                    function(B, W, eps) as.im(as.function(B), W=W, eps=eps),
                    W=object$W, eps=eps)
  }
  WZ <- mapply(function(Wminus,C,B) { Wminus + C*B },
               Wminus=as.list(object$param["Wminus",]),
               C=as.list(object$param["C",]),
               B=Zlist, SIMPLIFY=FALSE)
  if(type == "weights") {
    names(WZ) <- names(object$Blist)
    return(as.solist(WZ))
  }
  result <- exp(object$logpriorodds + Reduce("+", WZ))
  return(result)
}

      
