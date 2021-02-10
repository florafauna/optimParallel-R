#' @name optimParallel
#' @aliases optimparallel optimParallel-package optimParallel-Package OptimParallel-package OptimParallel-Package optimparallel-package optimparallel-Package 
#' @author Florian Gerber, \email{flora.fauna.gerber@@gmail.com}, \url{https://user.math.uzh.ch/gerber}.
#' @title parallel version of the L-BFGS-B method of \code{\link[stats]{optim}}
#' @keywords package
#' @docType package
#' @description
#' The function provides a parallel version of the L-BFGS-B method of \code{\link[stats]{optim}}.
#' If the evaluation time of the objective function \code{fn} is more than 0.1 sceconds, \code{optimParallel} can significantly reduce the optimization time. 
#' For a \eqn{p}-parameter optimization the speed increase is about factor \eqn{1+2p} when no analytic gradient is specified and \eqn{1+2p} processor cores are available.
#' @param par see the documentation of \code{\link[stats]{optim}}.
#' @param fn see the documentation of \code{\link[stats]{optim}}.
#' @param gr see the documentation of \code{\link[stats]{optim}}.
#' @param ... see the documentation of \code{\link[stats]{optim}}.
#' See section 'Notes' for more information. 
#' @param lower see the documentation of \code{\link[stats]{optim}}.
#' @param upper see the documentation of \code{\link[stats]{optim}}.
#' @param control see the documentation of \code{\link[stats]{optim}}.
#' @param hessian see the documentation of \code{\link[stats]{optim}}.
#' @param parallel is a list of additional control parameters and can supply any of the following components:
#' \describe{
#' \item{\code{cl}}{ an object of class \code{"cluster"} specifying the cluster to be used for parallel execution.
#' See \code{\link[parallel]{makeCluster}} for more information.
#' If the argument is not specified or \code{NULL}, the default cluster is used.
#' See \code{\link[parallel]{setDefaultCluster}} for information on how to set up a default cluster.} 
#'  \item{\code{forward}}{ logical vector of length 1. If \code{FALSE} (default when loading the package), a numeric central difference approximation of the gradient defined as
#' \eqn{(fn(x+\epsilon)-fn(x-\epsilon))/(2\epsilon)} is used, which corresponds to the gradient approximation used in \code{\link[stats]{optim}}.
#' If \code{TRUE}, a numeric forward difference approximation of the gradient essentially defined as
#' \eqn{(fn(x+\epsilon)-fn(x))/\epsilon} is used. This reduces the number of function calls from \eqn{1+2p} to \eqn{1+p} and can be useful if the number of available cores is smaller than \eqn{1+2p} or if the memory limit is reached. Note that the numeric central difference approximation is more accurate than the numeric forward difference approximation.}
#' \item{\code{loginfo}}{ logical vector of length 1 with default value \code{FALSE} when loading the package. If \code{TRUE},
#' additional log information containing the evaluated parameters as well as return values of \code{fn} and \code{gr} is returned.}
#' }
#' 
#' @return Same as the return value of \code{\link[stats]{optim}}. See the documentation thereof for more information.\cr
#' If \code{parallel=list(loginfo=TRUE)}, additional log information containing the evaluated parameters as well as
#' the return values of \code{fn} and \code{gr} is returned.
#'
#' @details \code{optimParallel} is a wrapper to \code{\link[stats]{optim}} and relies on the lexical scoping mechanism of R
#' and the R package \pkg{parallel} to evaluate \code{fn}
#' and its (approximate) gradient in parallel.\cr\cr
#' Some default values of the argument \code{parallel} can be set via\cr\code{options("optimParallel.forward", "optimParallel.loginfo")}.
#'
#' @references F. Gerber, R. Furrer (2019)
#' optimParallel: An R package providing a parallel version of the L-BFGS-B optimization method.
#' The R Journal, 11(1):352-358, https://doi.org/10.32614/RJ-2019-030
#' Also available as vignette of this package \code{vignette("optimParallel")}. 
#'
#' @section Notes:
#' \describe{
#' \item{1.}{If \code{fn} or \code{gr} depend on functions or methods from loaded packages,
#' it may be necessary to explicitly load those packages in all processes of the cluster.
#' For \code{cl} of class \code{"cluster"} one can use \code{clusterEvalQ(cl, search())} to check
#' whether all required packages are on the search paths of all processes.
#' If, for example, the R package \pkg{spam} is required and missing on those search paths,
#' it can be added via \code{clusterEvalQ(cl, library("spam"))}.} 
#' \item{2.}{If \code{fn} or \code{gr} have more than one argument,
#' it may be necessary to pass those to \code{optimParallel} via the \code{...} argument.
#' An illustration is given in the section 'Examples'. }
#' \item{3.}{We recommend that all R objects used by \code{fn} and/or \code{gr} are passed to \code{fn} and/or \code{gr} via arguments.
#' In certain cases it may also work that \code{fn} and/or \code{gr} use objects from the \code{.GlobalEnv} (without having corresponding arguments).
#' In that case it can be necessary to pass those objects to all processes of the used cluster via \code{\link[parallel]{clusterExport}}.
#' An illustration is given in the section 'Examples'.} 
#' \item{4.}{Using parallel R code inside \code{fn} and \code{gr} can work if suitable clusters are setup (one cluster for \code{optimParallel} and one for the parallel execution of \code{fn} and \code{gr}).}
#' \item{5.}{Using \code{optimParallel} with \eqn{n} parallel processes increases the memory usage by about factor \eqn{n} compared to a call to \code{\link[stats]{optim}}.
#' If the memory limit is reached this may severely slowdown the optimization.
#' Strategies to reduce memory usage are
#' (1) kill all unused processes on the computer,
#' (2) revise the code of \code{fn} and/or \code{gr} to reduce its memory usage, and
#' (3) reduce the number of parallel processes by specifying the argument \code{parallel=list(forward=TRUE)} and/or
#' setting up a cluster with less parallel processes.}
#' }
#'
#' @section Issues and bug report:
#' A list of known issues of \code{optimParallel} can be found at \url{https://github.com/florafauna/optimParallel-R/issues}.
#' Please report issues not listed there to\eqn{\,} \email{flora.fauna.gerber@@gmail.com}. Do not forget to include
#' an R script reproducing the issue and the output of \code{sessionInfo()}. 
#' 
#' @seealso
#' \code{\link[stats]{optim}},
#' \code{\link[parallel]{makeCluster}},
#' \code{\link[parallel]{setDefaultCluster}},
#' \code{\link[parallel]{stopCluster}},
#' \code{\link[parallel]{detectCores}}.
#' @examples
#' negll <- function(par, x, sleep=0, verbose=TRUE){
#'     if(verbose)
#'         cat(par, "\n")
#'    Sys.sleep(sleep)
#'    -sum(dnorm(x=x, mean=par[1], sd=par[2], log=TRUE))
#' }
#' set.seed(13); x <- rnorm(1000, 5, 2)
#'
#' cl <- makeCluster(2)     # set the number of processor cores
#' setDefaultCluster(cl=cl) # set 'cl' as default cluster
#'
#' optimParallel(par=c(1,1), fn=negll, x=x, lower=c(-Inf, .0001))
#'
#' optimParallel(par=c(1,1), fn=negll, x=x, sleep=0, verbose=TRUE,
#'               lower=c(-Inf, .0001), parallel=list(loginfo=TRUE))
#' 
#' setDefaultCluster(cl=NULL); stopCluster(cl)
#'
#' ## default values of the argument 'parallel':
#' options("optimParallel.forward", "optimParallel.loginfo")
#' 
#' \dontrun{
#' ## - use all avilable processor cores
#' ## - return cat() output to R prompt
#' ##   (may have issues on Windows)
#' if(tolower(.Platform$OS.type) != "windows"){
#'     cl <- makeCluster(spec=detectCores(), type="FORK", outfile="")  
#' } else
#'     cl <- makeCluster(spec=detectCores(), outfile="")
#' setDefaultCluster(cl=cl)
#'
#' ## return log information
#' options(optimParallel.loginfo=TRUE)              
#'
#' ## stop if change of f(x) is smaller than 0.01
#' control <- list(factr=.01/.Machine$double.eps)
#'
#' optimParallel(par=c(1,1), fn=negll, x=x, sleep=.5, verbose=TRUE,
#'               verbose=TRUE, lower=c(-Inf, .0001), control=control)
#' ## each step invokes 5 parallel calls to negll()
#'
#' optimParallel(par=c(1,1), fn=negll, x=x, sleep=.5, verbose=TRUE,
#'               lower=c(-Inf, .0001), control=control,
#'               parallel=list(forward=TRUE))
#' ## each step invokes 3 parallel calls to negll()
#'
#' ## passing objects to fn/gr (see section 'Notes')
#' ## ----------------------------------------------
#' a <- 10
#' fn <- function(par, b) sum((par-a-b)^2)
#'
#' ## approach 1:
#' clusterExport(cl, "a")
#' optimParallel(par=1, fn=fn, b=1)
#'
#' ## approach 2 (recommended):
#' ## rewrite 'fn' such that all necessary objects
#' ## are passed as arguments
#' fn <- function(par, a, b) sum((par-a-b)^2)
#' optimParallel(par=1, fn=fn, a=20, b=1)
#' 
#' setDefaultCluster(cl=NULL); stopCluster(cl) }
#' @export
#' @importFrom stats optim
optimParallel <- function(par, fn, gr = NULL, ..., 
                          lower = -Inf, upper = Inf, control = list(), hessian = FALSE,
                          parallel=list()){

    fg <- FGgenerator(par=par, fn=fn, gr=gr, ..., lower=lower, upper=upper,
                      control = control, parallel=parallel)
    control$fnscale <- NULL # already taken into account in FGgenerator()
    out <- stats::optim(par=par, fn=fg$f, gr=fg$g, method = "L-BFGS-B", lower=lower,
                        upper=upper, control=control, hessian=hessian)
    out$value <- out$value*fg$control$fnscale
    if(hessian[1])
        out$hessian <- out$hessian*fg$control$fnscale
    if(fg$parallel$loginfo){
        out[[length(out)+1]] <- fg$getLog()
        names(out)[length(out)] <- "loginfo"
    }
    out
}


#' @importFrom parallel getDefaultCluster parLapply clusterExport clusterEvalQ
FGgenerator <- function(par, fn, gr=NULL, ..., 
                        lower=-Inf, upper=Inf,
                        control=list(), 
                        parallel=list()){
    stopifnot(is.numeric(par), is.vector(par), length(par)>=1)
    n <- length(par)
    stopifnot(is.function(fn))
    testFn(fn, dots=list(...))
    if(!is.null(gr)){
        stopifnot(is.function(gr))
        testFn(gr, dots=list(...))
    }
    
    stopifnot(is.numeric(lower), is.numeric(upper))
    if(any(is.na(lower))) lower[is.na(lower)] <- -Inf
    if(any(is.na(upper))) lower[is.na(upper)] <- Inf

    stopifnot(is.null(control$ndeps) || is.numeric(control$ndeps))
    if(is.null(control$ndeps))  control$ndeps <- 1e-3
    stopifnot(is.null(control$fnscale) || is.numeric(control$fnscale))
    control$fnscale <- if(is.null(control$fnscale)) 1 else control$fnscale[1]
    stopifnot(is.null(control$parscale) || is.numeric(control$parscale))
    if(is.null(control$parscale)) control$parscale <- 1
    if(is.null(gr)){
        ndeps_mat <- array(0, c(n,n))
        ndeps_vec <- rep(control$ndeps*control$parscale, length.out=n)
        diag(ndeps_mat) <- ndeps_vec
    }
    
    if(is.null(parallel$forward))
        parallel$forward <- getOption("optimParallel.forward")
    stopifnot(length(parallel$forward)==1,
              isTRUE(parallel$forward) || !isTRUE(parallel$forward))
    
    if(is.null(parallel$cl))
        parallel$cl <- parallel::getDefaultCluster()
    if(!inherits(parallel$cl, "cluster"))
        stop("No (default) cluster specified. See the description of the argument 'parallel'.")
    if(is.null(parallel$loginfo))
        parallel$loginfo <- getOption("optimParallel.loginfo")
    stopifnot(length(parallel$loginfo)==1,
              isTRUE(parallel$loginfo) || !isTRUE(parallel$loginfo))
    

    ## prepare function evaluations
    ## export '...', 'fn', and 'gr' to cluster
    dots <- list(...)
    e <- list2env(dots)
    assign("fn", fn, envir=e)
    assign("gr", gr, envir=e)
    parallel::clusterExport(parallel$cl, "e", list2env(list(e=e)))
    if(!is.null(gr)){
        exprList <- getExprGr(fn=fn, gr=gr, dots=dots)
    } else {
        nParallel <- if(parallel$forward) 1+length(par) else 1+2*length(par)
        exprList <- lapply(seq_len(nParallel), getExprApprox, f=fn, name=names(par), dots=dots)
    }
#    print(exprList)
    
    evalFG <- function(par){
        ## the first argument of fn has to be a vector of length length(par)
        if(identical(par, par_last))
            return(list(value=value, grad=grad))
        if(is.null(gr)){
            if(parallel$forward){
                ndepsused <- ndeps_vec
                parMat <- data.frame(cbind(array(par, c(n,n))+ndeps_mat))
                if(!(is.null(upper) || all(is.na(upper)) || all(upper==Inf))){
                    hitu <- unlist(lapply(parMat, function(par){any(par>upper)}))
                    if(any(hitu)){
                        parMatl <- data.frame(cbind(array(par, c(n,n))-ndeps_mat))
                        parMat[hitu] <- parMatl[hitu]
                        ndepsused[hitu] <- -ndeps_vec[hitu]
                    }
                }
                parMat <- cbind(parMat, par)
                parallel::clusterExport(parallel$cl, "parMat", list2env(list(parMat=parMat)))
                parallel::clusterEvalQ(parallel$cl, assign("parMat", parMat, envir=e))
                ev <- unname(unlist(parallel::parLapply(parallel$cl, exprList, eval)))
                ev <- ev/control$fnscale
                value <- ev[length(ev)]
                length(ev) <- length(ev)-1
                grad <- (ev-value)/ndepsused
            } else { # two sided
                ndepsused <- 2*ndeps_vec
                parMat <- data.frame(cbind(array(par, c(n,n))+ndeps_mat, array(par, c(n,n))-ndeps_mat))
                if(!(is.null(upper) || all(is.na(upper)) || all(upper==Inf))){
                    hitu <- unlist(lapply(parMat, function(par){any(par>upper)}))
                    if(any(hitu)){
                        parMat[hitu] <- par
                        hitui <- apply(matrix(hitu, ncol=2), 1, any)
                        ndepsused[hitui] <- ndeps_vec[hitui] 
                    }
                }
                if(!(is.null(lower) || all(is.na(lower)) || all(lower==Inf))){
                    hitl <- unlist(lapply(parMat, function(par){any(par<lower)}))
                    if(any(hitl)){
                        parMat[hitl] <- par
                        hitli <- apply(matrix(hitl, ncol=2), 1, any)
                        ndepsused[hitli] <- ndeps_vec[hitli] 
                    }
                }
                parMat <- cbind(parMat, par)
                parallel::clusterExport(parallel$cl, "parMat", list2env(list(parMat=parMat)))
                parallel::clusterEvalQ(parallel$cl, assign("parMat", parMat, envir=e))
                ev <- unname(unlist(parallel::parLapply(parallel$cl, exprList, eval)))
                ev <- ev/control$fnscale
                value <- ev[length(ev)]
                length(ev) <- length(ev)-1
                ev_mat <- matrix(ev, ncol=2)
                grad <- c(ev_mat[,1]-ev_mat[,2])/ndepsused
            }
        }else{ # gr is not null
            parallel::clusterExport(parallel$cl, "par", list2env(list(par=par)))
            parallel::clusterEvalQ(parallel$cl, assign("par", par, envir=e))
            res <- parallel::parLapply(parallel$cl, exprList, eval)
            value <- res[[1]]/control$fnscale 
            grad <- res[[2]]/control$fnscale 
        }
        if(is.null(optimlog)){
            optimlog <- c(i_e+1, par, value, grad, use.names=FALSE)
            names(optimlog) <- c("step", paste0("par", seq_along(par)), "fn", paste0("gr", seq_along(par)))
        }
        else{
            optimlog <- rbind(optimlog, c(i_e+1, par, value=value, grad=grad))
            rownames(optimlog) <- NULL
        }
        par_last <<- par
        value <<- value
        grad <<- grad
        optimlog <<- optimlog
        i_e <<- i_e+1
        return(list(value=value, grad=grad))
    }
    f <- function(par){
        evalFG(par) 
        i_f <<- i_f+1
        return(value)
    }
    g <- function(par){
        evalFG(par) 
        i_g <<- i_g+1
        return(grad)
    }
    init <- function(){
        i_f <<- i_g <<- i_e <<- 0
        par_last <<- value <<- grad <<- NA
    }
    getCount <- function(){
        c(i_e, i_f, i_g)
    }
    getLog <- function(){
        optimlog
    }
    i_f <- i_g <- i_e <- 0
    par_last <- value <- grad <- NA
    optimlog <- NULL
    list(f=f, g=g, init=init, evalFG=evalFG, getCount=getCount, getLog=getLog, control=control, parallel=parallel)
}

getFunCallStr <- function(fn, fnName="fn", dots){
    ex <- paste0(fnName, "(par") 
    if(!is.primitive(fn)){
        ff <- formals(fn)
        if(names(ff)[1] != "...")
            ff <- ff[-1]
        if(all(names(ff) != "..."))    
            ff <- ff[names(ff) %in% names(dots)]
        else
            ff <- dots
        if(length(ff)>=1){
            ex <- paste0(ex, ",")
            moreArgs <- paste(lapply(names(ff), function(x) paste0(x, "=", x)), collapse = ", ")
            ex <- paste0(ex, moreArgs)
        }
    }
    paste0(ex,")")
}

getExprApprox <- function(n, fn, name, dots){
    ex <- paste0("local({par <- parMat[,", n, "]; ")
    if(!is.null(name))
        ex <- paste0(ex, "names(par) <- c(", paste0("\"", name, "\"", collapse=", "), "); ")
    ex <- paste0(ex, getFunCallStr(fn, dots=dots), "}, envir=e)")
    parse(text=ex)
}

getExprGr <- function(fn, gr, dots){
    list(parse(text=paste0("local(", getFunCallStr(fn, "fn", dots=dots), ", envir=e)")),
         parse(text=paste0("local(", getFunCallStr(gr, "gr", dots=dots), ", envir=e)")))
}

testFn <- function(f, dots){
    if(!identical(dots, list()) && any(names(formals(args(f)))[1]==names(dots)))
        warning("The first argument of \"fn\" and/or \"gr\" has the same name as one argument passed through \"...\". The value passed through \"...\" for that argument is ignored.")
    invisible(NULL)
}
