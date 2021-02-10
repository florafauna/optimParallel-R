rm(list = ls())
detach("package:optimParallel", unload=TRUE)
system("make lib")
library("optimParallel", lib.loc = "lib/")
## todo:
## - other optim methods
## - cluster make and stop automatically on.exit()
## - set Cores, 

## for Linux like platforms:
## -------------------------
cl <- makeCluster(3)
setDefaultCluster(cl)


## passing objects to fn/gr (see section 'Notes')
## ----------------------------------------------
a <- 10
fn <- function(par, b) sum((par-a-b)^2) 

## approach 1:
clusterExport(cl, "a")
optimParallel(par=1, fn=fn, b=1)

## approach 2 (recommended):
## rewrite 'fn' such that all necessary objects
## are passed as arguments
fn <- function(par, a, b) sum((par-a-b)^2) 
optimParallel(par=1, fn=fn, a=20, b=1)


negll <- function(par=0, sleep, verbose=FALSE){
    if(verbose)
        cat(par, "\n")
    Sys.sleep(sleep)
    -sum(dnorm(x=a, mean=par["mean"], sd=par["sd"], log=TRUE))
}
set.seed(13); a <- rnorm(1e3, 5, 2)


negll(1, sleep=0)

cl <- makeCluster(3)
setDefaultCluster(cl)
o1 <- optimParallel(par=c(mean=1, sd=1), fn=negll, 
                    verbose=TRUE,
                    sleep=0,
                    lower=c(-Inf), method="L-BFGS-B")

clusterExport(cl=cl,  )

system.time(o2 <- optim(par=c(mean=1,sd=1), fn=negll, verbose=TRUE, sleep=0,
                        lower=c(-Inf, .0001), method="L-BFGS-B"))



     optimParallel(par=c(mean=1,sd=1), fn=dnorm, mean=1, sd=1,
                   method = "L-BFGS-B", lower=c(-Inf, .0001))

     ## it can be faster to reuse an existing cluster multiple times.
     cl <- makeCluster(detectCores())
     optimParallel(par=c(1,1), fn=negll, x=x,
                   method = "L-BFGS-B", lower=c(-Inf, .0001),
                   parallel_args = list(cl=cl))
     
     ## the cluster can also be set in options().
     options(optimParallel.cl=cl)
     optimParallel(par=c(1,1), fn=negll,
                   method = "L-BFGS-B", lower=c(-Inf, .0001))
     
     ## when no longer used, the cluster can be stopped.
     stopCluster(cl); options(optimParallel.cl=NULL)

cl <-      makeForkCluster(nnodes = getOption("mc.cores", 2L))

a <- parLapply(cl=cl, 1:4, cat)

detectCores()                             # available cores
options(mc.cores=detectCores())           # set number of cores
options(optimParallel.lapply="mclapply")  # used by default

optimParallel(par=c(1,1), fn=neg2ll,
              method = "L-BFGS-B", lower=c(-Inf, .0001))

optimParallel(par=c(1,1), fn=neg2ll,
              verbose=TRUE, sleep=.5, # args to neg2ll()
              method = "L-BFGS-B", lower=c(-Inf, .0001),
              control= list(factr = .01/.Machine$double.eps),
              parallel_args=list(log=TRUE))
## each step invokes 5 parallel calls to neg2ll()


tt <- optimParallel(par=c(1,1), fn=neg2ll,
              verbose=TRUE, sleep=.5, 
              method = "L-BFGS-B", lower=c(-Inf, .0001),
              parallel_args=list(log=TRUE, one_sided=TRUE))
## each step invokes 3 parallel calls to neg2ll()

tt <- optimParallel(par=c(1,1), fn=neg2ll,
                    verbose=TRUE, sleep=.5, 
                    method = "L-BFGS-B", 
                    control= list(factr = .01/.Machine$double.eps),
                    parallel_args=list(log=TRUE, one_sided=TRUE))

## each step invokes 3 parallel calls to neg2ll()

optim(par=c(1,1), fn=neg2ll,
      verbose=TRUE, sleep=.5, 
      method = "BFGS", 
      control= list(factr = .01/.Machine$double.eps))






## Use option cl.cores to choose an appropriate cluster size.
cl <- makeCluster(2, outfile="")
verbose=TRUE
optimParallel(par=c(1,1), fn=neg2ll, x=x,
              verbose=TRUE, sleep=0, 
              method = "L-BFGS-B", lower=c(-Inf, .0001),
              parallel_args=list(lapply="parLapply", cl=cl))


system.time(optimParallel(par=c(1,1), fn=neg2ll, x=x,
              verbose=FALSE, sleep=.3, 
              method = "L-BFGS-B", lower=c(-Inf, .0001),
              parallel_args=list(lapply="lapply")))


neg2ll <- function(par, sleep=0, verbose=FALSE){
    if(verbose)
        cat(par, "\n")
    Sys.sleep(sleep)
    -sum(dnorm(x=x, mean=par[1], sd=par[2], log=TRUE))
}
x <- rnorm(1000, 5, 2)


options(optimParallel.lapply="parLapply")
cl <- makeCluster(detectCores())

optimParallel(par=c(1,1), fn=neg2ll,
              x=x, # it is required to pass all data used by fn 
              method = "L-BFGS-B", lower=c(-Inf, .0001),
              parallel_args=list(cl=cl))

optimParallel(par=c(1,1), fn=neg2ll,
              x=x, verbose=TRUE, sleep=.5,
              method="L-BFGS-B", lower=c(-Inf, .0001),
              control=list(factr=.01/.Machine$double.eps),
              parallel_args=list(cl=cl))
## note: printing to screen does not work with clusters

optimParallel(par=c(1,1), fn=neg2ll,
              x=x, verbose=TRUE, sleep=.5,
              method ="L-BFGS-B", lower=c(-Inf, .0001),
              control=list(factr=.01/.Machine$double.eps),
              parallel_args=list(cl=cl, one_sided=TRUE))

stopCluster(cl)

## optim and spatial prediction on a transect
n <- 100
loc <- runif(n)
distmat <- as.matrix(dist(loc))
expcov <- function(h, par){
    par[2] * exp(-h/par[1])
}
Sigma <- expcov(distmat, c(2,4))
data <- c(rmvnorm.spam(1, Sigma=as.spam(Sigma)))
plot(loc, data)









test <- function(...){
    args <- list(...)
    if(any(names(args) %in% c("cl", "X", "x", "fun", "mc.preschedule", "mc.set.seed", "mc.silent", 
                              "mc.cores", "mc.cleanup", "mc.allow.recursive"))
       stop("arguments passer though '...' cannot be named: \n
'cl', 'X', 'x', 'fun', 'mc.preschedule', 'mc.set.seed', 'mc.silent',\n
'mc.cores', 'mc.cleanup', 'mc.allow.recursive'. ")
}








negll <- function(par, data, sleep=0, cl, verbose=FALSE){
        if(verbose)
           cat(par, "\n")
        Sys.sleep(sleep)
        -sum(dnorm(x=data, mean=par[1], sd=par[2], log=TRUE))
     }
     set.seed(13); data <- rnorm(1000, 5, 2)
     
     optimParallel(par=c(1,1), fn=negll, data=data, cl=1,
                   method = "L-BFGS-B", lower=c(-Inf, .0001))
     
     optimParallel(par=c(1,1), fn=negll, x=x,
                   verbose=TRUE, sleep=.5, # args to negll()
                   method="L-BFGS-B", lower=c(-Inf, .0001),
                   control=list(factr=.01/.Machine$double.eps))
     ## each step invokes 5 parallel calls to negll()
     
     optimParallel(par=c(1,1), fn=negll, x=x,
                   verbose=TRUE, sleep=.5,
                   method ="L-BFGS-B", lower=c(-Inf, .0001),
                   control=list(factr=.01/.Machine$double.eps),
                   parallel_args=list(one_sided=TRUE))
     ## each step invokes 3 parallel calls to negll()
     
     ## store and access the log file:
     o <- optimParallel(par=c(1,1), fn=negll, x=x, method = "L-BFGS-B",
                        lower=c(-Inf, .0001), parallel_args=list(log=TRUE))
     o$log
     
     
     ## default values of the argument 'paralle_args'
     ## ---------------------------------------------
     options("optimParallel.one_sided", "optimParallel.log",
             "optimParallel.lapply", "optimParallel.cl")
     ## they can be changed via, e.g.,
     # options(optimParallel.log=TRUE)
     
     
     ## usage of parLapply (default on Windows)
     ------------------------------------------
     options(optimParallel.lapply="parLapply")
     ## optimParallel() makes a cluster and stops it on exit.
     optimParallel(par=c(1,1), fn=negll, x=x,
                   method = "L-BFGS-B", lower=c(-Inf, .0001))
     
     ## it can be faster to reuse an existing cluster multiple times.
     cl <- makeCluster(detectCores())
     optimParallel(par=c(1,1), fn=negll, x=x, 
                   method = "L-BFGS-B", lower=c(-Inf, .0001),
                   parallel_args = list(cl=cl))
     
     ## the cluster can also be set in options().
     options(optimParallel.cl=cl)
     optimParallel(par=c(1,1), fn=negll, x=x,
                   method = "L-BFGS-B", lower=c(-Inf, .0001))
     
     ## when no longer used, the cluster can be stopped.
     stopCluster(cl); options(optimParallel.cl=NULL)
     ## End(Not run)




test  <- function(a, b){
    list(...)
    
}

where <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
    
  } else if (exists(name, envir = env, inherits = FALSE)) {
    # Success case
    env
    
  } else {
    # Recursive case
    where(name, parent.env(env))
    
  }
}

    
renameargs <- function(fun, forbidden="f1"){
    names_in <- names(formals(fun))
    rena <- names_in %in% forbidden
    if(!(any((rena))))
        return(fun)
    names_new <- ifelse(rena, paste0(names_in,"_AVeryUnliklyName"), names_in)
    
    args <- lapply(seq_along(names_in),
                   function(x){ eval(parse(text=paste0("quote(",names_new[x],")"))) })
    names(args) <- names_in

    env_A <- new.env(parent=parent.frame(1))
    assign("fun", fun, envir=env_A)
    fout <- function(a){
        names_in_A <- names_in
        names_new_A <- names_new
        rm(names_in, names_new)
        print(names_in_A)
        for(i in seq_along(names_in))
            assign(names_in[i], eval(parse(text=names_new[i])), envir=env_A)
        ls( envir=env_A)
        rm(list = c("what", "args", "quote", "envir"))
        print(get("args", envir=env_A))
        do.call(what="fun", args=args, quote=FALSE, envir=env_A)

    }
    args_fout <- vector("pairlist",length(args))
    names(args_fout) <- names_new
    formals(fout) <- args_fout
    fout
}


test <- function(names_in, args){
    names_in+args
}

t1 <- renameargs(test, "names_in")
t1(a=2, args=3)
t1(a=2, args=3)
t1(names_in_AVeryUnliklyName=2, args=3)

f <- function(x){
    y <- x+1
    y
}

f <- function(x) x


tt <- gsub("x", "y", as.character(body(f)))
body(f) <- quote(tt)

body(f) <- parse(text=gsub("x", "y", as.character(body(f))))
    
renameArguments <- function(f, argnames="x", suffix="_AVeryUnliklyName"){
    stopifnot(is.function(f),
              is.character(argnames),
              is.character(suffix), length(suffix)==1)
    argnames <- argnames[argnames%in%names(formals(f))]
    bb <- as.character(body(f))
    if(bb[1]=="{")
        bb <- bb[2:length(bb)]
    bb <- paste0(bb, collapse="; ")
    bb <- paste0(c("{", bb, "}"), collapse="")

    for(i in seq_along(argnames))
        bb <- gsub(argnames[i], paste0(argnames[i], suffix, collapse=""), bb)
    body(f) <- parse(text=bb)

    for(i in seq_along(names(formals(f))))
        names(formals(f))[i][names(formals(f))[i]%in%argnames[i]] <- paste0(argnames[i], suffix, collapse="")
    f
}

    change(f, "x")


    tf <- function(a=1, b, c, d){
        x <- a+b
        y <- c+d
        x+y
    }
    tfo <- change(tf, c("a", "x"))


require(parallel)
cl <- makeCluster(2)
y <- 1
f <- function(x, y){
    x+y
}
args <- list(x=1)

gen <- function(f, args){
    for(i in seq_along(args)){
        assign(names(args)[i], args[[i]])
    }
    ff <- function(){}
    body(ff) <- body(f)
    update_fistargument <- function(value){
        assign(names(args)[1], value, inherits=TRUE)
    }
    list(f=ff, update_fistargument=update_fistargument)
}

integrates_args <- function(f, args){
    for(i in seq_along(args)){
        assign(names(args)[i], args[[i]])
    }
    ff <- function(){}
    body(ff) <- body(f)
    ff
}



par <- matrix(1:3)
getList <- function(par, args, f){
    lapply(1:nrow(par), function(x){
        args[[1]] <- par[x]
        gen(f=f, args)
    })
}

getList(matrix(1:3), list(x=1,y=2), f)

    
parLapply()
mclapply(1:3, FUN=function(x, FUN) {sum(x)}, FUN=1 )


    
