## rm(list=ls())
## library("testthat")
## library("optimParallel", lib.loc = "../../../lib/")
source("testsetup.R")

context("test-issues")

control <- structure(list(maxit = 10,
                          factr = 2.22044604925031e-16),
                     .Names = c("maxit","factr"))

FN1 <- function(par, sleep){
    Sys.sleep(sleep)
    sum(par^2)
}

GR1 <- function(par, sleep){
    Sys.sleep(sleep)
    2*par
}

test_that("optimParallel",{
    compareOptim(list(par=c(1,2,3), fn=FN1, gr=GR1, sleep=0,
                      
                      control=control),
                 verbose=verbose)
})

FN2 <- function(par, sleep){
    Sys.sleep(sleep)
    par["a"]^2+par["b"]^2
}
GR2 <- function(par, sleep){
    Sys.sleep(sleep)
    2*c(par["a"],par["b"])
}

FN3 <- function(par, sleep){
    Sys.sleep(sleep)
    par["a"]^2
}
GR3 <- function(par, sleep){
    Sys.sleep(sleep)
    2*c(par["a"])
}

test_that("optimParallel - named arguments",{
    compareOptim(list(par=c(a=1,b=2), fn=FN2, sleep=0,
                      control=control),
                 verbose=verbose)
    compareOptim(list(par=c(a=1,b=2), fn=FN2, gr= GR2, sleep=0,
                      control=control),
                 verbose=verbose)
    compareOptim(list(par=c(a=1), fn=FN3, sleep=0,
                      control=control),
                 verbose=verbose)
    compareOptim(list(par=c(a=1), fn=FN3, gr= GR3, sleep=0,
                      control=control),
                 verbose=verbose)
})


test_that("optimParallel - use compiled code from other packages",{
    compareOptim(list(par=c(a=1), fn=dnorm,
                      control=control),
                 verbose=verbose)
    compareOptim(list(par=c(a=1), fn=dnorm, mean=1,
                      control=control),
                 verbose=verbose)
})

FN4 <- function(par, a, sleep=0){
    Sys.sleep(sleep)
    sum(10*par^2) + a
}

GR4 <- function(par, b, sleep=0){
    Sys.sleep(sleep)
    b*2*par
}


test_that("optimParallel - fn and gr can have different aguments",{
    expect_equal(optimParallel(par=1, fn=FN4, gr=GR4,
                               a=1, b=10, sleep=0,
                               control=control)$par, 0)
})


test_that("optimParallel return correct sign of hessian if 'fnscale=-1'", {
    set.seed(13)
    x <- rnorm(1000, 5, 2)
    negll <- function(par, x) { -sum(dnorm(x=x, mean=par[1], sd=par[2], log=TRUE)) }
    posll <- function(par, x) {  sum(dnorm(x=x, mean=par[1], sd=par[2], log=TRUE)) }

    compareOptim(list(par=c(1,1), fn=negll, x=x,
                      control=control, hessian=TRUE),
                 verbose=verbose)

    compareOptim(list(par=c(1,1), fn=posll, x=x,
                      control=c(control, fnscale=-1), hessian=TRUE),
                 verbose=verbose)
})

test_that("fn can have normal and ... aguments", {
    fn <- function(par, data, ...)  par^2
    compareOptim(list(par=1, fn=fn, data=1:10,
                      control=control),
                 verbose=verbose)
})



