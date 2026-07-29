.onLoad <- function(libname, pkgname)
{
    options(optimParallel.forward=getOption("optimParallel.forward", FALSE))
    options(optimParallel.loginfo=getOption("optimParallel.loginfo", FALSE))
}
