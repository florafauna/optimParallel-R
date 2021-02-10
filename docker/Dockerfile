FROM rocker/r-devel

MAINTAINER Florian Gerber <flora.fauna.gerber@gmail.com>

## add directories from host
## ADD devel /devel

RUN apt-get update && apt-get install -y libxml2-dev qpdf pandoc ghostscript-x joe git
RUN echo "local({r <- getOption('repos'); r['CRAN'] <- 'https://cloud.r-project.org'; options(repos=r)}); options(Ncpus = max(parallel::detectCores(), 1))" > ~/.Rprofile
RUN R -e "install.packages(c('R.rsp', 'roxygen2', 'spam', 'microbenchmark', 'testthat', 'ggplot2', 'numDeriv', 'lbfgsb3', 'lbfgsb3c'))"
RUN RD -e "install.packages(c('R.rsp', 'roxygen2', 'spam', 'microbenchmark', 'testthat', 'ggplot2', 'numDeriv', 'lbfgsb3', 'lbfgsb3c'))"