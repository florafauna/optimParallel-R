set.seed(11)

x <- rnorm(n = 1e7, mean = 5, sd = 2)
negll <- function(par, x) -sum(dnorm(x = x, mean = par[1], sd = par[2], log = TRUE))
o1 <- optim(par = c(1, 1), fn = negll, x = x, method = "L-BFGS-B",
            lower = c(-Inf, 0.0001))
o1$par

#install.packages("optimParallel")
library(optimParallel)
cl <- makeCluster(detectCores()); setDefaultCluster(cl = cl)
o2 <- optimParallel(par = c(1, 1), fn = negll, x = x, lower = c(-Inf, 0.0001))
identical(o1, o2)

o3 <- optimParallel(par = c(1, 1), fn = negll, x = x, lower = c(-Inf, 0.0001),
                    parallel=list(loginfo = TRUE))
head(o3$loginfo, n = 3)
tail(o3$loginfo, n = 3)

negll_gr <- function(par, x){
    sm <- mean(x); n <- length(x)
    c(-n*(sm-par[1])/par[2]^2,
      n/par[2] - (sum((x-sm)^2) + n*(sm-par[1])^2)/par[2]^3)
}
o4 <- optimParallel(par = c(1, 1), fn = negll, gr = negll_gr, x = x,
                    lower = c(-Inf, 0.0001), parallel=list(loginfo = TRUE))
tail(o4$loginfo, n = 3)

## make Figure 1
library(ggplot2); theme_set(theme_bw())
library(foreach)
library(doParallel); registerDoParallel(12)
pdf("path.pdf", width = 8*.9*.9, height = 5*.8*.9)
grid <- expand.grid(par1=seq(.7, 7, length.out=100),
                    par2=seq(.7, 4, length.out=100),
                    z=NA)
grid$z <- foreach(i=1:nrow(grid), .combine=c) %dopar% {
    negll(c(grid$par1[i], grid$par2[i]), x=x)
}

col <- alpha(colorRampPalette(c("gray"))(55), 1)
stroke <- 1.1
shape <- 1
ggplot(data=data.frame(o3$log), aes(x=par1, y=par2)) +
    geom_contour(mapping=aes(x=par1, y=par2, z=z, color=..level..), data=grid,
                 bins=55) +
    geom_vline(xintercept=data.frame(o3$log)[nrow(o3$log),"par1"], color="darkRed") +
    geom_hline(yintercept=data.frame(o3$log)[nrow(o3$log),"par2"], color="darkRed") +
    geom_segment(aes(xend=c(tail(par1, n=-1), NA), yend=c(tail(par2, n=-1), NA)),
                  arrow=arrow(length=unit(0.5,"cm"), type = "open", angle=30), size=.7,
                 color="darkBlue") +
    geom_point(data=head(data.frame(o3$log),1), size =2, color="darkBlue")+
    scale_color_gradientn(colours=col)+
    scale_x_continuous(expand = c(0,0), limits=range(grid$par1), breaks=seq(0,10)) +
    scale_y_continuous(expand = c(0,0), limits=range(grid$par2)) +
    xlab(expression(mu)) + ylab(expression(sigma))+
    theme(legend.position="none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()


o5 <- optimParallel(par = c(1, 1), fn = negll, x = x, lower = c(-Inf, 0.0001),
                    parallel = list(loginfo = TRUE, forward=TRUE))
o5$loginfo[17:19, ]
tail(o5$loginfo, n = 3)


o6 <- optimParallel(par = c(1, 1), fn = negll, x = x, lower = c(-Inf, 0.0001),
                    parallel = list(loginfo = TRUE, forward=TRUE),
                    control = list(factr=1e-6/.Machine$double.eps))
tail(o6$loginfo, n = 3)




## benchmark example
library(microbenchmark)
measure <- function(expr, times=50, unit="s"){
    m <- microbenchmark(list=expr, times=times)
    summary(m, unit="s")["mean"]
}
time_negll <- measure(expression(negll(par = c(1, 1), x=x)))
time_optimParallel <- measure(expression(
    out <<- optimParallel(par=c(1,1), fn=negll, gr=NULL, x=x,
                          control=list(maxit=20), lower=c(-Inf, .001))))/out$counts[1]
time_optim <- measure(expression(
    out <<- optim(par=c(1,1), fn=negll, gr=NULL, x=x,
                  method="L-BFGS-B",
                  control=list(maxit=20), lower=c(-Inf, .001))))/out$counts[1]

round(time_negll, 3)
round(time_optim, 3)
round(time_negll*5, 3)
round(time_optimParallel, 3)
round(100*(1 - time_optimParallel / time_optim), 3)



demo_generator <- function(fn, gr) {
    par_last <- value <- grad <- NA
    eval <- function(par) {
        if(!identical(par, par_last)) {
            message("--> evaluate fn() and gr()")
            par_last <<- par
            value <<- fn(par)
            grad <<- gr(par)
        } else
            message("--> read stored value")
    }
    f <- function(par) {
        eval(par = par)
        value
    }
    g <- function(par) {
        eval(par = par)
        grad
    }
    list(fn = f, gr = g)
}
demo <- demo_generator(fn = sum, gr = prod)
demo$fn(1:5)
demo$gr(1:5)



## benchmark
fn <- function(par, sleep){
    Sys.sleep(sleep)
    sum(par^2)
}

gr <- function(par, sleep){
    Sys.sleep(sleep)
    2*par
}



## make Figure 2
PAR <- 100
FN <- function(par, sleep){
    Sys.sleep(sleep)
    sum(par^2)
}
GR <- function(par, sleep){
    Sys.sleep(sleep)
    2*par
}

CONTROL <- list(maxit=10, factr=.Machine$double.eps)
METHOD <- "L-BFGS-B"
grid <- expand.grid(p=1:3,Tf=c(0,.05,.2,.4,.6,.8,1),
                    parallel=c("optimParallel","optim"), gr=FALSE, To=NA)
grid <- rbind(grid,
              data.frame(p=3, Tf=unique(grid$Tf), parallel="optimParallel", gr=TRUE, To=NA))
grid <- rbind(grid,
              data.frame(p=3, Tf=unique(grid$Tf), parallel="optim", gr=TRUE, To=NA))
cl <- makeCluster(8); setDefaultCluster(cl=cl)

for(i in 1:nrow(grid)){
    par <- rep(PAR, grid[i,"p"])
    if(grid[i,"parallel"]=="optimParallel"){
        if(grid[i,"gr"]){
            total <- measure(
                expression(out <<- optimParallel(par=par, fn=FN, gr=GR, sleep=grid[i,"Tf"],
                                                 control=CONTROL)))
        } else {
            total <- measure(
                expression(out <<- optimParallel(par=par, fn=FN, gr=NULL, sleep=grid[i,"Tf"],
                                                 control=CONTROL)))
        }
        grid[i, "To"] <- total/out$counts[1]
    } else {
        if(grid[i,"gr"]){
            total <- measure(
                expression(out <<- optim(par=par, fn=FN, gr=GR, sleep=grid[i,"Tf"],
                                         method=METHOD,
                                         control=CONTROL)))
        }else{
            total <- measure(
                expression(out <<- optim(par=par, fn=FN, gr=NULL, sleep=grid[i,"Tf"],
                                         method=METHOD,
                                         control=CONTROL)))
            }
        grid[i, "To"] <- total/out$counts[1]
    }
    print(grid[i,])
    print(out$counts[1])
}
save(grid, file="benchmark.RData")

pdf("benchmark.pdf", width = 8*.9*.9, height = 5*.8*.9)
size <- .8
sizes <- 4
stroke <- 1.1
shape <- 1
grid$par <- as.factor(grid$p)
grid$Tolog <- log(grid$To)
grid$parallel <- factor(grid$parallel)
#grid$parallel <- factor(grid$parallel,levels(grid$parallel)[c(2,1)])
grid$pa <- factor(ifelse(grid$gr, paste0("p = ", grid$par, ", gradient"), paste("p =", grid$par)))
grid$pa <- factor(grid$pa,levels(grid$pa)[c(3,2,1,4)])
ggplot(grid, aes(x=Tf, y=To, color=pa, group=interaction(pa,parallel),
                 linetype=parallel)) + geom_point(size=sizes, shape=shape, stroke=stroke) + geom_line(size=size) +
    geom_point(data=grid[grid$parallel=="optimParallel",], mapping=aes(x=Tf, y=To), color="black", size=sizes, shape=shape, stroke=stroke) +
    geom_line(data=grid[grid$parallel=="optimParallel"&grid$par==1,], mapping=aes(x=Tf, y=To),
              color="black", size=size) +
    xlab("Elapsed time [s] for one evalutation of fn(x)") +
    ylab("Elapsed time [s] per iteration") +
    theme(legend.title=element_blank()) +
    scale_x_continuous(minor_breaks = seq(0 , 1, .1), breaks = seq(0, 1, .2))
dev.off()
