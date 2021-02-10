## create files for blog entry
## https://florarblog.wordpress.com/2018/08/07/optimparallel-speed-up-your-optim-calls/

rm(list=ls())
set.seed(11)
library(optimParallel)

x <- rnorm(n = 1000, mean = 5, sd = 2)
negll <- function(par, x) -sum(dnorm(x = x, mean = par[1], sd = par[2], log = TRUE))
o1 <- optim(par = c(1, 1), fn = negll, x = x, method = "L-BFGS-B",
            lower = c(-Inf, 0.0001))
o1$par

cl <- makeCluster(detectCores()); setDefaultCluster(cl = cl)
o2 <- optimParallel(par = c(1, 1), fn = negll, x = x, method = "L-BFGS-B",
                    lower = c(-Inf, 0.0001))
identical(o1, o2)

o3 <- optimParallel(par = c(1, 1), fn = negll, x = x, method = "L-BFGS-B",
                    lower = c(-Inf, 0.0001), parallel=list(loginfo = TRUE))
print(o3$loginfo[1:3, ], digits = 3)

## make Figure 1
library(ggplot2); theme_set(theme_bw())
library(fields)
col2 <- tim.colors(nrow(o3$log)-2)
col2 <- rep("darkred", 100)
for(j in 1:(nrow(o3$log)-2)){
        grid <- expand.grid(par1=seq(.7, 7, length.out=100),
                            par2=seq(.7, 4, length.out=100),
                            z=NA)
        for(i in 1:nrow(grid))
            grid$z[i] <- negll(c(grid$par1[i], grid$par2[i]), x=x)
        col <- alpha(colorRampPalette(c("gray"))(55), 1)
        stroke <- 1.1
        shape <- 1
        p <- ggplot(data=data.frame(o3$log[1:j,]), aes(x=par1, y=par2)) +
        geom_contour(mapping=aes(x=par1, y=par2, z=z, color=..level..), data=grid,
                     bins=55) +
        geom_vline(xintercept=data.frame(o3$log)[j,"par1"], color=col2[j]) +
        geom_hline(yintercept=data.frame(o3$log)[j,"par2"], color=col2[j]) +
        geom_point(data=head(data.frame(o3$log),j-1), size =3, shape=3, color="darkred", alpha=1, stroke=1)+
        scale_color_gradientn(colours=col)+
        scale_x_continuous(expand = c(0,0), limits=range(grid$par1)) +
        scale_y_continuous(expand = c(0,0), limits=range(grid$par2)) +
        xlab(expression(mu)) + ylab(expression(sigma))+
         annotate("text", x = 6.17, y = 3.8, label = paste0("iteration:  "), size=8)  +     annotate("text", x = 6.9, y = 3.8, label = paste(j-1), size=8, hjust=1)+
                     annotate("text", x = 1, y = .85, label = paste0("start"), size=8) +
        theme(legend.position="none", axis.text=element_text(size=15),
              axis.title=element_text(size=16),
              plot.title = element_text(hjust = 0.5, size=20, face="bold"),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
            ggtitle("Optimization path")


        if(j>1)
        p <- p+geom_segment(aes(xend=c(tail(par1, n=-1), NA), yend=c(tail(par2, n=-1), NA)),
                     arrow=arrow(length=unit(0.5,"cm"), type = "open", angle=30), size=.7,
                     color=col2[j], data=data.frame(o3$log[(j-1):j,])) +
            geom_segment(aes(xend=c(tail(par1, n=-1), NA), yend=c(tail(par2, n=-1), NA)), size=.7,
                     color=col2[1:j])
   
        ggsave(paste0("path_",sprintf("%02d", j),".png"), p,
               width = 8*.9*.9*1.3, height = .8*8*.9, device="png", dpi=200)
}

        grid <- expand.grid(par1=seq(.7, 7, length.out=100),
                            par2=seq(.7, 4, length.out=100),
                            z=NA)
        for(i in 1:nrow(grid))
            grid$z[i] <- negll(c(grid$par1[i], grid$par2[i]), x=x)
        col <- alpha(colorRampPalette(c("gray"))(55), 1)
        stroke <- 1.1
        shape <- 1
        p <- ggplot(data=data.frame(o3$log[1:j,]), aes(x=par1, y=par2)) +
        geom_contour(mapping=aes(x=par1, y=par2, z=z, color=..level..), data=grid,
                     bins=55) +
        geom_vline(xintercept=data.frame(o3$log)[j,"par1"], color=col2[j]) +
        geom_hline(yintercept=data.frame(o3$log)[j,"par2"], color=col2[j]) +
        geom_point(data=head(data.frame(o3$log),j-1), size =3, shape=3, color="darkred", alpha=1, stroke=1)+
        scale_color_gradientn(colours=col)+
        scale_x_continuous(expand = c(0,0), limits=range(grid$par1)) +
        scale_y_continuous(expand = c(0,0), limits=range(grid$par2)) +
        xlab(expression(mu)) + ylab(expression(sigma))+
             annotate("text", x = 6.17, y = 3.8, label = paste0("iteration:  "), size=8)  +     annotate("text", x = 6.9, y = 3.8, label = paste(j-1), size=8, hjust=1)+
                     annotate("text", x = 1, y = .85, label = paste0("start"), size=8) +
  theme(legend.position="none", axis.text=element_text(size=15),
              axis.title=element_text(size=16),
              plot.title = element_text(hjust = 0.5, size=20, face="bold"),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +ggtitle("Optimization path")


        if(j>1)
        p <- p+
            geom_segment(aes(xend=c(tail(par1, n=-1), NA), yend=c(tail(par2, n=-1), NA)), size=.7,
                     color=col2[1:j])



for(k in (nrow(o3$log)):(nrow(o3$log)+2))
    ggsave(paste0("path_",sprintf("%02d", k),".png"), p,
           width = 8*.9*.9*1.3, height = .8*8*.9, device="png", dpi=200)

system("convert -delay 100 path_*.png test.gif")

