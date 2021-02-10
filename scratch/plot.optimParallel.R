plot(1:100, col=colorRampPalette(c("#332288", "#88CCEE", "#44AA99",
                "#CC6677", "#AA4499", "#999933", "#117733"))(100),
     pch=19, cex=6)
plot(1:8, col=colorRampPalette(c("#332288", "#44AA99", "#88CCEE", 
                                 "#AA4499", "#882255", "#999933", "#117733"))(8),
     pch=19, cex=6)


plot.optimParallel <- function(oo, what=c("par", "gradient"), legendPosition="topleft", legendText=NULL, lwd=1.5, main="", ...){
    if(is.null(oo$loginfo))
        stop("Log information is missing. Call 'optimParallel()' with argument 'parallel=list(loginfo=TRUE)' to generate the necessary log information.")
        
    opar <- par("xpd", "mai")
    on.exit(par(opar))
    par(mai=c(.8,1,.8,1))
    what <- match.arg(what)
    if(what=="par"){
        indexPar <- 1:(ncol(oo$loginfo)/2-1)+1
        ylab <- "pars"
    } else {
        indexPar <- 1:(ncol(oo$loginfo)/2-1)+1+ncol(oo$loginfo)/2
        ylab <- "gradients"
    }
    nPar <- length(indexPar)
    indexFn <- (ncol(oo$loginfo)/2)+1
    colPar <- colorRampPalette(c("#332288", "#44AA99", "#88CCEE", 
                                 "#AA4499", "#882255", "#999933", "#117733"))(length(indexPar))
    colFn <- "black"

    if(is.null(legendText))
        legendText <- colnames(oo$loginfo)[c(indexPar, indexFn)]
    else if(length(legendText) != nPar+1)
        stop("argument 'legendText' has to be of length #parameters + 1.")
    matplot(oo$loginfo[,indexPar], type="l", lty=1, yaxt="n", col=colPar,
            xlab="", ylab=ylab, panel.first={grid(col="#8080cc", lty=1, lwd=.3)}, lwd=lwd, main=main, ...)
    mtext("step", side=1, line=2)
    axis(2, las=2)
    par(new=TRUE)
    plot(oo$loginfo[,1], oo$loginfo[,indexFn], xlab="", xaxt="n", yaxt="n", ylab="", type="l", col=colFn, lty=2, lwd=lwd, ...)
    axis(4, las=2, col=colFn)
    corners  <-  par("usr") # Gets the four corners of plot area (x1, x2, y1, y2)
    par(xpd = TRUE) # Draw outside plot area
    text(x = corners[2]*1.15, y = mean(corners[3:4]), "fn()", srt = 270)
    par(xpd = FALSE)
    legend(legendPosition, legend=legendText, col=c(colPar, colFn), lty=c(rep(1, nPar), 2) , bg="white", lwd=lwd, ...)
}
## check for global variables
codetools::findGlobals(plot.optimParallel, merge=FALSE)$variables
