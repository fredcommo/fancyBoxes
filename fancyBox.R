.addpts <- function(X, pcol){
  for(i in 1:ncol(X)){
    n <- length(!is.na(X[,i]))
    points(rnorm(n, i, 1/ncol(X)), X[,i], cex = 1/ncol(X), col = pcol[i])
    }
}
.addstats <- function(X, d,...){
  col <- rgb(.2, .2, .2, .85)
  Q <- lapply(1:ncol(X), function(i) quantile(X[,i], probs = c(.25, .5, .75)))
  for(i in 1:length(Q)){
    q <- Q[[i]]
    segments(x0=c(i-d, i-d*.75, i-d, i),
             y0=c(q[1], q[2], q[3], q[1]),
             x1=c(i+d, i+d*.75, i+d, i),
             y1=c(q[1], q[2], q[3], q[3]), col = col, ...)
  }
}
.addlines <- function(i, x, l, col,...){
  d <- density(x, na.rm = TRUE)
  dy <- d$y/max(d$y)*1/l
  x1 <- i+dy
  x2 <- i-dy
  lines(x1, d$x, col = col,...)
  lines(x2, d$x, col = col,...)
}
fancyBox <- function(X, l = 3, addPts = TRUE, addStat = TRUE,
                    boxcol = rgb(seq(0, 1, len = ncol(X)), .2, seq(1, 0, len = ncol(X)), 1),
                    pcol = rgb(seq(0, 1, len = ncol(X)), .2, seq(1, 0, len = ncol(X)), 1/log10(nrow(X))),
                    ylim = range(X, na.rm = TRUE)*1.25, ...){
  # X: a matrix (or data frame) of values, in columns
  # l: a coefficient to adjust the box widths. Increasing this value will reduce the width.
  # addPts: if TRUE (default) display the points
  # addStat: if TRUE (default) display the quantiles (q25, q50, q75)
  # boxcol, pcol: vectors of colors for boxes and points, resp.

  boxplot(X, border = NA, ylim = ylim,...)
  if(addPts) .addpts(X, pcol)
  if(addStat) .addstats(X, 1/l,...)
  for(i in 1:ncol(X)) .addlines(i, X[,i], l, col = boxcol[i], ...)
}
