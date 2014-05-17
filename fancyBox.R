.addpts <- function(x, y, s, col,...){
  points(rnorm(length(y), x, s), y, col = col,...)
#  points(rnorm(length(y), x, s), y, col = col,...)
}
.addstats <- function(x, y, d, col = rgb(.2, .2, .2, .85)){
  q <- quantile(y, probs = c(.25, .5, .75), na.rm = TRUE)
  segments(x0=c(x-d, x-d*.75, x-d, x),
           y0=c(q[1], q[2], q[3], q[1]),
           x1=c(x+d, x+d*.75, x+d, x),
           y1=c(q[1], q[2], q[3], q[3]), col = col, lwd=3)
}
.addlines <- function(x, y, l, col){
  if(length(y)>1){
    d <- density(y, na.rm = TRUE)
    dy <- d$y/max(d$y)*1/l
    x1 <- x+dy
    x2 <- x-dy
    lines(x1, d$x, col = col, lwd=4)
    lines(x2, d$x, col = col, lwd=4)
  }
}
addCols <- function(n, a){
  rgb(seq(0, 1, len = n), .2, seq(1, 0, len = n), a)
}
fancyBox <- function(X, Class = NULL, l = 3, addPts = TRUE, addStat = TRUE,
                     boxcol = {n = ifelse(is.null(Class), ncol(X), nlevels(as.factor(Class)))
                               addCols(n, 1)},
                     pcol = {n = ifelse(is.null(Class), ncol(X), nlevels(as.factor(Class)))
                             addCols(n, .5)},
                     ylim = range(X, na.rm = TRUE)*1.25, ...){
  # X: a matrix (or data frame) of values, in columns
  # l: a coefficient to adjust the box widths. Increasing this value will reduce the width.
  # addPts: if TRUE (default) display the points
  # addStat: if TRUE (default) display the quantiles (q25, q50, q75)
  # boxcol, pcol: vectors of colors for boxes and points, resp.

  # cex = 2/log(length(y))
  
  if(is.null(Class)){
    boxplot(X, border = NA, ylim = ylim,...)
    n <- ncol(X)
    if(addPts)
      for(i in 1:n) .addpts(i, X[,i], .025, pcol, ...) #pcol[i],...)
    if(addStat)
      for(i in 1:n) .addstats(i, X[,i], 1/l*.75)
    for(i in 1:n) .addlines(i, X[,i], l, col = boxcol[i])
  }
  else{
    Class <- as.factor(Class)
    boxplot(X ~ Class, border = NA, ylim = ylim,...)
    n <- nlevels(Class)
    if(addPts)
      for(i in 1:n) .addpts(i, X[Class == levels(Class)[i]], .1, pcol[i])
    if(addStat)
      for(i in 1:n) .addstats(i, X[Class == levels(Class)[i]], 1/l,...)
    for(i in 1:n) .addlines(i, X[Class == levels(Class)[i]], l, col = boxcol[i], ...)    
  }
}
