###############################
# fancyBox DEMO
###############################
getFilesList <- function(git, tag = ''){
  flist <- git@tree$path
  return(flist[grep(tag, flist)])
}

require("rGithubClient")
git <- getRepo('fredcommo/fancyBoxes')
Rlist <- getFilesList(git, '.R$')

# Simulate data
op <- par(no.readonly = TRUE)
m <- seq(-3, 3, len = 100)
s <- seq(.2, 2, len = 100)
n <- 10; p = 1000
X <- lapply(1:n, function(i){
  x <- lapply(1:2, function(i) rnorm(p/2, sample(m, 1), sample(s, 1)))
  return(do.call(c, x))  
})
X <- do.call(cbind, X)

par(mfrow = c(2, 2))
boxplot(X, main = "Original boxplot")
fancyBox(X, lwd = 3, addStat=FALSE, addPts=FALSE,
         xlab = "Some x label", ylab = "Some y label", main = "My fancy boxes\nJust the lines", las = 1)
fancyBox(X, lwd = 3, addStat=FALSE, addPts=TRUE,
         xlab = "Some x label", ylab = "Some y label", main = "My fancy boxes\nAdding the points", las = 1)
fancyBox(X, lwd = 3, xlab = "Some x label", ylab = "Some y label", main = "My fancy boxes\nAdding the quantiles", las = 1)
legend("topleft", legend = "quantiles", lwd = 3, bty = "n")
par(op)
