[Example]: https://github.com/fredcommo/fancyBoxes/blob/master/fancyBoxplot.png
# Fancy boxes, a nice way to display your data...

### [Example]

### Show what boxplots do not show...


```
# If not installed yet
install.packages("devtools")
require("devtools")
# a nice package by Brian Bot
install_github("rGithubClient", "brian-bot") 
```

```
###############################
# fancyBox DEMO
###############################

require("rGithubClient")
git <- getRepo('fredcommo/fancyBoxes')
sourceRepoFile(git, "fancyBox.R")

# Simulate data
m <- seq(-3, 3, len = 100)
s <- seq(.2, 2, len = 100)
n <- 10; p = 1000
X <- lapply(1:n, function(i){
  x <- lapply(1:2, function(i) rnorm(p/2, sample(m, 1), sample(s, 1)))
  return(do.call(c, x))  
})
X <- do.call(cbind, X)

op <- par(no.readonly = TRUE)

par(mfrow = c(2, 2))
boxplot(X, main = "Original boxplot")
fancyBox(X, lwd = 3, addStat=FALSE, addPts=FALSE,
         xlab = "Some x label", ylab = "Some y label", main = "My fancy boxes\nJust the lines", las = 1)
fancyBox(X, lwd = 3, addStat=FALSE, addPts=TRUE,
         xlab = "Some x label", ylab = "Some y label", main = "My fancy boxes\nAdding the points", las = 1)
fancyBox(X, lwd = 3, xlab = "Some x label", ylab = "Some y label", main = "My fancy boxes\nAdding the quantiles", las = 1)
legend("topleft", legend = "quantiles", lwd = 3, bty = "n")
par(op)

# Using Classes
n = 10; p = 1000
Class <- rep(LETTERS[1:n], each = p/n)
X <- lapply(1:n, function(i) rnorm(p/n, sample(m, 1), sample(s, 1)))
X <- do.call(c, X)
par(mfrow = c(1, 2))
boxplot(X ~ Class, main = "Original boxplot")
fancyBox(X, Class, lwd = 3,
         xlab = "Some x label", ylab = "Some y label", main = "My fancy boxes", las = 1)
legend("topleft", legend = "quantiles", lwd = 3, bty = "n")
par(op)
```
