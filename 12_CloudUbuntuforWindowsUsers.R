## ----setup, include=TRUE, results='hide', message = FALSE, warning = FALSE, cache=FALSE--------------------------------------------
options(width = 70, digits = 2)


## ----------------------------------------------------------------------------------------------------------------------------------
x <- 1:1000

for(i in 1:length(x)){
  x[i] <- x[i]^3
}
head(x)

