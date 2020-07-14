## ----setup, include=TRUE, echo=FALSE, results='hide', message = FALSE, warning = FALSE, cache=FALSE--------------------------------
options(width = 70, digits = 2)


## ----------------------------------------------------------------------------------------------------------------------------------
for( i in 1:6){
  print(i)
}


## ----------------------------------------------------------------------------------------------------------------------------------
head(mtcars)

for(i in 1:6 ){
  print(mtcars[i, "hp"] + 20)
}


## ----------------------------------------------------------------------------------------------------------------------------------
x <- seq(from = 1,
         to = 10000000,
         by = 1)

forTime <- proc.time() #stores our start time
xCube <- NA_integer_ #creates an empty integer variable

for(i in 1:length(x)) {
  xCube[i] = x[i] ^ 3  #stores the cube of x at position i in xCube at position i
}

forTime <- proc.time() - forTime #stop time less start time is time taken
forTime #shows total time taken

head(x)
head(xCube)


## ----------------------------------------------------------------------------------------------------------------------------------
rm(xCube) #removes the old xCube from memory.

forTime2<- proc.time()
xCube <- vector(mode = "numeric", length = length(x))

for (i in 1:length(x) ) {
  xCube[i] = x[i]^3
}

forTime2 <- proc.time()-forTime2
forTime2


## ----------------------------------------------------------------------------------------------------------------------------------
xCube <- NULL
vTime <- proc.time()
xCube <- x^3
vTime <- proc.time()-vTime
vTime


## ----------------------------------------------------------------------------------------------------------------------------------
set.seed(1234) #this sets the random number start to 1234.

for (i in 1:5 ) {
  x <- rnorm(5,4,2)
  print(x)
  print(paste0("Xbar: ",mean(x)))
  print(paste0("StdDev: ",sd(x)))
}


## ----------------------------------------------------------------------------------------------------------------------------------
set.seed(1234) #allows duplication of results

y <- 8 #mean(y) < 8.1 returns TRUE
i <- 0 #initialise counter and set to zero

while (mean(y) < 8.1) { 
  i <- i + 1 #increase counter by 1 each time
  y <- rnorm(5, mean = 4.0, sd = 2) #store random sample from population
}

print(y) #observe sample
mean(y) #confirm mean(y) >= 8.1


## ----logic-boxplot, fig.width=6, fig.height=3, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "box and whisker plot showing our atypical sample."----
if( mean(y) > 8.1 ) {
  
  boxplot(y,
          main = "This sample has an atypically large mean.",
          horizontal = TRUE)
  
} else{
  
  plot(y,
       main = "This sample mean is not so very large.")
  
}


## ----------------------------------------------------------------------------------------------------------------------------------


set.seed(1234) #allows duplication of results

y <- 8 #mean(y) < 8.1 returns TRUE

while (mean(y) < 8.1) { 
  y <- rnorm(5, mean = 4.0, sd = 2) #store random sample from population
  
  if ( mean(y) < 8.1 ) { next() }
  print(y)
}



## ----------------------------------------------------------------------------------------------------------------------------------
set.seed(1234) #allows duplication of results

z <- NULL

repeat{
  z <- rnorm(5, mean = 4.0, sd = 2) #store random sample from population
  if (mean(z) >= 8.1) { break }
}

print(z)


## ----------------------------------------------------------------------------------------------------------------------------------
xL <- 1:10
lapply(xL, sqrt)


## ----------------------------------------------------------------------------------------------------------------------------------
sapply(xL, sqrt)


## ----------------------------------------------------------------------------------------------------------------------------------
vapply(xL, sqrt, FUN.VALUE = NA) #this has an error
vapply(xL, sqrt, FUN.VALUE = double(1)) 


## ----------------------------------------------------------------------------------------------------------------------------------
vapply(xL, sqrt, FUN.VALUE = double(2)) #this has an error


## ----------------------------------------------------------------------------------------------------------------------------------
lapply(iris, summary)


## ----------------------------------------------------------------------------------------------------------------------------------
vapply(iris, summary, c(Min. = 0, `1st Qu.` = 0, Median = 0,
                        Mean = 0, `3rd Qu.` = 0, Max. = 0))
vapply(iris[1:4], summary, c(Min. = 0, `1st Qu.` = 0, Median = 0,
                             Mean = 0, `3rd Qu.` = 0, Max. = 0))


## ----------------------------------------------------------------------------------------------------------------------------------
head(mtcars)
unique(mtcars$cyl)


## ----mtcars-plot, fig.width=6, fig.height=3, out.width='1\\linewidth', fig.pos="!ht", fig.cap = "visualising the data of cyl vs mpg."----
plot(mtcars$cyl, mtcars$mpg)


## ----------------------------------------------------------------------------------------------------------------------------------
tapply(
  X = mtcars$mpg,
  INDEX = mtcars$cyl,
  FUN = mean)


## ----------------------------------------------------------------------------------------------------------------------------------
apply(mtcars, MARGIN = 2, FUN = sd)


## ----------------------------------------------------------------------------------------------------------------------------------
cor.test(mtcars$mpg, mtcars$cyl)


## ----------------------------------------------------------------------------------------------------------------------------------
mapply(cor.test,
       mtcars[, c("mpg", "hp")],
       mtcars[, c("cyl", "disp")],
       MoreArgs = list(method = "pearson", alternative = "two.sided"))

