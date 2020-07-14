## ----setup, include=TRUE, results='hide', message = FALSE, warning = FALSE, cache=FALSE--------------------------------------------
library(stringdist)
library(data.table)
library(foreign)

options(stringsAsFactors = FALSE,
        datatable.print.nrows = 20, ## if over 20 rows
        datatable.print.topn = 3, ## print first and last 3
        digits = 2) ## reduce digits printed by R


## ----------------------------------------------------------------------------------------------------------------------------------
d <- read.dta("data/ch08/ICPSR_04691/DS0001/04691-0001-Data.dta")
d <- as.data.table(d)
setkey(d, IDNUMR)


## ----------------------------------------------------------------------------------------------------------------------------------
str(d, give.attr = FALSE, strict.width = "cut", list.len = 20)


## ----------------------------------------------------------------------------------------------------------------------------------
table(d[, EDUCATIO])
table(d[, PLANGUAG])
table(d[, BMICLASS])
table(d[, S1Q01])


## ----------------------------------------------------------------------------------------------------------------------------------
grep(
  pattern = "abc",
  x = c("a", "abc", "def", "abc", "d"))

grepl(
  pattern = "abc",
  x = c("a", "abc", "def", "abc", "d"))


## ----------------------------------------------------------------------------------------------------------------------------------
grep(
  pattern = "REFUSED",
  x = c(
    "1 - REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - REFUSED"),
  value = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------------
grep(
  pattern = "- REFUSED",
  x = c(
    "1 - REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - REFUSED"),
  value = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------------
grep( pattern = "- REFUSED$",
  x = c(
    "1 - REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - REFUSED"),
  value = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------------
grep( pattern = "[0-9]+ - REFUSED$",
  x = c(
    "1 - TREATMENT REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - JACK - REFUSED",
    "4 - REFUSED",
    "97 - REFUSED",
    "- REFUSED"),
  value = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------------
grep( pattern = "^[-]*[0-9]+ - REFUSED$",
  x = c(
    "1 - TREATMENT REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - JACK - REFUSED",
    "4 - REFUSED",
    "97 - REFUSED",
    "-97 - REFUSED",
    "- REFUSED",
    "TRICK CASE 4 - REFUSED"),
  value = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------------
grep(
  pattern = "^[-]*[0-9]+ - REFUSED$|MISSING$",
  x = c(
    "1 - TREATMENT REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - JACK - REFUSED",
    "4 - REFUSED",
    "97 - REFUSED",
    "-97 - REFUSED",
    "-2 - MISSING",
    "- REFUSED",
    "TRICK CASE 4 - REFUSED",
    "4 - REFUSED HELP"),
  value = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------------
p <- "^[-]*[0-9]+ - REFUSED$|MISSING$|DON'T KNOW$|LEGITIMATE SKIP$|PARTIAL INTERVIEW$|NOT IN UNIVERSE$"

grep( pattern = p,
  x = c(
    "1 - TREATMENT REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - JACK - REFUSED",
    "4 - REFUSED",
    "97 - REFUSED",
    "-97 - REFUSED",
    "-2 - MISSING",
    "96 - DON'T KNOW",
    "-4 - LEGITIMATE SKIP",
    "-3 - PARTIAL INTERVIEW",
    "-2 - NOT IN UNIVERSE",
    "- REFUSED",
    "TRICK CASE 4 - REFUSED",
    "4 - PARTIAL INTERVIEW OF DOCTOR"),
  value = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------------
v <- c("EDUCATIO", "PLANGUAG", "BMICLASS", "S1Q01", "S2Q01")
d[, (v) := lapply(.SD, function(x) {
  x[grepl(pattern = p, x)] <- NA
  if (is.factor(x)) droplevels(x) else x
}), .SDcols = v]


table(d[, EDUCATIO])
table(d[, S1Q01])


## ----------------------------------------------------------------------------------------------------------------------------------
gsub(
  pattern = "abc",
  replacement = "",
  x = c("a", "abcd", "123abc456"))


## ----------------------------------------------------------------------------------------------------------------------------------
p.remove <- "^[-]*[0-9]+ - "
gsub(
  pattern = p.remove,
  replacement = "",
  x = c(
    "1 - TREATMENT REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - JACK - REFUSED",
    "4 - REFUSED",
    "97 - REFUSED",
    "-97 - REFUSED",
    "-2 - MISSING",
    "96 - DON'T KNOW",
    "-4 - LEGITIMATE SKIP",
    "-3 - PARTIAL INTERVIEW",
    "-2 - NOT IN UNIVERSE",
    "- REFUSED",
    "TRICK CASE 4 - REFUSED",
    "4 - PARTIAL INTERVIEW OF DOCTOR"))


## ----------------------------------------------------------------------------------------------------------------------------------
d <- read.dta("data/ch08/ICPSR_04691/DS0001/04691-0001-Data.dta")
d <- as.data.table(d)
setkey(d, IDNUMR)


## ----------------------------------------------------------------------------------------------------------------------------------
d[, (v) := lapply(.SD, function(x) {
  f <- is.factor(x)
  x[grepl(pattern = p, x)] <- NA
  x <- gsub(pattern = p.remove, replacement = "", x)
  if (f) factor(x) else x
}), .SDcols = v]

table(d[, EDUCATIO])
table(d[, S1Q01])
table(d[, S2Q01])


## ----------------------------------------------------------------------------------------------------------------------------------
table(d[!S2Q02R %between% c(0, 90), S2Q02R])
table(d[!S2Q03R %between% c(0, 900), S2Q03R])


## ----adt-histS2Q02Ra, fig.width=4, fig.height=4, out.width='.5\\linewidth',fig.pos="!ht", fig.cap = c("Outliers that are near >900 and >90")----
par(mfrow = c(1,2))
hist(d$S2Q02R)
hist(d$S2Q03R)


## ----------------------------------------------------------------------------------------------------------------------------------
v2 <- c("S2Q02R", "S2Q03R", "AGEYR_CH")
m <- sort(c(9, 99, 999))

for (k in v2) {
  j <- i <- 1
  while(j == 1 & i <= length(m)) {
    if (max(d[[k]], na.rm = TRUE) < m[i]) {
      j <- 0
      d[!(get(k) %between% c(0, ifelse(m[i] > 90,
        m[i] - 9, m[i] - 1e-9))), (k) := NA_integer_]
    } else {
      i <- i + 1
    }
  }
}


## ----adt-histS2Q02Rb, fig.width=4, fig.height=4, out.width='.5\\linewidth',fig.pos="!ht", fig.cap = c("The outliers are gone because histograms ignore missing values")----
par(mfrow = c(1,2))
hist(d$S2Q02R)
hist(d$S2Q03R)


## ----------------------------------------------------------------------------------------------------------------------------------
v.health <- paste0("S2Q", c(19, 20, 21, 22, 23, 24, 26, 35, 37))
v.health
table(unlist(d[, v.health, with = FALSE]))


## ----------------------------------------------------------------------------------------------------------------------------------
p <- "^[-]*[0-9]+ - REFUSED$|MISSING$|DON'T KNOW$|LEGITIMATE SKIP$|PARTIAL INTERVIEW$|NOT IN UNIVERSE$"

d[, (v.health) := lapply(.SD, function(x) {
  x[grepl(pattern = p, x)] <- NA
  if (is.factor(x)) droplevels(x) else x
}), .SDcols = v.health]


table(unlist(d[, v.health, with = FALSE]), useNA = "ifany")


## ----------------------------------------------------------------------------------------------------------------------------------
d[, HealthConditions := rowMeans(d[, v.health, with = FALSE] == "1 - YES", na.rm = TRUE)]

table(round(d$HealthConditions, 2),
      useNA = 'ifany')


## ----------------------------------------------------------------------------------------------------------------------------------
d[, NHealthConditions := rowSums(d[, v.health, with = FALSE] == "1 - YES", na.rm = TRUE)]

table(d$NHealthConditions, useNA = 'ifany')


## ----------------------------------------------------------------------------------------------------------------------------------
d[, NMissHealthConditions := rowSums(is.na(d[, v.health, with = FALSE]))]

d[NMissHealthConditions == length(v.health), NHealthConditions := NA_integer_]

table(d$NHealthConditions, useNA = 'ifany')


## ----------------------------------------------------------------------------------------------------------------------------------
Reduce(`+`, c(1, 2, 3))
Reduce(`+`, list(1:3, 4:6, 7:9))
Reduce(`*`, list(1:3, 4:6, 7:9))
Reduce(`^`, list(1:3, 4:6, 3:1))
Reduce(`-`, list(1:3, 4:6, 7:9))
Reduce(`/`, list(1:3, 4:6, 7:9))


## ----------------------------------------------------------------------------------------------------------------------------------
fplus <- function(e1, e2) {
  if (is.factor(e1)) {
    e1 <- as.numeric(e1) - 1
  }
  if (is.factor(e2)) {
    e2 <- as.numeric(e2) - 1
  }
  e1 + e2
}


## ----------------------------------------------------------------------------------------------------------------------------------
d[, NHealthConditions2 := Reduce(fplus, .SD), .SDcols = v.health]

table(d$NHealthConditions2)


## ----------------------------------------------------------------------------------------------------------------------------------
reference.Names <- c("This Test", "Test Thas",
  "Jane Mary", "Jack Dun-Dee")
observed.Names <- c("this test", "test this", "test that",
  "JaNe  Mary.", "Mary Sou", "Jack Dee", "Jane Jack")


## ----------------------------------------------------------------------------------------------------------------------------------
stringdistmatrix(reference.Names,
                 observed.Names,
                 method = "dl")


## ----------------------------------------------------------------------------------------------------------------------------------
amatch(observed.Names,
       reference.Names,
       method = "dl", maxDist = 4)


## ----------------------------------------------------------------------------------------------------------------------------------
stringdistmatrix(
  tolower(reference.Names),
  tolower(observed.Names),
  method = "dl")

stringdistmatrix(
  tolower(gsub("\\.", "", reference.Names)),
  tolower(gsub("\\.", "", observed.Names)), method = "dl")


## ----------------------------------------------------------------------------------------------------------------------------------
strsplit(
  x = reference.Names[1],
  split = "\\s")[[1]]

strsplit(
  x = observed.Names[2],
  split = "\\s")[[1]]


## ----------------------------------------------------------------------------------------------------------------------------------
stringdistmatrix(
  strsplit(
    x = reference.Names[1],
    split = "\\s")[[1]],
  strsplit(
    x = observed.Names[2],
    split = "\\s")[[1]],
  method = "dl")


## ----------------------------------------------------------------------------------------------------------------------------------
stringdistmatrix(
  strsplit(
    x = tolower(reference.Names[1]),
    split = "\\s")[[1]],
  strsplit(
    x = tolower(observed.Names[2]),
    split = "\\s")[[1]],
  method = "dl")


## ----------------------------------------------------------------------------------------------------------------------------------
sum(apply(stringdistmatrix(
  strsplit(
    x = tolower(reference.Names[1]),
    split = "\\s")[[1]],
  strsplit(
    x = tolower(observed.Names[2]),
    split = "\\s")[[1]],
  method = "dl"), 1, min))


## ----------------------------------------------------------------------------------------------------------------------------------
matchIt <- function(index,
                    pool,
                    ignore = c("\\.", "-"),
                    split = FALSE,
                    ignore.case = TRUE,
                    fuzz = .05,
                    method = "dl") {
  # for those things we want to ignore, drop them
  # e.g., remove spaces, periods, dashes
  rawpool <- pool
  
  for (v in ignore) {
    index <- gsub(v, "", index)
    pool <- gsub(v, "", pool)
  }
  # if ignore case, convert to lower case
  if (ignore.case) {
    index <- tolower(index)
    pool <- tolower(pool)
  }
  
  if (!identical(split, FALSE)) {
    index2 <- strsplit(index, split = split)[[1]]
    index2 <- index2[nzchar(index2)]
    pool2 <- strsplit(pool, split = split)
    pool2 <- lapply(pool2, function(x)
      x[nzchar(x)])
    
    # calculate distances defaults to the Damerau-Levenshtein distance
    distances <- sapply(pool2, function(x) {
      sum(apply(
        stringdistmatrix(index2, x, method = method),
        1,
        min,
        na.rm = TRUE
      ))
    })
  } else {
    # calculate distances defaults to the Damerau-Levenshtein distance
    distances <- stringdist(index, pool, method = method)
  }
  
  # some methods result in Infinity answers, set these missing
  distances[!is.finite(distances)] <- NA
  
  # get best and worst
  best <- min(distances, na.rm = TRUE)
  worst <- max(distances, na.rm = TRUE)
  
  # if fuzz, grab all distances that are within fuzz percent of
  # the difference between best and worst, tries to capture
  # potentially very close matches to the best
  if (fuzz) {
    usedex <- which(distances <= (best + ((worst - best) * fuzz)))
  } else {
    usedex <- which(distances == best)
  }
  
  # define a distance below which its considered a
  # perfect or exact match
  perfect <- distances[usedex] < .01
  out <- rawpool[usedex]
  
  # count the number of perfect matches
  count <- sum(perfect)
  
  if (any(perfect)) {
    # if there are perfect matches, just use one
    match <- out[perfect][1]
    
    # return a data table of the perfect match
    # and number of perfect matches (probably 1)
    data.table(Match = match,
               N = count,
               Written = NA_character_)
  } else {
    # if no perfect matches,
    # return a list of close matches, comma separated
    # also return what exactly was written
    data.table(
      Match = paste(out, collapse = ", "),
      N = count,
      Written = index
    )
  }
}


## ----------------------------------------------------------------------------------------------------------------------------------
output <- lapply(observed.Names, function(n) {
  matchIt(
    index = n,
    pool = reference.Names,
    ignore = c("\\.", "-"),
    split = "\\s",
    fuzz = .05)
})

output <- do.call(rbind, output)

output


## ----------------------------------------------------------------------------------------------------------------------------------
finaloutput <- output[, .(
  N = sum(N),
  Uncertain = paste(na.omit(Written), collapse = ", ")),
  by = Match]


finaloutput

