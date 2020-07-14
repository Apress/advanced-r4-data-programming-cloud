## ----setup, include=TRUE, results='hide', message = FALSE, warning = FALSE, cache=FALSE--------------------------------------------
options(width = 70, digits = 2)


## ----------------------------------------------------------------------------------------------------------------------------------
# ch02Packages <- c("data.table", "haven", "readxl", "writexl")
# install.packages(ch02Packages)
library(data.table)
library(haven)
library(readxl)
library(writexl)


## ----------------------------------------------------------------------------------------------------------------------------------
?'+'
help("+")


## ----------------------------------------------------------------------------------------------------------------------------------
Sys.Date()
Sys.time()
Sys.timezone()


## ----------------------------------------------------------------------------------------------------------------------------------
# getwd() #safe to run - machine dependent results

file.exists("data/ch02/ch02_text.txt") #meant to return TRUE
file.exists("NOSUCHFILE.FAKE") #meant to return FALSE


## ----------------------------------------------------------------------------------------------------------------------------------
file.access("data/ch02/ch02_text.txt", mode = 0)
file.access("data/ch02/ch02_text.txt", mode = 1)
file.access("data/ch02/ch02_text.txt", mode = 2)
file.access("data/ch02/ch02_text.txt", mode = 4)


## ----------------------------------------------------------------------------------------------------------------------------------
file.info("data/ch02/ch02_text.txt", "NOSUCHFILE.FAKE")


## ----------------------------------------------------------------------------------------------------------------------------------
ch02FileData <- file.info("data/ch02/ch02_text.txt", "NOSUCHFILE.FAKE")
is.data.frame(ch02FileData)
ch02FileData$size
is.numeric(ch02FileData$size)
ch02FileData$size > 5


## ----------------------------------------------------------------------------------------------------------------------------------
file.info("data/ch02/ch02_text.txt")

newTime<-Sys.time()-20
newTime
Sys.setFileTime("data/ch02/ch02_text.txt",newTime)
file.info("data/ch02/ch02_text.txt")


## ----------------------------------------------------------------------------------------------------------------------------------
file.create("ch02_created.docx", showWarnings = TRUE)
file.remove("ch02_created.docx")
file.remove("NOSUCHFILE.FAKE")


## ----------------------------------------------------------------------------------------------------------------------------------
Sys.time()
file.copy(
  "data/ch02/ch02_text.txt",
  "ch02_copy.txt",
  overwrite = TRUE,
  recursive = FALSE,
  copy.mode = TRUE,
  copy.date = TRUE
)
file.info("ch02_copy.txt")
file.rename("ch02_copy.txt", "ch02.txt")


## ----------------------------------------------------------------------------------------------------------------------------------
dir.create("output")
dir.exists("output")


## ----------------------------------------------------------------------------------------------------------------------------------
countiesTxCSV <-
  fread("data/ch02/Counties_in_Texas.csv",
        header = TRUE,
        sep = ",")
head(countiesTxCSV)


## ----------------------------------------------------------------------------------------------------------------------------------
is.data.table(countiesTxCSV)


## ----------------------------------------------------------------------------------------------------------------------------------
countiesTxDTA <- read_dta("data/ch02/Counties_in_Texas.dta", skip = 0)
tail(countiesTxDTA)
is.data.frame(countiesTxDTA)


## ----------------------------------------------------------------------------------------------------------------------------------
countiesTxSPSS <- read_spss("data/ch02/Counties_in_Texas.sav", skip = 0)
head(countiesTxSPSS, n = 10)
is.data.frame(countiesTxSPSS)


## ----------------------------------------------------------------------------------------------------------------------------------
countiesTxXl <- read_xlsx("data/ch02/Counties_in_Texas.xlsx")
head(countiesTxXl)


## ----------------------------------------------------------------------------------------------------------------------------------
write_xlsx(countiesTxCSV, "data/ch02/Output1.xlsx") #Excel

write_sav(countiesTxCSV, "data/ch02/Output2.sav") #SPSS

write_dta(countiesTxCSV, "data/ch02/Output3.dta") #Stata


## ----------------------------------------------------------------------------------------------------------------------------------
sink("data/ch02/Output4.txt", append = TRUE, split = TRUE)
x <- 10
xSquared <- x^2

x
xSquared

sink()

