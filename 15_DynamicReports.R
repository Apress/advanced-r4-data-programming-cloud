## ----setup, include=TRUE, results='hide', message = FALSE, warning = FALSE, cache=FALSE--------------------------------------------
options(width = 70, digits = 2)


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## install.packages("rmarkdown")
## install.packages("tufte")
## install.packages("knitr")
## install.packages("evaluate")
## install.packages("ltm")
## install.packages("formatR")
## library(knitr)


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## ---
## title: "ch15_html"
## author: "Matt & Joshua Wiley"
## date: "29 April 2020"
## output: html_document
## ---
## 
## ```{r echo=FALSE}
## ##notice the 'echo=FALSE'
## ##it means the following code will never make it to our knited html
## library(data.table)
## diris <- as.data.table(iris)
## ##it is still run, however, so we do have access to it.
## ```
## 
## #This is the top-level header; it is not a comment.
## 
## This is plain text that is not code.
## 
## If we wish *italics*  or **bold**, we can easily add those to these documents.
## Of course, we may need to mention `rmarkdown` is the package used,
## and inline code is nice for that.
## If mathematics are required, then perhaps x~1~^2^ + x~2~^2^ = 1 is wanted.
## 
## ##calling out mathematics with a header 2
## On the other hand, we may need the mathematics called out explicitly,
## in which case  $x^{2} + y^{2}= \pi$ is the way to make that happen.
## 
## ###I like strikeouts; I am less clear about level 3 headers.
## I often find when writing reports that I want to say something
## is ~~absolutely foolish~~ obviously relevant to key stakeholders.
## 
## ######If you ever write something that needs Header 6
## Then I believe, as this unordered list suggests:
## 
## * You need to embrace less order starting now
## 
##     - Have you considered other careers?
## 
## ```{r echo=FALSE}
## summary(diris)
## ```
## 
## However, notice that one can include both the code and the console output:
## 
## ```{r}
## hist(diris$Sepal.Length)
## ```


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## ---
## title: "ch15_html"
## author: "Matt & Joshua Wiley"
## date: "29 April 2020"
## output: html_document
## ---


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## ```{r echo=FALSE}
## library(data.table)
## diris <- as.data.table(iris)
## ```
## 
## ```{r echo=FALSE}
## summary(diris)
## ```


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## ```{r fig.align="right"}
## hist(diris$Sepal.Length)
## ```


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## ##Dynamic Reports and the Cloud
## 
## 
## library(shiny)
## library(ltm)
## library(data.table)
## library(rmarkdown)
## library(tufte)
## library(formatR)
## library(knitr)
## library(evaluate)
## 
## function(input, output) {
##   output$contents <- renderTable({
##     inFile <- input$file1
## 
##     if (is.null(inFile))
##       return(NULL)
## 
##     read.csv(
##       inFile$datapath,
##       header = input$header,
##       sep = input$sep,
##       quote = input$quote
##     )
##   })
## 
##   scores <- reactive({
##     inFile <- input$file1
##     if (is.null(inFile))
##       return(NULL)
## 
##     read.csv(
##       inFile$datapath,
##       header = input$header,
##       sep = input$sep,
##       quote = input$quote
##     )
##   })
## 
##   output$downloadReport <- downloadHandler(
##     filename = function() {
##       paste('quiz-report', sep = '.', 'pdf')
##     },
## 
##     content = function(file) {
##       src <- normalizePath('report.Rmd')
## 
##       owd <- setwd(tempdir())
##       on.exit(setwd(owd))
##       file.copy(src, 'report.Rmd', overwrite = TRUE)
## 
##       knitr::opts_chunk$set(tidy = FALSE, cache.extra = packageVersion('tufte'))
##       options(htmltools.dir.version = FALSE)
## 
##       out <- render('report.Rmd')
##       file.rename(out, file)
##     }
##   )
## }


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## output$contents <- renderTable({
## 
##       inFile <- input$file1
## 
##       if (is.null(inFile))
##         return(NULL)
## 
##       read.csv(inFile$datapath, header = input$header,
##                sep = input$sep, quote = input$quote)
##     })


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## scores <- reactive({
##       inFile <- input$file1
## 
##       if (is.null(inFile))
##         return(NULL)
## 
##       read.csv(inFile$datapath, header = input$header,
##                sep = input$sep, quote = input$quote)
##     })


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## output$downloadReport <- downloadHandler(
##   filename = function() {
##     paste('quiz-report', sep = '.', 'pdf')
##   },
## 
##   content = function(file) {
##     src <- normalizePath('report.Rmd')
## 
##     owd <- setwd(tempdir())
##     on.exit(setwd(owd))
##     file.copy(src, 'report.Rmd', overwrite = TRUE)
##     knitr::opts_chunk$set(tidy = FALSE, cache.extra = packageVersion('tufte'))
##     options(htmltools.dir.version = FALSE)
## 
##     out <- render('report.Rmd')
##     file.rename(out, file)
##   }    )


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## #Dynamic Reports and the Cloud User Interface
## shinyUI(fluidPage(title = 'Score Control Panel',
##                   sidebarLayout(
##                     sidebarPanel(
##                       helpText(
##                         "Upload a wide CSV file where
##                  each column is 0 or 1 based on whether the student
##                  got that question incorrect or correct."
##                       ),
## 
##                  fileInput(
##                    'file1',
##                    'Choose file to upload',
##                    accept = c(
##                      'text/csv',
##                      'text/comma-separated-values',
##                      'text/tab-separated-values',
##                      'text/plain',
##                      '.csv',
##                      '.tsv'
##                    )
##                  ),
##                  tags$hr(),
##                  checkboxInput('header', 'Header', TRUE),
##                  radioButtons('sep', 'Separator',
##                               c(
##                                 Comma = ',',
##                                 Semicolon = ';',
##                                 Tab = '\t'
##                               ),
##                               ','),
##                  radioButtons(
##                    'quote',
##                    'Quote',
##                    c(
##                      None = '',
##                      'Double Quote' = '"',
##                      'Single Quote' = "'"
##                    ),
##                    '"'
##                  ),
##                  tags$hr(),
##                  helpText("This is where helpertext goes"),
##                  downloadButton('downloadReport')
##                     ),
##                  mainPanel(tableOutput('contents'))
##                   )))


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## fileInput('file1', 'Choose file to upload',
##           accept = c(
##             'text/csv',
##             'text/comma-separated-values',
##             'text/tab-separated-values',
##             'text/plain',
##             '.csv',
##             '.tsv'
##           )
## ),


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## downloadButton('downloadReport')


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## ---
## title: "Example Scoring Report"
## subtitle: "Item-Level Analysis"
## date: "`r Sys.Date()`"
## output:
##   tufte::tufte_handout: default
## ---
## 
## # Raw Data
## 
## Here is a sample of the data uploaded including the first few and last
## rows, and first few and last columns:
## 
## ```{r echo=FALSE, include = FALSE}
## scores <- scores()
## scores <- as.data.table(scores)
## setnames(scores, 1, "Student")
## 
## ```
## 
## ```{r, echo = FALSE, results = 'asis'}
## if (nrow(scores) > 6) {
##  row.index <- c(1:3, (nrow(scores)-2):nrow(scores))
## }
## 
## if (ncol(scores) > 6) {
##  col.index <- c(1:3, (ncol(scores)-2):ncol(scores))
## }
## 
## kable(scores[row.index, col.index, with = FALSE])
## ```
## 
## ```{r, include = FALSE}
## items <- names(scores)[-1]
## 
## scores[, SUM := rowSums(scores[, items, with = FALSE])]
## 
## ## now calculate biserial correlations
## ## first melt data to be long
## scores.long <- melt(scores, id.vars = c("Student", "SUM"))
## 
## ## calculate biserial correlation, by item
## ## order from high to low
## biserial.results <- scores.long[, .(
##   r = round(biserial.cor(SUM, value, level = 2), 3),
##   Correct = round(mean(value) * 100, 1)
##   ), by = variable][order(r, decreasing = TRUE)]
## 
## alpha.results <- cronbach.alpha(scores[, !c("Student", "SUM"), with=FALSE])
## 
## rasch.results <- rasch(scores[,!c("Student", "SUM"), with=FALSE])
## 
## ```
## 
## The test overall had `r ifelse(alpha.results$alpha > .6, "acceptable
## reliability", "low reliability")` of
## alpha = `r format(alpha.results$alpha, FALSE, 2, 2)`.^[Alpha ranges
## from 0 to 1, with one indicating a perfectly reliable test.]
## 
## The graph shows the measurement error by level of
## ability.^[Higher values indicate more measurement error,
## indicating the test is less reliable at very low and very high ability levels (scores).]
## 
## ```{r, echo = FALSE, fig.width = 5, fig.height = 4}
## 
## ## The Standard Error of Measurement can be plotted by
## vals <- plot(rasch.results, type = "IIC", items = 0, plot = FALSE)
## plot(vals[, "z"], 1 / sqrt(vals[, "info"]),
##      type = "l", lwd = 2, xlab = "Ability", ylab = "Standard Error",
##      main = "Standard Error of Measurement")
## 
## ```
## 
## # Item Analysis
## 
## Results for individual items are shown in the following
## table.^[*r* indicates the point biserial correlation of an item with the total score.
## *Correct* indicates the percent of correct responses to a particular item.
## The items are sorted from highest to lowest correlation.]
## 
## ```{r, echo = FALSE, results = 'asis'}
## kable(biserial.results)
## ```


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## ---
## title: "Example Scoring Report"
## subtitle: "Item Level Analysis"
## date: "`r Sys.Date()`"
## output:
##   tufte::tufte_handout: default
## ---


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## ```{r echo=FALSE, include = FALSE}
## scores <- scores()
## scores <- as.data.table(scores)
## setnames(scores, 1, "Student")
## ```


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## ```{r, echo = FALSE, results = 'asis'}
## if (nrow(scores) > 6) {
##  row.index <- c(1:3, (nrow(scores)-2):nrow(scores))
## }
## 
## if (ncol(scores) > 6) {
##  col.index <- c(1:3, (ncol(scores)-2):ncol(scores))
## }
## 
## kable(scores[row.index, col.index, with = FALSE])
## ```


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## ```{r, include = FALSE}
## items <- names(scores)[-1]
## 
## scores[, SUM := rowSums(scores[, items, with = FALSE])]
## 
## ## now calculate biserial correlations
## ## first melt data to be long
## scores.long <- melt(scores, id.vars = c("Student", "SUM"))
## 
## ## calculate biserial correlation, by item
## ## order from high to low
## biserial.results <- scores.long[, .(
##   r = round(biserial.cor(SUM, value, level = 2), 3),
##   Correct = round(mean(value) * 100, 1)
##   ), by = variable][order(r, decreasing = TRUE)]
## 
## alpha.results <- cronbach.alpha(scores[, !c("Student", "SUM"), with=FALSE])
## 
## rasch.results <- rasch(scores[,!c("Student", "SUM"), with=FALSE])
## 
## ```


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## The test overall had `r ifelse(alpha.results$alpha > .6, "acceptable
## reliability", "low reliability")` of
## alpha = `r format(alpha.results$alpha, FALSE, 2, 2)`.^[Alpha ranges from 0 to 1,
## with one indicating a perfectly reliable test.]
## 
## The graph shows the measurement error by level of
## ability.^[Higher values indicate more measurement error,
## indicating the test is less reliable at very low and very high ability levels (scores).]


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## ```{r, echo = FALSE, fig.width = 5, fig.height = 4}
## 
## ## The Standard Error of Measurement can be plotted by
## vals <- plot(rasch.results, type = "IIC", items = 0, plot = FALSE)
## plot(vals[, "z"], 1 / sqrt(vals[, "info"]),
##      type = "l", lwd = 2, xlab = "Ability", ylab = "Standard Error",
##      main = "Standard Error of Measurement")
## 
## ```

