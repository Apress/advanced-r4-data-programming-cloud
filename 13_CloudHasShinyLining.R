## ----setup, include=TRUE, results='hide', message = FALSE, warning = FALSE, cache=FALSE--------------------------------------------
options(width = 70, digits = 2)
library(knitr)
library(shiny)


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## library(shiny)
## 
## # user interface drawing a pie chart
## ui <- shinyUI(fluidPage(
## 
##    # Page title
##    titlePanel("User Controlled Chart"),
## 
##    # Sidebar with numeric, text, and check box inputs controlled by user
##    sidebarLayout(
##       sidebarPanel(
##          numericInput("pie",
##                      "Percent of Pie Chart",
##                      min = 0,
##                      max = 100,
##                      value = 50),
## 
##          textInput("pietext", "Text Input", value = "Default Title",
##                    placeholder = "Enter Your Title Here"),
## 
##          checkboxInput("pieChoice",
##                        "  I want a Pie Chart instead.", value = FALSE)
##       ),
## 
##       # Show the plot(s)
##       mainPanel(
##          plotOutput("piePlot")
##       )
##    )
## ))
## 
## # server side R code creating Pie Chart or barplot hidden from user
## server <- shinyServer(function(input, output) {
## 
##    output$piePlot <- renderPlot({
##       # generate Pie chart ratios based on input$pie from user
##       y <- c(input$pie, 100-input$pie)
## 
##       # draw the pie chart or barplot with the specified ratio and label
## 
##     if(input$pieChoice == FALSE){
##       barplot(y, ylim = c(0,100),
##               names.arg = c(input$pietext, paste0("Complement of ", input$pietext)))
## 
##     }else{
##       pie(y, labels = c(input$pietext, paste0("Complement of ", input$pietext)))}
## 
##    })
## })
## 
## # Run the application
## shinyApp(ui = ui, server = server)


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## library(shiny)
## 
## # Define user interface
## ui <- shinyUI(fluidPage())
## 
## # Define server
## server <- shinyServer(function(input, output) {})
## 
## # Run the application
## shinyApp(ui = ui, server = server)


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## # Define user interface
## ui <- shinyUI(fluidPage(
##   sidebarLayout(
##     sidebarPanel(),
##     mainPanel()
##   )
##   ))


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## numericInput(inputId, label, value, min = NA, max = NA, step = NA,  width = NULL)
## textInput(inputId, label, value = "", width = NULL, placeholder = NULL)
## checkboxInput(inputId, label, value = FALSE, width = NULL)


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## # user interface drawing a pie chart
## ui <- shinyUI(fluidPage(
##   # Sidebar with numeric, text, and check box inputs controlled by user
##   sidebarLayout(
##     sidebarPanel(
##       numericInput("pie", "Percent of Pie Chart",
##                    min = 0, max = 100, value = 50),
##       textInput("pietext", "Text Input", value = "Default Title",
##                 placeholder = "Enter Your Title Here"),
## 
##       checkboxInput("pieChoice",  "  I want a Pie Chart instead.", value = FALSE)
##               ),
## 
##     mainPanel()
## )
## ))


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## # server side R code creating Pie Chart or Bar plot
## server <- shinyServer(function(input, output) {
## 
##    output$piePlot <- renderPlot({
##       # generate Pie chart ratios based on input$pie from user
##       y <- c(input$pie, 100-input$pie)
## 
##       # draw the pie chart or barplot with the specified ratio and label
## 
##     if(input$pieChoice == FALSE){
##       barplot(y, ylim = c(0,100),
##               names.arg = c(input$pietext, paste0("Complement of ", input$pietext)))
## 
##     }else{
##       pie(y, labels = c(input$pietext, paste0("Complement of ", input$pietext)))}
## 
##    })
## })


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## sliderInput("ayear", "Academic Year:",
##                      min = 2014, max = 2017,
##                      value = 2014, step = 1,
##                      animate = animationOptions(interval=600, loop=T) )


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## library(shiny)
## slider_data<-read.csv("slider_data.csv", header = TRUE, sep = ",")
## Phase1    <- slider_data[,2]
## Phase2    <- slider_data[,3]
## Phase3    <- slider_data[,4]
## Phase4    <- slider_data[,5]
## 
## # Define UI for application that draws Bar Plot
## ui <- shinyUI(fluidPage(
## 
##    # Application title
##    titlePanel("Training Programme Results"),
## 
##    # Sidebar with a slider input for number of bins
##    sidebarLayout(
##       sidebarPanel(
## sliderInput("ayear", "Academic Year:",
##                      min = 2014, max = 2017,
##                      value = 2014, step = 1,
##                      animate = animationOptions(interval=600, loop=T) )  ),
## 
##       # Show the bar plot
##       mainPanel(plotOutput("barPlot") )               )               ))
## 
## # Define server logic required to draw a barplot
## server <- shinyServer(function(input, output) {
## 
##    output$barPlot <- renderPlot({
##       # Count values in each phase that match the correct date.
##      cap<-input$ayear*100
##      x <- c(sum(Phase1<cap), sum(Phase2<cap), sum(Phase3<cap), sum(Phase4<cap))
## 
##       # draw the barplot for the correct year.
##       barplot(x, names.arg = c("Phase I", "Phase II", "Phase III", "Fellows"),
##               col = c("deeppink1","deeppink2","deeppink3","deeppink4"),  ylim=c(0,50))
##    })           })
## 
## # Run the application
## shinyApp(ui = ui, server = server)


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## ui <- shinyUI(fluidPage(
## 
##       fileInput("file", label = h3("Histogram Data File input")),
##       tableOutput("tabledf"),
##       tableOutput("tabledf2"),
##       plotOutput("histPlot1")  ))


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## histData<-reactive({
##   file1<-input$file
##   read.csv(file1$datapath,header=TRUE ,sep = ",")      })


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## output$tabledf2<-renderTable({
##   histData()            })
## 
## output$histPlot1<-renderPlot({
##   hist(as.numeric(histData()$X1))              })


## ----eval=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------
## library(shiny)
## 
## ui <- shinyUI(fluidPage(
## 
##       # Copy the line below to make a file upload manager
##       fileInput("file", label = h3("Histogram Data File input"), multiple = FALSE),
##       tableOutput("tabledf"),
##       tableOutput("tabledf2"),
##       plotOutput("histPlot1")
## 
## ))
## 
## # Define server logic required to draw a
## server <- shinyServer(function(input, output) {
## 
##   output$tabledf<-renderTable({
##     input$file
##   })
## 
## histData<-reactive({
##   file1<-input$file
##   read.csv(file1$datapath, header=TRUE ,sep = ",")     })
## 
## output$tabledf2<-renderTable({
##   histData()           })
## 
## output$histPlot1<-renderPlot({
##   hist(as.numeric(histData()$X1))               })
## 
## })
## 
## # Run the application
## shinyApp(ui = ui, server = server)

