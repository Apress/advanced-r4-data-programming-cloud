#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("XML")
#install.packages("data.table")
#install.packages("zoo")

########################################################################
##                                                                    ##
##            Shiny Dashboard Sampler                                 ##
##                                                                    ##
########################################################################

library(shiny)
library(shinydashboard)


#run-once code

slider_data <- read.csv("slider_data.csv", header = TRUE, sep = ",")
Phase1    <- slider_data[, 2]
Phase2    <- slider_data[, 3]
Phase3    <- slider_data[, 4]
Phase4    <- slider_data[, 5]

startYear <- 2013
endYear <- 2016

# Define UI for dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Data Programming and the Cloud"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Pie Charts!",
        tabName = "PieChart_Time",
        icon = icon("pie-chart")
      ),
      menuItem(
        "Use a Slider",
        tabName = "slider_time",
        icon = icon("sliders"),
        badgeLabel = "New"
      ),
      menuItem(
        "Upload a Histogram File",
        tabName = "Upload_hist",
        icon = icon("bar-chart"),
        badgeLabel = "Live"
      )
    ),
    img(
      src = "R4_DataProgandCloud.JPG",
      height = 200,
      width = 150
    )
    
  ),
  dashboardBody(tabItems(
    # PieChart_Time Content
    tabItem(tabName = "PieChart_Time",
            fluidRow(
              box(
                title = "PieChart_Time",
                status = "warning",
                numericInput(
                  "pie",
                  "Percent of Pie Chart",
                  min = 0,
                  max = 100,
                  value = 50
                ),
                
                textInput(
                  "pietext",
                  "Text Input",
                  value = "Default Title",
                  placeholder = "Enter Your Title Here"
                ),
                
                checkboxInput("pieChoice",
                              "  I want a Pie Chart instead.",
                              value = FALSE)
              ),
              
              box(
                title = "Graphical Output",
                solidHeader = TRUE,
                status = "warning",
                plotOutput("piePlot")
              )
            )),
    
    # Slider Tab Content
    tabItem(
      tabName = "slider_time",
      h2("Training Programme Results"),
      fluidRow(
        box(
          title = "Control the Academic Year",
          status = "primary",
          solidHeader = TRUE,
          sliderInput(
            "ayear",
            "Academic Year:",
            min = 2014,
            max = 2017,
            value = 2014,
            step = 1,
            animate = animationOptions(interval =
                                         600, loop = T)
          )
        ),
        box(plotOutput("barPlot"))
      ),
      fluidRow(
        valueBox(
          endYear - startYear,
          "Years of Data",
          icon = icon("band-aid"),
          width = 2
        )
      )
      
    ),
    
    #Histogram from an Uploaded CSV
    tabItem(tabName = "Upload_hist",
            
            fluidRow(
              box(title = "File Input",
        # Copy the line below to make a file upload manager
                  fileInput(
                    "file",
                    label = h3("Histogram Data File input"),
                    multiple = FALSE
                  )),
              box(
                title = "Data from file input",
                collapsible = TRUE,
                tableOutput("tabledf")
              )
              
            ),
            
            fluidRow(
              box(tableOutput("tabledf2")),
              box(background = "blue",
                  plotOutput("histPlot1"))
            ))
    
  )),
  title = "Dashboard Sampler",
  skin = "yellow"
  
)

#################### Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  output$piePlot <- renderPlot({
    # generate Pie chart ratios based on input$pie from user
    y <- c(input$pie, 100 - input$pie)
    
    # draw the pie chart or barplot with the specified ratio and label
    
    if (input$pieChoice == FALSE) {
      barplot(y,
              ylim = c(0, 100),
              names.arg = c(input$pietext, paste0("Complement of ",
                                                  input$pietext)))
      
    } else{
      pie(y, labels = c(input$pietext, paste0("Complement of ",
                                              input$pietext)))
    }
    
  })
  
  
  output$barPlot <- renderPlot({
    # Count values in each phase which match the correct date.
    cap <- input$ayear * 100
    x <- c(sum(Phase1 < cap),
           sum(Phase2 < cap),
           sum(Phase3 < cap),
           sum(Phase4 < cap))
    
    
    # draw the barplot for the correct year.
    barplot(
      x,
      names.arg = c("Phase I", "Phase II", "Phase III", "Fellows"),
      col = c("deeppink1", "deeppink2", "deeppink3", "deeppink4"),
      ylim = c(0, 50)
    )
  })
  
  ####Here is where the input of a file happens
  
  output$tabledf <- renderTable({
    input$file
  })
  
  histData <- reactive({
    file1 <- input$file
    read.csv(file1$datapath, header = TRUE , sep = ",")
    
  })
  
  output$tabledf2 <- renderTable({
    histData()
    
  })
  
  output$histPlot1 <- renderPlot({
    hist(as.numeric(histData()$X1))
    
  })
  
  ##############end of server####################
})

# Run the application
shinyApp(ui = ui, server = server)