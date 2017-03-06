library(shiny)
library(shinyBS)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Starting with XGBoost for Classification"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Dataset", 
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 tags$hr(),
                 radioButtons("datasetOptions", 
                              "Choose dataset:",
                              c("My dataset",
                                "Titanic", 
                                "Iris")),
                 tags$hr(),
                 uiOutput("targetFeature"),
                 tags$hr(),
                 uiOutput("features")

      ),
        tabPanel("Tuning Parameters", 
                 radioButtons("fixedParameter", 
                              "Fix one parameter:",
                              c("Max Depth", 
                                "Number of Rounds", 
                                "Learning Rate")),
                 tags$hr(),
                 uiOutput("parameter1"),
                 tags$hr(),
                 uiOutput("parameter2"),
                 tags$hr(),
                 uiOutput("runButton")
      )
    )),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "mainTabsetId",
        tabPanel("Documentation", includeMarkdown("documentation.Rmd")),
        tabPanel("Results", value = "results", 
                 tags$hr(),
                 bsAlert("alert"),
                 plotOutput("resultsPlot"),
                 uiOutput("download"))
        

      )
    )
  )
))

