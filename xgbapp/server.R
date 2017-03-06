library(shiny)
library(shinyBS)
library(caret)
library(xgboost)
library(plyr)
library(ggplot2)
library(titanic)


shinyServer(function(input, output, session) {
  trainedModel <- reactiveValues()
  trainXGB <- function (data, 
                        targetColumn,
                        features,
                        nrounds, 
                        maxdepth, 
                        eta,
                        cv) {

    if(!is.null(features)) {
      trainData <- data[,features]
    }
    else {
      trainData <- data
    }
    trainLabels <- as.factor(data[,targetColumn])
    summaryFunction <- NULL
    if(nlevels(trainLabels) == 2) { 
      trainLabels <- mapvalues(trainLabels, from = c("0","1"), to = c("negative","positive"))
    }
    else {
      trainLabels <- make.names(trainLabels)
    }

    trainMatrix <- data.matrix(trainData)
    
    ## Model selection using CV 
    
    ##Search Grid
    xgb_grid <- expand.grid(
      nrounds = nrounds,
      max_depth = maxdepth,
      eta = eta,
      gamma = 0,
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 1
    )
    
    if(nlevels(trainLabels) == 2) {
      ##Train Control
      xgb_train_control <- trainControl(
        method = "cv",
        number = cv,
        verboseIter = FALSE,
        returnData = FALSE,
        returnResamp = "all",                                                      
        classProbs = TRUE,    
        allowParallel = TRUE,
        summaryFunction = twoClassSummary
      )
    }
    else {
      xgb_train_control <- trainControl(
        method = "cv",
        number = cv,
        verboseIter = FALSE,
        returnData = FALSE,
        returnResamp = "all",                                                      
        classProbs = TRUE,    
        allowParallel = TRUE
      )
    }
    
    xgb_train <- train(
      x=trainMatrix,
      y=trainLabels,
      trControl = xgb_train_control,
      tuneGrid = xgb_grid,
      method = "xgbTree"
    )
    return(xgb_train)
  }
  
  output$targetFeature <- renderUI({
    inFile <- input$file1
    datasetSelected <- input$datasetOptions
    if(datasetSelected == "My dataset") {
      if(is.null(inFile))
        return(NULL)
      else
        dataset <- read.csv(inFile$datapath)
    }
    if(datasetSelected == "Titanic"){
      dataset <- titanic_train
    } 
    else if (datasetSelected == "Iris") {
      dataset <- iris
    }
    radioButtons("target", 
                 "Target column:",
                  names(dataset))
  })
  
  output$features <- renderUI({
    inFile <- input$file1
    datasetSelected <- input$datasetOptions
    if(datasetSelected == "My dataset") {
      if(is.null(inFile))
        return(NULL)
      else
        dataset <- read.csv(inFile$datapath)
    }
    if(datasetSelected == "Titanic"){
        dataset <- titanic_train
      } 
    else if (datasetSelected == "Iris") {
      dataset <- iris
    }
    target <- input$target
    checkboxGroupInput("otherFeatures", "Features:",
      setdiff(names(dataset), target))
  })
  
  output$parameter1 <- renderUI({
    possibleParameters <- c("Max Depth", 
                            "Number of Rounds", 
                            "Learning Rate")
    placeholderEx <- "Eg.: 4;6;8;10"
    fixedParameter <- input$fixedParameter
    thisParameter1 <- setdiff(possibleParameters, fixedParameter)[1]
    if(thisParameter1 == "Number of Rounds") {
      placeholderEx <- "Eg.: 100;200;300"
    }
    else if(thisParameter1 == "Learning Rate") {
      placeholderEx <- "Eg.: 0.05;0.1;0.3"
    }
    textInput("paramater1Id", 
              thisParameter1, 
              placeholder = placeholderEx)
  })
  
  output$parameter2 <- renderUI({
    possibleParameters <- c("Max Depth", 
                            "Number of Rounds", 
                            "Learning Rate")
    placeholderEx <- "Eg.: 4;6;8;10"
    fixedParameter <- input$fixedParameter
    thisParameter2 <- setdiff(possibleParameters, fixedParameter)[2]
    if(thisParameter2 == "Number of Rounds") {
      placeholderEx <- "Eg.: 100;200;300"
    }
    else if(thisParameter2 == "Learning Rate") {
      placeholderEx <- "Eg.: 0.05;0.1;0.3"
    }
    textInput("paramater2Id", 
              label = thisParameter2,
              placeholder = placeholderEx)
  })
  
  output$runButton <- renderUI({
    inFile <- input$file1
    datasetSelected <- input$datasetOptions
    if(!is.null(inFile) || datasetSelected != "My dataset")
      bsButton("runButtonId",
               label  = "Tune your model!", 
               style  = "primary")  
  })
  
  
  observeEvent(input$runButtonId, {
    updateTabsetPanel(session, 
                      "mainTabsetId", 
                      selected = "results")
    withProgress(message = "Tuning your model",
                 value = 0, {
        closeAlert(session, "alertParameters")
        inFile <- input$file1
        datasetSelected <- input$datasetOptions
        possibleParameters <- c("Max Depth", 
                                "Number of Rounds", 
                                "Learning Rate")
        if(!is.null(inFile) && datasetSelected == "My dataset") {
          inFile <- input$file1
          dataset <- read.csv(inFile$datapath)
        }
        else if(datasetSelected == "Titanic") {
          dataset <- titanic_train
        }
        else if(datasetSelected == "Iris") {
                dataset <- iris
        }
        incProgress(0.25)
        features <- input$otherFeatures
        targetColumn <- input$target
        fixedParameter <- input$fixedParameter
        parameter1 <- setdiff(possibleParameters, fixedParameter)[1]
        parameter2 <- setdiff(possibleParameters, fixedParameter)[2]
                   
        if(fixedParameter == "Number of Rounds") {
          nrounds <- 50
          if(parameter1 == "Learning Rate") {
            eta <- as.numeric(strsplit(input$paramater1Id,";")[[1]])
            maxdepth <- as.numeric(strsplit(input$paramater2Id,";")[[1]])
            size1 <- length(eta)
            size2 <- length(maxdepth)
          }
          else {
            maxdepth <- as.numeric(strsplit(input$paramater1Id,";")[[1]])
            eta <- as.numeric(strsplit(input$paramater2Id,";")[[1]])
            size1 <- length(eta)
            size2 <- length(maxdepth)
          }
        }
        else if(fixedParameter == "Learning Rate") {
          eta <- 0.3
          if(parameter1 == "Number of Rounds") {
            nrounds <- as.numeric(strsplit(input$paramater1Id,";")[[1]])
            maxdepth <- as.numeric(strsplit(input$paramater2Id,";")[[1]])
            size1 <- length(nrounds)
            size2 <- length(maxdepth)
          }
          else {
            maxdepth <- as.numeric(strsplit(input$paramater1Id,";")[[1]])
            nrounds <- as.numeric(strsplit(input$paramater2Id,";")[[1]])
            size1 <- length(nrounds)
            size2 <- length(maxdepth)
          }
        }
        else if(fixedParameter == "Max Depth") {
          maxdepth <- 6
          if(parameter1 == "Learning Rate") {
            eta <- as.numeric(strsplit(input$paramater1Id,";")[[1]])
            nrounds <- as.numeric(strsplit(input$paramater2Id,";")[[1]])
            size1 <- length(nrounds)
            size2 <- length(eta)
          }
          else {
            nrounds <- as.numeric(strsplit(input$paramater1Id,";")[[1]])
            eta <- as.numeric(strsplit(input$paramater2Id,";")[[1]])
            size1 <- length(nrounds)
            size2 <- length(eta)
          }
        }
        if(sum(is.na(maxdepth)) > 0 || 
           sum(is.na(eta)) > 0 || 
           sum(is.na(nrounds)) > 0 ||
           size1 < 2 ||
           size2 < 2){
          trainedModel <<- NULL
          createAlert(session, "alert", "alertParameters", title = "Oops",
                      content = "There was a problem parsing your parameters
                                input. <br> Check if both of them are following the correct
                                pattern. <br> It should be something like: 1;2;3", 
                      append = FALSE)
        }
        else {
          incProgress(0.5)
          closeAlert(session, "alertParameters")
          setTimeLimit(60)
          out <-  tryCatch ({
            trainXGB( dataset,
                      targetColumn,
                      features,
                      nrounds,
                      maxdepth,
                      eta,
                      10)
            }, error = function(err) {
                return(NA)
            })
            if(!is.na(out)) {
              trainedModel <<- out
            }
            else {
              trainedModel <<- NULL
              createAlert(session, "alert", "alertParameters", title = "Oops",
                          content = "There was a problem tuning your model. <br>
                  This usually happens because you choose a continuous variable
                  as your target column. <br> Remember, this app only works
                  for classification problems.
                  <br> Check all the steps and try again.", 
                          append = FALSE)
            }
        }
    })
    
  })
  
  output$resultsPlot <- renderPlot({
    input$runButtonId
    if(!is.null(input$runButtonId) && input$runButtonId > 0){
        if(!is.null(trainedModel)) {
          plot(trainedModel)
        }
    }
  })
  output$download <- renderUI({
    input$runButtonId
    if(!is.null(input$runButtonId) && input$runButtonId > 0){
      if(!is.null(trainedModel)) {
        downloadButton("rdata","Download R data")
      }
    }
  })

  output$rdata <- downloadHandler(
    filename = function () {
      paste("xgboost.RData")
    },
    content = function(file) {
      save(trainedModel,file=file)
    }
  )

})
