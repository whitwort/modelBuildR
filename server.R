library(shiny)
library(jsonlite)

# UI generators
optionsPanel <- function(model) {
    parameterRange <- (max(model$parameters) - min(model$parameters)) 
    stateRange     <- (max(model$states)     - min(model$states))
    
    
    panel( "Advanced options"
         , selectInput( 'solver'
                      , 'Solver'
                      , choices   = names(solvers)
                      , selectize = FALSE
                      )
         , numericInput( 'timeSteps'
                       , 'Time points'
                       , value = timeStepCount
                       , min   = 1
                       )
         , numericInput( 'parameterStepSize'
                       , 'Parameter step size'
                       , value = if (parameterRange > 0) { parameterRange / parameterStepCount } else { 1 }
                       , min   = 0
                       )
         , numericInput( 'stateStepSize'
                       , 'State step size'
                       , value = if (stateRange > 0) { stateRange / stateStepCount } else { 1 }
                       , min   = 0
                       )
         )
    
}

variablePanel <- function(model, variable, pretext = "", posttext = "", step = 1) {
    names         <- names(model[[variable]])
    defaultValues <- model[[variable]]
    
    lapply( 1:length(names)
          , function(i) {
              name <- names[i]
              
              numericInput( inputID = paste(name, i, sep = ".")
                          , label   = paste(pretext, name, posttext)
                          , value   = defaultValues[i]
                          , step    = 1
                          )
            }
          )
}

statesPanel <- function(model) {
    panel( "States"
         , variablePanel( model
                        , 'states'
                        , pretext  = paste("Initial")
                        , posttext = paste("(", model$stateUnits, ")", sep = "")
                        , step     = model$stateStepSize
                        )
         , class = "panel panel-success"
         )
}

parametersPanel <- function(model) {
    panel( "Parameters"
         , variablePanel( model
                        , 'parameters'
                        , posttext = paste("(", model$parameterUnits, ")", sep = "")
                        , step     = model$parameterStepSize
                        )
         , class = "panel panel-info"
         )
}


# Server runtime
shinyServer(function(input, output, session) {

    model <- reactive({ 
        sapply(modelSchema, function(s) { input[[s]] }, simplify = FALSE)
    })
    
    pushModel <- function(model) {
        sapply( intersect(names(model), names(input))
              , function(s) {  
                  session$sendInputMessage(s, list(value = model[[s]]))
                }
              )
        
        model()
    }
    
    openModel <- observeEvent(input$open, {
        if (!is.na(installedModels[input$open])) {
            pushModel(fromJSON(installedModels[input$open]))
        }
    })
    
    uploadModel <- observeEvent(input$modelUpload, {
        pushModel(fromJSON(input$modelUpload$datapath))
    })
    
    rateEquationText <- reactive({
        math        <- math
        names(math) <- paste("math", 0:(length(math) - 1), sep = ".")
        
        parameters        <- paste("parameters[", 1:(length(input$parameters)), "]", sep = "")
        names(parameters) <- paste("parameters", 0:(length(input$parameters) - 1), sep = ".")
        
        states            <- paste("states[", 1:(length(input$states)), "]", sep = "")
        names(states)     <- paste("states", 0:(length(input$states) - 1), sep = ".")
        
        namespace <- c(math, parameters, states)
        sapply( input$equations
              , function(eq) {
                  if (is.na(eq)) {
                      NA
                  } else {
                      paste(namespace[as.character(eq)], collapse = "")    
                  }
                }
              )
    })
    
    # Output bindings
    output$equations <- renderEquationErrors({
        equations <- rateEquationText()
        test <- sapply( equations
                      , function(eq) {
                          if (is.na(eq)) { return(FALSE) }
                          p <- try(parse(text = eq), silent = FALSE)
                          if (class(p) == "expression") { TRUE } else { FALSE }
                        }
                      , USE.NAMES = FALSE
                      )
        
        if (length(test) < length(input$states)) {
            test[ (length(test) + 1):length(input$states) ] <- FALSE
        }

        as.list(test)
    })
    
    output$download <- downloadHandler( filename = paste(model()$name, "json", sep = ".")
                                      , content  = function(tmp) { 
                                            cat(toJSON(as.list(model()), pretty = TRUE), file = tmp)
                                        }
                                      )
    
    # Dynamic UI elements
    output$optionsPanel <- renderUI({ optionsPanel(model()) })
    
    # Test
    output$debug <- renderText({
        toJSON(as.list(model()), pretty = TRUE)
        #print(toJSON(as.list(input$equations), pretty = TRUE))
        #print(toJSON(as.list(session$clientData)))
        #toJSON(as.list(session$clientData), pretty = TRUE)
        #print(input$open)
    })
    
})

