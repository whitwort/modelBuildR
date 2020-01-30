library(shiny)
library(jsonlite)

# UI generators
optionsPanel <- function(model) {
    parameterRange <- if (length(model$parameters) > 0) {
                        (max(model$parameters) - min(model$parameters)) 
                      } else {
                        0    
                      }
    stateRange     <- if (length(model$states) > 0) {
                        (max(model$states)     - min(model$states))
                      } else {
                        0
                      }
    
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

simulationControls <- function(model) {
    sidebarHeader <- "The time simulation will update as you change initial states and parameters below.  Use the 'Summarize' tab to iterate over a series of input parameters and produce summary plots of the results."
    stateFormat   <- function(name) {
                       paste("Initial", name, model$stateUnits)
                     }
    
    
    list( helpText(HTML(markdownToHTML(text = sidebarHeader, fragment.only = TRUE)))
        , lapply( names(model$states)
                , function(name) {
                    numericInput( name
                                , stateFormat(name)
                                , model$states[name]
                                )  
                  }
                )
        )
}

# Server runtime
function(input, output, session) {

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
                  paste(namespace[as.character(eq)], collapse = "")   
                }
              )
    })
    
    checkEquationErrors <- reactive({
        equations <- rateEquationText()
        test <- sapply( equations
                      , function(eq) {
                          if (is.na(eq)) { return(FALSE) }
                          p <- try(parse(text = eq), silent = TRUE)
                          if (class(p) == "expression") { TRUE } else { FALSE }
                      }
                      , USE.NAMES = FALSE
                      )
        
        if (length(test) < length(input$states)) {
            test[ (length(test) + 1):length(input$states) ] <- FALSE
        }
        
        as.list(test)
    })
    
    # Issue error if state and parameter names are not all unique; true if all are unique
    checkUniqueNames <- reactive({
        model <- model()
        n     <- c(names(model$states), names(model$parameters))
        !any(duplicated(n))
    })
    
    observe({
        if (!checkUniqueNames()) {
            model <- model()
            if (any(duplicated(names(model$states)))) {
                session$sendCustomMessage( 'addClass'
                                         , list( selector = "#states-panel"
                                               , class = "panel-danger"
                                               )
                                         )  
            } else if (any(duplicated(names(model$parameters)))) {
                session$sendCustomMessage( 'addClass'
                                         , list( selector = "#parameters-panel"
                                               , class = "panel-danger"
                                               )
                                         )  
            } else {
                session$sendCustomMessage( 'addClass'
                                         , list( selector = ".danger-toggle"
                                               , class = "panel-danger"
                                               )
                                         )  
            }
        } else {
            session$sendCustomMessage( 'removeClass'
                                     , list( selector = ".danger-toggle"
                                           , class = "panel-danger"
                                           )
                                     )
        }
    })
    
    # Hide run if the model is empty or has errors
    observe({
        model  <- model()
        errors <- !length(model$equations) > 0 || 
                  !checkUniqueNames() || 
                  !all(checkEquationErrors())

        if (errors) {
          hideTab('navbar', 'run', session = session)
        } else {
          showTab('navbar', 'run')
        }
        
    })
    
    # Build page output bindings
    output$equations <- renderEquationErrors({
        checkEquationErrors()
    })
    
    output$download <- downloadHandler( filename = function() { 
                                            paste(model()$name, "json", sep = ".")
                                        }
                                      , content  = function(tmp) { 
                                            cat(toJSON(as.list(model()), pretty = TRUE), file = tmp)
                                        }
                                      )
    
    output$optionsPanel <- renderUI({ optionsPanel(model()) })
    
    # Run page output bindings
    output$runPanel <- renderUI({
        model <- model()
        
        controls <- tagList(simulationControls(model))
        kinetics <- tabPanel("Simulate")
        summary  <- tabPanel("Summarize")
        
        fluidPage( titlePanel(model$name)
                 , fluidRow( column(3, wellPanel(controls))
                           , column(9, tabsetPanel(kinetics, summary, id = 'tabs'))
                           )
                 )
    })
    

}

