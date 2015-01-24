library(shiny)

# Constants
stepCount <- 1000

shinyServer(function(input, output, session) {

    # per-session data structures
    model <- reactiveValues()
    
    updateModel <- reactive({
        model$name          <- input$name
        model$description   <- input$description
        
        model$parameters    <- updateVariables(input, 'parameters')
        model$states        <- updateVariables(input, 'states')
        
        model$time          <- c( start = input$timeStart
                                , end   = input$timeEnd
                                , step  = (input$timeEnd - input$timeStart) / 1000
                                )
        
        model$parameterUnits  <- input$parameterUnits
        model$stateUnits      <- input$stateUnits
        model$timeUnits       <- input$timeUnits
        
    })
    
    # Test
    output$debug <- renderPrint({
        print(list(input$add.parameters, date()))
    })
    
    ## Build
    
    # Dyamic UI rendering
    output$parametersUI  <- renderUI({
        variableUI(model, 'parameters')
    })
    
    output$statesUI      <- renderUI({
        variableUI(model, 'states')
    })
    
    ## Run
    
})

# UI generation helpers
variableUI <- function(model, namespace) {
    
    variableCount <- length(model[[namespace]])
    
    if (variableCount == 0) {
        variableRows <- div()
    } else {
        ui <- lapply( 1:variableCount
                    , function(n) {
                            
                        label = names(model[[namespace]])[n]
                        value = model[[namespace[label]]]
                          
                        fluidRow( column( 5
                                        , textInput( paste(namespace, 'label', n, sep = "-")
                                                   , ''
                                                   , label
                                                   )
                                        )
                                 , column( 5
                                         , numericInput( paste(namespace, 'value', n, sep = "-")
                                                       , ''
                                                       , value
                                                       )
                                         )
                                 , column( 2
                                         , actionButton( paste('remove', namespace, sep = ".")
                                                       , ''
                                                       , icon  = icon('minus')
                                                       , style = 'width: 100%;'
                                                       )
                                         ) 
                                 )
                      })
        
        variableRows <- div(ui)
    }
    
    tagList( variableRows
           , fluidRow( column( 2
                             , inputButton( paste('add', namespace, sep = ".")
                                          , ''
                                          , icon  = icon('plus')
                                          , style = 'width: 100%;'
                                          )
                             , offset = 10
                             )
                      , style = "margin-bottom: 5px;"
                      )
            )
}

updateVariables <- function(input, namespace) {
    
    inputLabels <- input[grepl(paste(namespace, 'label', sep = "-"), names(input))]
    inputValues <- input[grepl(paste(namespace, 'value', sep = "-"), names(input))]
    
    variables         <- c(inputValues, recursive = TRUE)
    names(variables)  <- c(inputLabels, recursive = TRUE)
    
    variables
    
}

