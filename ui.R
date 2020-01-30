library(shiny)
library(shinythemes)

openChoices <- c( as.list(names(installedModels))
                , list(NA, div( style = "padding: 3px 20px;", fileInput('modelUpload', 'Upload')))
                )

buildPage <- tabPanel( 'build'
                     , h2('Build a model')
                     , p("You can use this page to build a mathematical model based on a set of time resolved differential equations.")
                     , p("When you've finished you can run simulations with your model on the 'Run' tab above.  If you don't see a Run tab it's because there is a problem with your model; look for boxes highlighted in red to find the problem.")
                     , fluidRow( column( 8
                                       , textInput('name', 'Name', value = 'myModel')
                                       , textareaInput('description', 'Description')
                                       , downloadButton('download', 'Download model', icon = icon('save', lib = 'glyphicon'))
                                       , dropdownInput('open', 'Open model', choices = openChoices, icon = icon('open', lib = 'glyphicon'))
                                       , offset = 2
                                       , class  = 'well'
                                       )
                               )
                     
                     , fluidRow( column( 4
                                       , panel( "States"
                                              , p('State variables start with initial values that can change over time during the simulation.  Useful for tracking things like molecular concentrations or populations. State and parameter names must all be unique.')
                                              , vectorInput('states', type = 'number', valueLabel = "Initial", nameLabel = "Name")
                                              , textInput('stateUnits', 'Units', value = 'mM', horizontal = TRUE)
                                              , class = "panel panel-success danger-toggle"
                                              , id = "states-panel"
                                              )
                                        )
                         
                                , column( 4
                                       , panel( "Parameters"
                                              , p("Parameters are variables that can be set before the simulation is run but don't change over time.  Useful for fixed rates like kinetic constants.  State and parameter names must all be unique.")
                                              , vectorInput('parameters', type = 'number', valueLabel = "Default", nameLabel = "Name")
                                              , textInput('parameterUnits', 'Units', value = 's-1', horizontal = TRUE)
                                              , class = "panel panel-info danger-toggle"
                                              , id = "parameters-panel"
                                              )
                                       )
                               , column( 4
                                       , panel( "Time scale"
                                              , p('Define the minimum and maximum useful time scales to use when running the simulation.')
                                              , numericInput('timeStart', 'Minimum', value = 1, min = 0, horizontal = TRUE)
                                              , numericInput('timeEnd', 'Maximum', value = 10, min = 0, horizontal = TRUE)
                                              , textInput('timeUnits', 'Units', value = 's', horizontal = TRUE)
                                              )
                                       )
                               )
                     
                     , fluidRow( column( 8
                                       , equationEditor( "equations"
                                                       , description = "Build rate of change equations by selecting variables and math symbols from the drop down list, or by typing names and hitting 'enter' when the symbol you want is selected.  You can use the arrow keys to move your cursor and backspace to delete selections. You can only use the names and symbols in the list."
                                                       )
                                       , offset = 2
                                       )
                               )
                     
                     , fluidRow( column( 8
                                       , htmlOutput('optionsPanel')
                                       , offset = 2
                                       )
                               )
                     
                     # , fluidRow( column(8, textOutput('debug'), class = 'well', offset = 2))
                     ) 

runPage   <- tabPanel( 'run'
                     , uiOutput('runPanel')
                     )

app       <- navbarPage( 'modelBuildR'
                       , id = "navbar"
                       , buildPage
                       , runPage
                       , header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "widgets.css"))
                       , footer = div( md('Version 0.1.  [Source code](https://github.com/whitwort/modelBuildR) freely available under [GPLv2](http://www.gnu.org/licenses/gpl-2.0.html).  Powered by [Shiny](http://shiny.rstudio.com/) and [deSolve](http://cran.r-project.org/web/packages/deSolve/index.html).')
                                     , style = 'text-align: center;'
                                     )
                       )

app
