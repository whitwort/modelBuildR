library(shiny)

buildPage <- tabPanel( 'build'
                     , h2('Build a model')
                     , p("You can use this page to build a mathematical model based on a set of time resolved differential equations.  When you've finished you can run simulations using your model on the 'Run' tab above.")
                     
                     , h3('Model')
                     , fluidRow( column( 8
                                       , textInput('name', 'Name', value = '')
                                       , textareaInput('description', 'Description')
                                       , offset = 2
                                       , class  = 'well'
                                       )
                               )
                     
                     , h3('Model Variables')
                     , fluidRow( column( 4
                                       , h4('Parameters')
                                       , p("Parameters are variables that can be set before the simulation is run but don't change over time.  Useful for fixed rates like kinetic constants.")
                                       , uiOutput('parametersUI')
                                       , textInput('parameterUnits', 'Units', value = 's-1', verticalLabel = FALSE)
                                       , class = 'well'
                                       )
                               , column( 4
                                       , h4('States')
                                       , p('State variables start with initial values but change over time in the simulation.  Useful for tracking things like molecular concentrations or populations.')
                                       , uiOutput('statesUI')
                                       , textInput('stateUnits', 'Units', value = 'mM', verticalLabel = FALSE)
                                       , class = 'well'
                                       )
                               , column( 4
                                       , h4('Time scale')
                                       , p('Define the minimum and maximum useful time scales to use when running the simulation.')
                                       , numericInput('timeStart', 'Minimum', value = 1, min = 0, verticalLabel = FALSE)
                                       , numericInput('timeEnd', 'Maximum', value = 10, min = 0, verticalLabel = FALSE)
                                       , textInput('timeUnits', 'Units', value = 's', verticalLabel = FALSE)
                                       , class = 'well'
                                       )
                               )
                     , fluidRow( column( 8, textOutput('debug'), class = 'well', offset = 2))
                     )

runPage   <- tabPanel( 'run'
                     )

app       <- navbarPage( 'modelBuildR'
                       , buildPage
                       , runPage
                       , footer = div( md('Version 0.1.  [Source code](https://github.com/whitwort/modelBuildR) freely available under [GPLv2](http://www.gnu.org/licenses/gpl-2.0.html).  Powered by [Shiny](http://shiny.rstudio.com/) and [deSolve](http://cran.r-project.org/web/packages/deSolve/index.html).')
                                     , style = 'text-align: center;'
                                     )
                       , responsive = TRUE
                       )

shinyUI(app)
