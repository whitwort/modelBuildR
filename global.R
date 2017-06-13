library(jsonlite)
library(deSolve)

source("widgets.R", local = TRUE)

math        <- c("+", "-", "/", "*", "(", ")", "^")
names(math) <- math

# Cross-session constants
timeStepCount      <- 1000
parameterStepCount <- 25
stateStepCount     <- 25

modelSchema <- c( 'name'
                , 'description'
                , 'solver'
                , 'parameters'
                , 'parameterUnits'
                , 'parameterStepSize'
                , 'states'
                , 'stateUnits'
                , 'stateStepSize'
                , 'timeStart'
                , 'timeEnd'
                , 'timeUnits'
                , 'timeSteps'
                , 'equations'
                )

solvers    <- list( ode = ode )

installedModels        <- list.files("models", pattern = ".json", full.names = TRUE)
names(installedModels) <- sapply( installedModels
                                , function(file) {
                                      model <- fromJSON(file)
                                      model$name
                                  }
                                )
