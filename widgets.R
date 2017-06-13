library(shiny)
library(markdown)
library(magrittr)

## Fixes, enhacements and additions to the stock shiny widget set

# HTML elements
md <- . %>% markdownToHTML(text = ., fragment.only = TRUE) %>% HTML

# Stock Shiny widget API extensions
textInput <- function(inputId, label = NULL, value = "", horizontal = FALSE, class = "form-control") {
    
    if (!is.null(label)) label <- tags$label(label, `for` = inputId)
    
    input <- tags$input(id = inputId, type = "text", value = value, class = class)
    divClass <- "form-group shiny-input-container"
    
    if (horizontal) {
        fluidRow(column(3, label), column(9, input), class = divClass)
    } else {
        div(class = divClass, label, input)
    }
    
}

numericInput <- function(inputId, label = NULL, value = "", min = NA, max = NA, step = NA, horizontal = FALSE, class = "form-control") {
    
    if (!is.null(label)) label <- tags$label(label, `for` = inputId)
    
    input <- tags$input(id = inputId, type = "number", class = class, value = shiny:::formatNoSci(value))
    if (!is.na(min) ) input$attribs$min  = min
    if (!is.na(max) ) input$attribs$max  = max
    if (!is.na(step)) input$attribs$step = step
    
    divClass <- "form-group shiny-input-container"
    
    if (horizontal) {
        fluidRow(column(3, label), column(9, input), class = divClass)
    } else {
        div(class = divClass, label, input)
    }
    
}

downloadButton <- function (outputId, label = "Download", class = NULL, icon = icon("download"), ...) {
    aTag <- tags$a( id      = outputId
                  , class   = paste("btn btn-default shiny-download-link", class)
                  , href    = ""
                  , target  = "_blank"
                  , icon    
                  , label
                  , ...
                  )
}


# Custom controls
panel <- function(heading, ..., class = 'panel-default') {
    div( div(heading, class = "panel-heading")
       , div(..., class = "panel-body")
       , class = paste('panel', class)
       )
}

textareaInput <- function(inputId, label = NULL, value = "", rows = 4, horizontal = FALSE, class = NULL, ...) {
    
    if (!is.null(label)) label <- tags$label(label, `for` = inputId)
    class <- paste("form-control", class)
    
    input <- tags$textarea(id = inputId, class = class, value = value, rows = rows, ...)
    divClass <- "form-group shiny-input-container"
    
    if (horizontal) {
        fluidRow(column(2, label), column(10, input), class = divClass)
    } else {
        div(class = divClass, label, input)
    }
    
}

dropdownInput <- function(inputId, label, choices, class = NULL, icon = NULL, ...) {
    names <- if (is.null(names(choices))) { 1:length(choices) } else { names(choices) }
    choices <- as.list(choices)
    
    div( id     = inputId
       , class  = "btn-group shiny-input-container dropdown-input"
       , tags$button( type  = "button"
                    , class = paste("btn btn-default dropdown-toggle", class)
                    , `data-toggle`  = "dropdown"
                    , `aria-expanded` = "false"
                    , icon
                    , label
                    , tags$span(class = "caret")
                    )
       , tags$ul( class = "dropdown-menu"
                , role  = "menu"
                , lapply( names
                        , function(name) {
                            if (is.na(name) || is.na(choices[[name]])) {
                                tags$li(class = "divider")
                            } else if (is(choices[[name]], 'shiny.tag')) {
                                tags$li(choices[[name]])
                            }else {
                                tags$li(tags$a( href  = "#"
                                              , value = choices[[name]]
                                              , choices[[name]]
                                              )
                                        )
                            }
                          }
                        )
                )
       )
    
}
registerInputHandler("modelBuildR.dropdown-input", function(data, shinysession, name) {
    data
}, force = TRUE)

vectorInput <- function(inputId, type = 'text', nameLabel = 'name', valueLabel = 'value', values = NULL) {
    
    tagList( singleton(tags$head(tags$script(src = 'widgets.js')))
           , singleton(tags$head(tags$link(href = 'widgets.css')))
           , singleton(tags$head(tags$script(src = 'handlebars.runtime-v2.0.0.js')))
           , singleton(tags$head(tags$script(src = 'templates.js')))

           , div( id            = inputId
                , class         = 'vector-input'
                , type          = type
                , nameLabel     = nameLabel
                , valueLabel    = valueLabel
                , values        = values
                )
           )
    
}
registerInputHandler("modelBuildR.vector-input", function(data, shinysession, name) {
    
    values <- as.numeric(sapply(data, function(row) { row$value } ))
    names(values) <- sapply(data, function(row) { row$name } )
    
    values
    
}, force = TRUE) # TODO: the need to do this even between restarts seems like a bug...

#  WARNING this widget may not work if there are other selectize widgets on the
#  page causing main branch selectize to be included; we ship the
#  allow-duplicates branch.  The current implementation is also very app specific.
equationEditor <- function(inputId, description = "", values = NULL) {
    tagList( singleton(tags$head(tags$script(src = 'widgets.js')))
           , singleton(tags$head(tags$link(href = 'widgets.css')))
           , singleton(tags$head(tags$script(src = 'handlebars.runtime-v2.0.0.js')))
           , singleton(tags$head(tags$script(src = 'templates.js')))
           , singleton(tags$head(tags$link(href = 'selectize.bootstrap3.css', rel = 'stylesheet')))
           , singleton(tags$head(tags$script(src = 'selectize.js'))) # supports duplicates option
           
           , div( id            = inputId
                , class         = 'equation-editor'
                , math          = paste(math, collapse = ",")
                , values        = values
                , description   = description
                )
           )
}

registerInputHandler("modelBuildR.equation-editor", function(data, shinysession, name) {
    if (length(data) > 0) {
        for (i in 1:length(data)) {
            if (is.null(data[[i]])) {
                data[[i]] <- NA
            }
        }
    }
    data
}, force = TRUE)

renderEquationErrors <- function(expr, env = parent.frame(), quoted = FALSE) {
    installExprFunction(expr, "validationFunction", env, quoted)
    markRenderFunction(equationEditor, function() { validationFunction() } )
}



