library(shiny)
library(markdown)
library(magrittr)

## Fixes, enhacements and additions to the stock shiny widget set

# Markdown element from text string
md <- . %>% markdownToHTML(text = ., fragment.only = TRUE) %>% HTML

# textarea input
stretchy <- 'width: 100%; max-width: 100%; height: 100%; box-sizing: border-box;'

textareaInput <- function(inputId, label, value = "", rows = 4, stretch = TRUE, verticalLabel = TRUE, style = NULL) {
    
    if (stretch) style <- paste(style, stretchy, sep = "")
    
    label <- tags$label(label, `for` = inputId)
    text  <- tags$textarea(id = inputId, rows = rows, style = style)
    
    if (verticalLabel) {
        tagList(label, text)
    } else {
        fluidRow(column(4, label), column(8, text ))
    }
    
}

textInput <- function(inputId, label, value = "", stretch = TRUE, verticalLabel = TRUE, style = NULL) {
    
    if (stretch) style <- paste(style, stretchy, sep = "")
    
    label <- tags$label(label, `for` = inputId)
    text  <- tags$input(id = inputId, type = "text", value = value, style = style)
    
    if (verticalLabel) {
        tagList(label, text)
    } else { 
        fluidRow(column(4, label), column(8, text ))
    }
    
}

numericInput <- function(inputId, label, value, min = NA, max = NA, step = NA, stretch = TRUE, verticalLabel = TRUE, style = NULL) {
    
    if (stretch) style <- paste(style, stretchy, sep = "")
    
    # hacked from shiny numericInput
    inputTag <- tags$input(id = inputId, type = "number", value = shiny:::formatNoSci(value))
    if (!is.na(min))  inputTag$attribs$min  = min
    if (!is.na(max))  inputTag$attribs$max  = max
    if (!is.na(step)) inputTag$attribs$step = step
    # 
    
    inputTag$attribs$style = style
    
    label <- tags$label(label, `for` = inputId)
    
    if (verticalLabel) {
        tagList(label, inputTag)
    } else { 
      fluidRow( column(4, label), column(8, inputTag ))
    }
    
}

actionButton <- function(inputId, label, icon = NULL, ...) {
    
    if (is.character(icon)) icon <- icon(icon)
    
    tags$button( id    = inputId
               , type  = "button"
               , class = "btn action-button"
               , list(icon, label)
               , ...
               )
}

inputButton <- function(inputId, label, icon = NULL, ...) {
    
    if (is.character(icon)) icon <- icon(icon)
    
    tagList( singleton(tags$head(tags$script(src = "widgets.js")))
           , tags$button( id    = inputId
                        , type  = "button"
                        , class = "btn input-button"
                        , list(icon, label)
                        , ...
                        )
           )
    
}
