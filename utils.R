restart <- function() { 
    compile()
    system('touch restart.txt') 
}

compile <- function(from = "templates/", to = "www/templates.js") {
    system(paste('handlebars -m ', from, "> ", to, sep = ""))
}
