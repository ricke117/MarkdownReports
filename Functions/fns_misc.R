# Simple function for treating data varaibles in shiny input$ selections
# as variables in dplyr, tidyverse
shinydply <- function(x){ return(!!rlang::sym(x)) }