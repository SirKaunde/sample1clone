boxoutUI <- function(id){
  ns <- NS(id)
  plotOutput(ns("boxplot"))
}