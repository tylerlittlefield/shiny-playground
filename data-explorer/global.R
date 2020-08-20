library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(waiter)
library(reactable)
library(dplyr)

utils <- list.files("R", full.names = TRUE)
invisible(lapply(utils, source))

config <- list(
  data_choices = list.files("data")
)
