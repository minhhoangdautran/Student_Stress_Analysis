# global.R
library(shiny)
library(shinydashboard)
library(fresh)
library(plotly)
library(dplyr)

# 1. Define your "Source of Truth"
green  <- "#C5D89D" # green pastel
red    <- "#EA7B7B"
blue   <- "#434E78"
yellow <- "#F8FAB4"
orange <- "#FFC7A7"
purple <- "#A2AADB"
pink <- "#FFDCDC"

# 2. Create the theme
my_theme <- create_theme(
  adminlte_color(
    green = "#C5D89D",
    red = "#EA7B7B",
    blue = "#434E78",
    yellow = "#F8FAB4",
    orange = "#FFC7A7",
    purple = "#A2AADB"
  )
)