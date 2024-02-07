rm(list = ls())

packages = c("shiny", "ggplot2", "dplyr", "shinyjs",
             "DT", "kableExtra", "MASS", "shinyWidgets",
             "shinydashboard", "shinydashboardPlus", "scales",
             "reshape", "shinyBS", "knitr", "shinycssloaders",
             "markdown")



testin <- function(package){
  if(!(package %in% installed.packages())){
    install.packages(package)
  }  
}


#lapply(packages, testin)
lapply(packages, require, character.only = TRUE)


library(shinyjs)
library(ggplot2)
library(dplyr)
library(scales)
library(reshape)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyBS)
library(kableExtra)
library(knitr)
library(markdown)
library(MASS)
library(shinycssloaders)


source("Functions.R")



