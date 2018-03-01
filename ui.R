#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(tidyverse)
library(readxl)
library(plotly)
library(d3heatmap)
library(DT)

pheno <- read_rds('data/pheno/pheno.rds')
# pheno.column = setdiff(colnames(pheno), "source")
# names(pheno.column) = gsub("_", " ", pheno.column)
# pheno.bgi.name = subset(pheno, source == "BGI")$filename
# pheno.pku.name = subset(pheno, source == "PKU")$sample_id
# names(pheno.pku.name) = subset(pheno, source == "PKU")$filename

shinyUI(
    fluidPage(
        useShinyjs(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),

        navbarPage(
            id = "main-ui",
            title = "RNA-seq Datasets",
            theme = shinythemes::shinytheme("cerulean"),
            position = 'fixed-top',

            source("ui-ss.R", local = TRUE)$value,
            source("ui-eda.R", local = TRUE)$value,
            source("ui-ge.R", local = TRUE)$value,
         )
    )
