#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(DT)
library(tximport)
library(DESeq2)
library(ggplot2)
library(plotly)
library(pheatmap)
library(d3heatmap)
library(genefilter)
library(tidyverse)
library(glue)
library(withr)
library(readxl)
library(DEFormats)
library(edgeR)
library(pheatmap)
library(RColorBrewer)

# pheno <- readxl::read_excel("data/pheno/pheno.xlsx", sheet = 1) %>%
#     replace_na(list(stimulation = '-'))
# write_rds(pheno, path = 'data/pheno/pheno.rds')
pheno <- read_rds('data/pheno/pheno.rds')
tx2gene <- read_tsv('data/gene/gene.map',
                    col_names = c('tx_id', 'gene_name'),
                    col_types = 'cc')
gene_info <- read_rds('data/gene/gene_info.rds')
# load('data/gene/human.RData')

# If a column of the dataframe has only null value, then drop this column
dropInvalidColumn <- function(df) {
    df[,sapply(df, function(x){
        sum((!is.na(x)) & (x != "")) > 0
    })]
}

parse_space <- function(x) {
    if (str_detect(x, fixed(' '))) {
        x = glue("`{x}`")
    }
    x
}

# Needed for `plotly` when used in linux server
if (names(dev.cur()) != "null device") { dev.off() }
pdf(NULL)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    observeEvent(input$project, {
        if (input$project == '') {
            show('group_var')
            show('confounding_var')
        } else {
            hide('group_var')
            hide('confounding_var')
        }
    })

    source("server-ss.R", local = TRUE)
    source("server-eda.R", local = TRUE)
    source("server-ge.R", local = TRUE)
    source("server-de.R", local = TRUE)
})
