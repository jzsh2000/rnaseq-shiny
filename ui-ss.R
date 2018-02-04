# bootstrap's button dropdowns
# reference: http://stackoverflow.com/a/34633977
dropdownButton <- function(label = "",
                           status = c("default", "primary", "success", "info",
                                      "warning", "danger"), ..., width = NULL) {

  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width))
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li,
           style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button",
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}

tabPanel("Sample Selector",
    tags$h2("Select RNA-seq Datasets", style = 'margin-top: 60px;'),
    # shinythemes::themeSelector(),
    hr(),
    tabsetPanel(
        id = "tabset_sample",
        selected = "selected",
        tabPanel("selected",
                 fluidRow(
                     column(width = 6,
                            selectizeInput(
                                inputId = "project",
                                label = NULL,
                                choices = NULL,
                                options = list(
                                    placeholder = 'select an project below',
                                    onInitialize = I('function() { this.setValue(""); }')
                                )
                            )),
                     column(width = 2,
                            dropdownButton(label = "jump to",
                                           status = "primary",
                            actionLink('jump_pca',
                                       'principal components analysis'),
                            actionLink('jump_expr',
                                       'gene expression value')
                            )),
                     column(width = 4,
                            actionButton('btn_reset', 'reset selection',
                                         class = "btn-warning"))
                 ),
                 dataTableOutput("table_pheno_selected")),

        tabPanel("available",
                 fluidRow(
                     column(width = 4,
                            span('Click on RNA-seq records to select samples')),
                     column(width = 4,
                            downloadButton('btn_dl_pheno',
                                           'Download sample table'))
                 ),

                 dataTableOutput("table_pheno_all")
        )
    )
)
