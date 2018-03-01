tabPanel("Gene Expression Value",
         tags$h2("Gene Expression Scatter Plot", style = 'margin-top: 60px;'),
         hr(),
         sidebarLayout(
             sidebarPanel(
                 selectizeInput(inputId = 'ge_group',
                                label = 'Group samples by',
                                choices = c('donor', 'cell type',
                                            'surface marker', 'stimulation'),
                                selected = 'cell type'),

                 conditionalPanel(
                     condition = 'input.tabset_ge == "gene expression"',
                     textInput(inputId = "ge_name",
                               label = "Gene name (e.g. AXL)",
                               value = "AXL"),
                     hr(),
                     checkboxInput(inputId = "ge_log",
                                   label = "log scale",
                                   value = FALSE),
                     sliderInput(inputId = "ge_expr_size",
                                 label = "Point size",
                                 value = 3,
                                 min = 1,
                                 max = 10,
                                 step = 1)
                 ),

                 conditionalPanel(
                     condition = 'input.tabset_ge == "heatmap"',
                     checkboxInput(
                         inputId = 'ge_scale',
                         label = 'center and scale gene expression value',
                         value = TRUE
                     ),
                     checkboxInput(
                         inputId = 'ge_filter',
                         label = 'filter out low-expressed genes',
                         value = TRUE
                     ),
                     fluidRow(
                         column(width = 9,
                                textAreaInput(
                                    inputId = 'ge_gene_list',
                                    label = 'Gene list',
                                    height = '400px',
                                    placeholder = 'Your awesome gene list'
                                )),
                         column(width = 3,
                                column(width = 3,
                                       actionLink(inputId = 'ge_clear',
                                                  label = 'clear'),
                                       hr(),
                                       actionLink(inputId = 'ge_list1',
                                                  label = 'List 1'),
                                       br(),
                                       actionLink(inputId = 'ge_list2',
                                                  label = 'List 2'))
                         )
                     ),
                     hr(),
                     verbatimTextOutput('ge_heatmap_unmatched')
                 )
             ),

             mainPanel(
                 tabsetPanel(id = "tabset_ge",
                     tabPanel('gene expression',
                              downloadButton(outputId = "btn_dl_expr",
                                             label = 'Download gene expression table'),
                              plotlyOutput('ge_expr'),
                              hr(),
                              textOutput('ge_gene_summary')
                     ),
                     tabPanel('heatmap',
                              d3heatmapOutput('ge_d3heatmap'),
                              hr(),
                              plotOutput('ge_pheatmap')))
         ))
)
