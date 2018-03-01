tabPanel("Exploratory Analysis",
         tags$h2("PCA & Clustering", style = 'margin-top: 60px;'),
         hr(),
         sidebarLayout(
             sidebarPanel(
                 selectizeInput(inputId = 'eda_pca_color',
                                label = 'Color by',
                                choices = c('cell type', 'donor',
                                            'surface marker', 'stimulation')
                                ),
                 selectizeInput(inputId = 'eda_pca_shape',
                                label = 'Shape by',
                                choices = c('donor', 'cell type',
                                            'surface marker', 'stimulation')
                                ),
                 sliderInput(inputId = "eda_gene_num",
                             label = "Number of high-variable genes used",
                             min = 100,
                             max = 2000,
                             value = 500,
                             step = 100),
                 conditionalPanel(
                     "input.tabset_eda == 'principal components analysis'",
                     sliderInput(inputId = "eda_pca_size",
                                  label = "Point size",
                                  value = 3,
                                  min = 1,
                                  max = 10,
                                  step = 1)),
                 conditionalPanel(
                     "input.tabset_eda == 'correlation matrix'",
                     selectizeInput(inputId = "eda_cor_method",
                                    label = "Correlation coefficient",
                                    choices = c("pearson",
                                                "kendall",
                                                "spearman"),
                                    selected = "pearson")),
                 conditionalPanel(
                     "input.tabset_eda == 'heatmap'",
                     selectizeInput(inputId = "eda_hclust_method",
                                    label = "Linkage function",
                                    choices = c("ward.D2", "single",
                                                "complete",
                                                "average"),
                                    selected = "complete"))
             ),

             mainPanel(
                 tabsetPanel(id = "tabset_eda",
                     tabPanel('principal components analysis',
                              plotlyOutput('plot_eda_pca')),
                     tabPanel('correlation matrix',
                              plotlyOutput('plot_eda_cor')),
                     tabPanel('heatmap',
                              plotOutput('plot_eda_hm'))
                 )
             )
         ))
