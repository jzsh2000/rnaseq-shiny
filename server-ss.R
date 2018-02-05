# get selected row
get_rowid <- reactive({
    if (length(input$table_pheno_all_rows_selected) >= 1) {
        sample_rowid = input$table_pheno_all_rows_selected
        updateSelectizeInput(session, inputId = "project",
                             selected = "")
    } else {
        sample_rowid = numeric()
    }

    sample_rowid
}) %>% debounce(1500)

observeEvent(input$jump_pca, {
                 if (length(get_rowid()) == 0) {
                     showNotification("Please select samples",
                                      type = "error")
                 }
                 else {
                     updateNavlistPanel(session,
                                        inputId = "main-ui",
                                        selected = "Exploratory Analysis")
                     updateTabsetPanel(session,
                                       inputId = "tabset_eda",
                                       selected = "principal components analysis")
                 }
             })

observeEvent(input$jump_expr, {
                 if (length(get_rowid()) == 0) {
                     showNotification("Please select samples",
                                      type = "error")
                 }
                 else {
                     updateNavlistPanel(session,
                                        inputId = "main-ui",
                                        selected = "Gene Expression Value")
                 }
             })

proxy = dataTableProxy('table_pheno_all')

observeEvent(input$btn_reset, {
    updateSelectizeInput(session, "project", selected = "")
    updateSelectizeInput(session, "deg_contrast", choices = "", selected = "")
    proxy %>% selectRows(NULL)
})

output$table_pheno_selected <- renderDataTable({
    sample_rowid <- get_rowid()
    pheno[sample_rowid, ]
    },
    options = list(
        paging = FALSE,
        dom = 't'
    ),
    selection = "none")

output$table_pheno_all <- DT::renderDataTable(
    pheno,
    filter = 'top',
    extensions = 'Buttons',
    rownames = FALSE,
)

output$btn_dl_pheno <- downloadHandler(
    filename = "sample_table.xlsx",
    content = function(file) {
        file.copy('data/pheno/pheno.xlsx', file)
    },
    contentType = "application/vnd.ms-excel"
)
