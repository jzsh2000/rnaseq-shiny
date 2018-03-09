# generate DESeqDataSet object and its rlog form
get_dds <- reactive({
    sample_rowid = get_rowid()
    validate(need(length(sample_rowid) > 0, "Please select samples"))

withProgress(
    message = "Process gene expression value",
    value = 0, {

    incProgress(detail = "prepare sample table", amount = 0.1)
    sample_df = pheno %>%
        filter(row_number() %in% sample_rowid) %>%
        dplyr::select(label, donor, `cell type`) %>%
        mutate(donor = fct_drop(donor),
               `cell type` = fct_drop(`cell type`))

    choices_candidate = c('donor', 'cell type')
    choices_valid <- sapply(choices_candidate, function(x) {
        length(levels(sample_df[[x]])) >= 2
    })
    choices_valid = choices_candidate[choices_valid]

    if ('cell type' %in% choices_valid) {
        updateSelectizeInput(session, 'eda_pca_color',
                             choices = choices_valid,
                             selected = 'cell type')
        updateSelectizeInput(session, 'ge_group',
                             choices = choices_valid,
                             selected = 'cell type')
    } else {
        updateSelectizeInput(session, 'eda_pca_color',
                             choices = choices_valid)
        updateSelectizeInput(session, 'ge_group',
                             choices = choices_valid)
    }

    if ('donor' %in% choices_valid) {
        updateSelectizeInput(session, 'eda_pca_shape',
                             choices = choices_valid,
                             selected = 'donor')
        updateSelectizeInput(session, 'confounding_var',
                             choices = c('(none)', choices_valid),
                             selected = 'donor')
    } else {
        updateSelectizeInput(session, 'eda_pca_shape',
                             choices = choices_valid)
        updateSelectizeInput(session, 'confounding_var',
                             choices = c('(none)', choices_valid))
    }

    sample_files = set_names(
        file.path('data/salmon', sample_df$label, 'quant.sf'),
        sample_df$label
    )

    incProgress(detail = "read Salmon output", amount = 0.1)

    sample_txi <- tximport(sample_files,
                           type = 'salmon',
                           tx2gene = tx2gene,
                           importer = read_tsv)

    incProgress(detail = 'create DESeq2 dataset', amount = 0.2)

    dds <- DESeqDataSetFromTximport(
        sample_txi,
        colData = sample_df,
        design = ~label
    )
    rowData(dds) <- gene_info

    incProgress(detail = "normalize expression value", amount = 0.2)

    rld = rlog(dds)

    incProgress(detail = "count TPM", amount = 0.3)

    dds.tpm = sample_txi$abundance

    setProgress(1)
})
list(dds = dds, rld = rld, tpm = dds.tpm)
})

output$plot_eda_pca <- renderPlotly({
    sample_rld <- get_dds()$rld

    sample_pca_data <- plotPCA(sample_rld,
                    intgroup = c('label'),
                    ntop = input$eda_gene_num,
                    returnData = TRUE)
    percentVar <- round(100 * attr(sample_pca_data, "percentVar"),
                        digits = 1)
    sample_pca_data <- sample_pca_data %>%
        as_data_frame() %>%
        mutate(label = as.character(label)) %>%
        dplyr::select(label, PC1, PC2) %>%
        left_join(pheno, by = 'label') %>%
        dplyr::select(label, donor, `cell type`, PC1, PC2)
    colnames(sample_pca_data) = make.names(colnames(sample_pca_data))

    p <- ggplot(sample_pca_data,
                aes_string(x = "PC1", y = "PC2",
                           color = make.names(input$eda_pca_color),
                           shape = make.names(input$eda_pca_shape))) +
        geom_point(size = input$eda_pca_size, alpha = 0.7) +
        xlab(paste0("PC1: ", percentVar[1], "% variance")) +
        ylab(paste0("PC2: ", percentVar[2], "% variance")) +
        coord_fixed() +
        theme_bw()

    ggplotly(p, tooltip = c(make.names(input$eda_pca_color),
                            make.names(input$eda_pca_shape)
                            )
             )

})

output$plot_eda_cor <- renderPlotly({
    rld <- get_dds()$rld
    gene.top = order(-rowVars(assay(rld)))[seq_len(input$eda_gene_num)]
    cor.matrix = cor(assay(rld)[gene.top,], method = input$eda_cor_method)
    heatmap.obj = pheatmap(cor.matrix, silent = TRUE)
    cor.order = heatmap.obj$tree_row$order
    cor.label = heatmap.obj$tree_row$labels[cor.order]
    plot_ly(x = cor.label,
            y = cor.label,
            z = unname(cor.matrix[cor.order, cor.order]),
            colorscale = "Greys",
            type = "heatmap")  %>%
        layout(xaxis = list(categoryorder = "trace"),
               yaxis = list(categoryorder = "trace"))
})

output$plot_eda_hm <- renderPlot({
    rld <- get_dds()$rld
    gene.top = order(-rowVars(assay(rld)))[seq_len(input$eda_gene_num)]
    # print(colData(rld))
    if (input$eda_pca_color %in% colnames(colData(rld))) {
        anno_df = colData(rld)[input$eda_pca_color]
        pheatmap(assay(rld)[gene.top,],
                 scale = "row",
                 show_rownames = FALSE,
                 annotation_col = as.data.frame(anno_df),
                 clustering_method = input$eda_hclust_method)
    } else {
        pheatmap(assay(rld)[gene.top,],
                 scale = "row",
                 show_rownames = FALSE,
                 clustering_method = input$eda_hclust_method)
    }
})
