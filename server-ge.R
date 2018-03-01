get_gene_row_id <- reactive({
    row.id = match(input$ge_name, gene_info$Symbol)
    if (is.na(row.id)) {
        gene_name = c(limma::alias2Symbol(input$ge_name), '')[1]
        row.id = match(gene_name, gene_info$Symbol)
        if (is.na(row.id)) {
            gene_name = toupper(input$ge_name)
            gene_name = c(limma::alias2Symbol(gene_name), '')[1]
            row.id = match(gene_name, gene_info$Symbol)
        }
    }
    row.id
}) %>% debounce(1500)

get_gene_list <- reactive({
    gene_list = str_split(input$ge_gene_list, '\\n')[[1]] %>%
        map_chr(str_trim)

    # remove empty stings
    str_subset(gene_list, '.')
}) %>% debounce(1000)

get_gene_info <- reactive({
    row_id = get_gene_row_id()
    gene_name = gene_info$Symbol[row_id]
    # add alias
    if (gene_name %in% gene_info$Symbol) {
        gene_alias = gene_info$Synonyms[row_id]
        gene_summary = gene_info$Summary[row_id]
    } else {
        gene_alias = '-'
        gene_summary = ''
    }
    list(row_id = row_id,
         gene_name = gene_name,
         gene_alias = gene_alias,
         gene_summary = gene_summary)
})

get_heatmap_gene <- reactive({
    gene_list = get_gene_list()
    if (length(gene_list) < 2) {
        return(list(expr = matrix(NA_real_),
                    unmatched = character(),
                    lowexpr = character()))
    } else {
        gene.expr = matrix()
        gene.unmatched = character()
        # "gene_id", "gene_name", "gene_biotype", "description", "source"

        gene.matched = as.character(intersect(gene_list, gene_info$Symbol))
        gene.unmatched = as.character(setdiff(gene_list, gene_info$Symbol))
        gene.expr = get_dds()$tpm[gene.matched, ]

        if (input$ge_filter) {
            gene.lowexpr <- (rowMaxs(gene.expr) < 1)
            gene.expr = gene.expr[!gene.lowexpr,]
        } else {
            gene.lowexpr = rep(FALSE, nrow(gene.expr))
        }

        return(list(expr = gene.expr,
                    unmatched = gene.unmatched,
                    lowexpr = gene.matched[gene.lowexpr]))
    }
})

output$ge_expr <- renderPlotly({
    dds <- get_dds()$dds
    row_id <- get_gene_info()[['row_id']]
    if (is.na(row_id)) return(list())

    gene_name = get_gene_info()[['gene_name']]
    gene_alias = get_gene_info()[['gene_alias']]
    gene_summary = get_gene_info()[['gene_summary']]
    # gene_name = as.character(rowData(dds)$gene_name[row_id])

    if (gene_alias != '-') {
        plot_title = glue('{gene_name} ({gene_alias})')
    } else {
        plot_title = gene_name
    }

    dds.expr <- get_dds()$tpm
    row.tpm = dds.expr[row_id,]

    if (input$ge_log) {
        row.expr = log1p(row.tpm)
    } else {
        row.expr = row.tpm
    }

    dat <- data.frame(
        TPM = row.tpm,
        expr = row.expr,
        group = fct_drop(pheno[[input$ge_group]][match(colnames(dds),
                                                       pheno$label)]),
        donor = fct_drop(pheno[['donor']][match(colnames(dds),
                                                pheno$label)])
    )
    colnames(dat)[3] = input$ge_group

    expr_plot <- ggplot(dat, aes_string(x = parse_space(input$ge_group),
                                        y = "expr",
                                        TPM = 'TPM',
                                        color = parse_space(input$ge_group),
                                        donor = 'donor')) +
        geom_point(size = input$ge_expr_size) +
        ggtitle(plot_title) +
        xlab("") +
        ylab(ifelse(input$ge_log, 'log2(TPM)', 'TPM')) +
        expand_limits(y = 0) +
        # guides(color = guide_legend(title = input$ge_group)) +
        theme_bw()

    ggplotly(
        expr_plot,
        tooltip = c('TPM', 'donor')
    )
})

# download gene expression value (FPKM or TPM)
output$btn_dl_expr <- downloadHandler(
    filename = function() {
        return("gene_expression.csv")
    },
    content = function(file) {
        gene.info = rowData(get_dds()$dds)
        gene.expr = get_dds()$tpm

        write.csv(cbind(as.data.frame(gene.info)[,c("GeneID",
                                                    "Symbol",
                                                    'Synonyms',
                                                    "description")],
                        as.data.frame(round(gene.expr, digits = 3))),
                  file, row.names = FALSE)
    }
)

output$ge_gene_summary <- renderText({
    get_gene_info()[['gene_summary']]
})

output$ge_d3heatmap <- renderD3heatmap({
    gene.expr = get_heatmap_gene()[['expr']]
    if (is.na(gene.expr[1,1])) {
        return(list())
    }

    hm.color = colorRampPalette(rev(brewer.pal(n = 7, name =
                                                   "RdYlBu")))(100)

    if (input$ge_scale) {
        dist_fun <- function(x, ...) {
            if (length(intersect(rownames(x),
                                 rownames(gene.expr))) > 0) {
                dist(t(scale(t(log1p(x)))), ...)
            } else {
                dist(scale(log1p(x)), ...)
            }
        }
        d3heatmap(gene.expr,
                  distfun = dist_fun,
                  scale = 'row',
                  show_grid = FALSE,
                  colors = hm.color)
    } else {
        dist_fun <- function(x, ...) {
            dist(log1p(x), ...)
        }
        d3heatmap(gene.expr,
                  distfun = dist_fun,
                  show_grid = FALSE,
                  colors = hm.color)
    }
})

output$ge_pheatmap <- renderPlot({
    gene.expr = get_heatmap_gene()[['expr']]
    if (is.na(gene.expr[1,1])) {
        return(list())
    }

    sample_rld <- get_dds()$rld

    anno_df = data.frame(group = pheno[[input$ge_group]][match(colnames(sample_rld), pheno$label)])
    rownames(anno_df) = colnames(sample_rld)
    pheatmap(log1p(gene.expr),
             scale = ifelse(input$ge_scale, 'row', 'none'),
             border_color = NA,
             annotation_col = anno_df,
             fontsize_row = min(10, 250 / length(nrow(gene.expr)))
             )

})

output$ge_heatmap_unmatched <- renderPrint({
    gene.expr = get_heatmap_gene()[['expr']]
    gene.unmatched = get_heatmap_gene()[['unmatched']]
    gene.lowexpr = get_heatmap_gene()[['lowexpr']]
    if (!is.na(gene.expr[1,1])) {
        glue('Number of matched genes: {nrow(gene.expr)}')
    }
    if (length(gene.unmatched) != 0) {
        cat('Unmatched genes:\n')
        with_options(list(width = 20), print(gene.unmatched))
    } else if (length(gene.lowexpr) != 0) {
        cat('Filtered genes:\n')
        with_options(list(width = 20), print(gene.lowexpr))
    } else {
        cat('Everything is OK\n')
    }
})

# update gene expression plot when a gene is selected in the table of
# differential expressed genes
observe(
    if(!is.null(input$table_deg_row_last_clicked)) {
        gene_name = get_deg_deseq()$Symbol[
            input$table_deg_row_last_clicked]
        updateTextInput(session, "ge_name", value = gene_name)
    }
)

observeEvent(input$ge_clear, {
    updateTextAreaInput(session, 'ge_gene_list', value = '')
})

observeEvent(input$ge_list1, {
    updateTextAreaInput(session, 'ge_gene_list',
                        value = paste(readLines('data/example/1.txt'),
                                      collapse = '\n'))
})

observeEvent(input$ge_list2, {
    updateTextAreaInput(session, 'ge_gene_list',
                        value = paste(readLines('data/example/2.txt'),
                                      collapse = '\n'))
})
