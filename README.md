# HGMD

hgmd_pro.R

```R
library(shiny)
library(DT)
load("hgmd_pro_2023.04_allmut.RData")
load("hgmd_pro_2023.04_extrarefs.RData")


ui <- fluidPage(
  titlePanel("HGMD Pro Database 2023.04"),
  textInput("gene_input", "Enter Gene Symbol:"),
  dataTableOutput("table"),
  dataTableOutput("table2"),
  tags$footer(
    style = "text-align: center; padding: 10px; background-color: #f5f5f5;",
    "last updated 13Feb2024 14:48h    -    questions to jean.elbers@meduniwien.ac.at    -     with help from Vivienne Arnold :)")
)


server <- function(input, output) {
  gene_query <- reactive({
    if (is.null(input$gene_input) || input$gene_input == "") {
      return(NULL)
    } else {
      input$gene_input
    }
  })
  output$table <- renderDataTable({
    fetched5 <- fetched[toupper(fetched$gene) == toupper(gene_query()),]
    fetched5$disease <- as.factor(fetched5$disease)
    fetched5$gene <- as.factor(fetched5$gene)
    fetched5$chrom <- as.factor(fetched5$chrom)
    fetched5$genename <- as.factor(fetched5$genename)
    fetched5$gdbid <- as.factor(fetched5$gdbid)
    fetched5$omimid <- as.factor(fetched5$omimid)
    fetched5$amino <- as.factor(fetched5$amino)
    fetched5$deletion <- as.factor(fetched5$deletion)
    fetched5$insertion <- as.factor(fetched5$insertion)
    fetched5$descr <- as.factor(fetched5$descr)
    fetched5$refseq <- as.factor(fetched5$refseq)
    fetched5$hgvs <- as.factor(fetched5$hgvs)
    fetched5$hgvsAll <- as.factor(fetched5$hgvsAll)
    fetched5$dbsnp <- as.factor(fetched5$dbsnp)
    fetched5$chromosome <- as.factor(fetched5$chromosome)
    fetched5$expected_inheritance <- as.factor(fetched5$expected_inheritance)
    fetched5$tag <- as.factor(fetched5$tag)
    fetched5$dmsupport <- as.factor(fetched5$dmsupport)
    fetched5$mutype <- as.factor(fetched5$mutype)
    fetched5$author <- as.factor(fetched5$author)
    fetched5$title <- as.factor(fetched5$title)
    fetched5$fullname <- as.factor(fetched5$fullname)
    fetched5$allname <- as.factor(fetched5$allname)
    fetched5$vol <- as.factor(fetched5$vol)
    fetched5$page <- as.factor(fetched5$page)
    fetched5$pmid <- as.factor(fetched5$pmid)
    fetched5$pmidAll <- as.factor(fetched5$pmidAll)
    fetched5$reftag <- as.factor(fetched5$reftag)
    fetched5$comments <- as.factor(fetched5$comments)
    fetched5$acc_num <- as.factor(fetched5$acc_num)
    fetched5$new_date <- as.factor(fetched5$new_date)
    fetched5$base <- as.factor(fetched5$base)
    fetched5$clinvarID <- as.factor(fetched5$clinvarID)
    fetched5$clinvar_clnsig <- as.factor(fetched5$clinvar_clnsig)
         data <- tryCatch(DT::datatable(fetched5,
                    colnames = c("disease", "gene", "chrom", "genename", "gdbid", "omimid", "amino", "deletion", "insertion", "codon", "codonAff", "descr", "refseq", "hgvs", "hgvsAll", "dbsnp", "chromosome", "startCoord", "endCoord", "expected_inheritance", "gnomad_AC", "gnomad_AF", "gnomad_AN", "tag", "dmsupport", "rankscore", "mutype", "author", "title", "fullname", "allname", "vol", "page", "year", "pmid", "pmidAll", "reftag", "comments", "acc_num", "new_date", "base", "clinvarID", "clinvar_clnsig"),
                    class = 'cell-border stripe',
                            rownames = FALSE,
                            extensions = c('FixedColumns', 'FixedHeader', 'Scroller'),
                            selection = "single",
                            options = list(autoWidth = TRUE,
                                           scrollX = TRUE,
                                           deferRender = TRUE,
                                           fixedColumns = list(leftColumns=0),
                                           fixedHeader = TRUE,
                                           scrollY = 200,
                                           scroller = TRUE),
                    escape = FALSE,
                    filter = "top",
                    caption = "Table 1. HGMD Pro 2023.04 data filtered by Gene Symbol"), error=function(e){})
    return(data)
  })
  fetched3$pmid <- paste0('<a href="https://pubmed.ncbi.nlm.nih.gov/', fetched3$pmid, '">', fetched3$pmid, '</a>')
  output$table2 <- renderDataTable({
        req(input$table_rows_selected)
        fetched4 <- fetched[toupper(fetched$gene) == toupper(input$gene_input), ]
        selected_acc_num <- fetched4[input$table_rows_selected, "acc_num"]
        filtered_acc_num <- fetched3[fetched3$acc_num == selected_acc_num, ]
        filtered_acc_num$acc_num <- as.factor(filtered_acc_num$acc_num)
        filtered_acc_num$disease <- as.factor(filtered_acc_num$disease)
        filtered_acc_num$gene <- as.factor(filtered_acc_num$gene)
        filtered_acc_num$author <- as.factor(filtered_acc_num$author)
        filtered_acc_num$title <- as.factor(filtered_acc_num$title)
        filtered_acc_num$journal <- as.factor(filtered_acc_num$journal)
        filtered_acc_num$fullname <- as.factor(filtered_acc_num$fullname)
        filtered_acc_num$allname <- as.factor(filtered_acc_num$allname)
        filtered_acc_num$vol <- as.factor(filtered_acc_num$vol)
        filtered_acc_num$issue <- as.factor(filtered_acc_num$issue)
        filtered_acc_num$page <- as.factor(filtered_acc_num$page)
        filtered_acc_num$year <- as.factor(filtered_acc_num$year)
        filtered_acc_num$pmid <- as.factor(filtered_acc_num$pmid)
        filtered_acc_num$comments <- as.factor(filtered_acc_num$comments)
        filtered_acc_num$support <- as.factor(filtered_acc_num$support)
        filtered_acc_num$reftag <- as.factor(filtered_acc_num$reftag)
        data4 <- DT::datatable(filtered_acc_num,
                    colnames = c("acc_num", "disease", "gene", "author", "title", "journal", "fullname","allname", "vol", "issue", "page", "year", "pmid", "comments", "support", "reftag"),
                    class = 'cell-border stripe',
                            rownames = FALSE,
                            extensions = c('FixedColumns', 'FixedHeader', 'Scroller'),
                            selection = list(mode = 'single', selected = rownames(filtered_acc_num[1])),
                            options = list(autoWidth = TRUE,
                                           scrollX = TRUE,
                                           deferRender = TRUE,
                                           fixedColumns = list(leftColumns=0),
                                           fixedHeader = TRUE,
                                           scrollY = 200,
                                           scroller = TRUE),
                    escape = FALSE,
                    filter = "top",
                    caption = "Table 2. Supporting Information of Selected Row's Acc_num for Table 1")
   
         return(data4)
  })
}
options(browser="firefox")
app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = FALSE, host = "0.0.0.0", port=4018)
``
