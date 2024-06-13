library(shiny)
library(shinydashboard)
library(tidyverse)
library(magrittr)
library(tidyr)
library(scales)
library(shinyWidgets)
library(formattable)
library(DT)
library(d3heatmap)
library(R.utils)
library(RColorBrewer)
source("R/server_utils.R", local = TRUE)
source("R/ui_utils.R", local = TRUE)

# constants & input files
USUAL_OPTIONS <- c('low', 'exclude', 'high')
DISTANCE_OPTIONS <- c('close', 'exclude', 'far') 
TOTAL_OBJECTIVES <- 10 
DATA <- read_csv('test_data.csv', quoted_na = FALSE)
#HEATMAP_DATA <- read_tsv('data/heatmap_data.tsv')
#NLP <- read_csv('data/nlp_allowed.csv')
NLP <- read_csv('test_data.csv')

ui <- dashboardPage(
  skin = 'purple',
  dashboardHeader(title = "RJH-Metastasis"),
  dashboardSidebar(
    sidebarMenu(
      id = 'mysidebar',
      menuItem(
        "Essentiality",
        tabName = "widgets",
        icon = icon("shapes"),
        radioButtons(
          inputId = 'flt_1',
          label = "depMap",
          choices = list('essential', 'nonessential'),
          selected = 'nonessential'
        )
      ),
      menuItem(
        "Literature support",
        tabName = "widgets",
        icon = icon("graduation-cap"),
        sliderTextInput01(
          "obj_1", 
          "# of papers, gene in Metastasis context + COAD",
          "high"
        )
      ),
      menuItem(
        "Graph-derived",
        tabName = "widgets",
        icon = icon("project-diagram"),
        sliderTextInput01(
          "obj_2", 
          "# uniqie neighbours connected to a node in full KG",
          'exclude'
        ),
        sliderTextInput01(
          "obj_3", 
          "# edges connected to a node in full BIKG",
          'exclude'
        ),
        sliderTextInput01(
          "obj_4", 
          "node degree in PPI subgraph",
          'exclude'
        ),
        sliderTextInput01(
          "obj_5", 
          "pagerank, (~ popularity) of a node in PPI subgraph",
          'high'
        ),
        sliderTextInput01(
          "obj_6", 
          "betweenness (~ node's influence) in PPI subgraph",
          'high'
        )
      ),
      menuItem(
        "Consistency",
        tabName = "widgets",
        icon = icon("check-double"),
        sliderTextInput01(
          'obj_7',
          "screen in M1A",
          'high'
        )
      ),
      menuItem(
        "Preclinical evidence",
        tabName = "widgets",
        icon = icon("dna"),
        sliderTextInput01(
          'obj_8',
          "RNAseq, adjusted p-value",
          'low'
        ),
        sliderTextInput01(
          'obj_9',
          "RNAseq, log2 fold change",
          'high'
        ),
        sliderTextInput01(
          'obj_10',
          "RNAseq, DNB",
          'high'
        )
      ),
      actionBttn(
        inputId = "rank",
        label = "rank!",
        style = "gradient",
        color = "royal",
        icon = icon("random"),
        size = 'sm'
      ),
      actionBttn(
        inputId = "reset",
        label = "reset",
        style = "gradient",
        color = "primary",
        icon = icon("redo-alt"),
        size = 'sm'
      )
    )
  ),
  
  dashboardBody(
    fluidRow(
      box(
        title = tagList(shiny::icon("lightbulb"), "How it works"),
        status = NULL, 
        solidHeader = TRUE,
        collapsible = TRUE,
        includeMarkdown('docs/intro_rjh.md'),
        width = 12),
      tabBox(
        title = 'Optimal hits, tab view',
        id = 'tabset1',
        width = 12,
        tabPanel(
          title = tagList(shiny::icon("list-alt"), "Result"),
          pickerInput(
            inputId = "SortMenu",
            label = "Sort top genes by", 
            choices = list(
              'essentiality (depmap)' = c("depmap"),
              #'tractability' = c("trct_ab", "trct_sm", 'trct_om'),
              'literature support' = c("lit_total"),
              'graph-derived' = c('n_neighbours', 'n_edges','degree', 'pagerank', 'betweenness'),
              'consistency in M1A screens' = c('full_screen_M1A'),
              #'clinical relevance' = c('clin_ES1', 'clin_ES2', 'clin_ES3'),
              'expression' = c('RNAseq_adjpval', 'RNAseq_LFC','DNB')),
            selected = 'full_screen',
            options = list(size = 5)),
          DT::dataTableOutput("TopTable"),
          downloadButton('TopDownload',"Download Top results")),
        tabPanel(
          title = tagList(shiny::icon("question"), "Variables explained"),
          includeMarkdown('docs/variables_explained_rjh.md')
        )
      ),
      # box(
      #   title = 'heatmap controls',
      #   id = 'tabsetNLP',
      #   width = 2,
      #   height="664px",
      #   collapsible = TRUE,
      #   sliderInput("cluster", "min cluster size",
      #               value = 1,
      #               min = 1,
      #               max = 100,
      #               width = '300px'),
      #   sliderInput("papers", "min number of papers with gene cluster",
      #               value = 1,
      #               min = 1,
      #               max = 500,
      #               width = '300px'),
      #   pickerInput(
      #     inputId = "gene",
      #     label = "select a gene", 
      #     choices = NLP$ensembl_gene_id,
      #     selected = 'ENSG00000171862',
      #     choicesOpt = list(
      #       subtext = paste("symbol", 
      #                       NLP$gene,
      #                       sep = ": ")))),
      # box(
      #   title = 'heatmap showing multi-term gene co-occurrence',
      #   id = 'heatmapTab',
      #   width = 10,
      #   collapsible = TRUE,
      #   d3heatmapOutput("heatmap", width = "100%", height = '600px')
      # ),
      tabBox(
        title = 'Optimal hits, relative view',
        id = 'tabset2',
        width = 12,
        tabPanel(
          title = tagList(shiny::icon("list-alt"), "Optimal hits"),
          plotOutput("ParetoBar", height = '1300px')),
        tabPanel(
          title = tagList(shiny::icon("question"), "Plot explained"),
          includeMarkdown('docs/pareto_bar_explained.md')
        )
      ),
      box(
        title = "Full dataset",
        solidHeader = TRUE,
        collapsible = TRUE,
        DT::dataTableOutput("table"),
        width = 12
      ),
      tabBox(
        id = 'tabset0',
        width = 12,
        tabPanel(
          title = tagList(shiny::icon("chart-bar"), "Distribution of Pareto levels"),
          plotOutput("ParetoHist")),
        tabPanel(
          title = tagList(shiny::icon("question"), "Plot explained"),
          includeMarkdown('docs/pareto_hist_explained.md')
        ))
    ))
)

server <- function(input, output, session) {
  
  # data for plots & full table
  genericdata <- reactive({
    
    # depends on rank action button to delay ranking
    input$rank
    
    isolate({
      # extract directions for objectives
      all_objectives <- NULL
      for (i in seq(TOTAL_OBJECTIVES)) {
        all_objectives <- c(all_objectives,
                            input[[paste0("obj_", as.character(i))]])
      }
      
      data.prt <- any_filter(DATA, flt_d = input$flt_1) %>%
        mutate_at(c('RNAseq_LFC'), abs) %>%
        dplyr::select(-c('depmap'))
      
      head(data.prt)
      res <- pareto_helper(data.prt,
                           all_objectives,
                           num_objectives = TOTAL_OBJECTIVES,
                           return_pref = FALSE)
    })
  })
  # top hits for top view and download button
  topdata <- reactive({
    
    res <- genericdata()
    d <- res %>%
      left_join(dplyr::select(DATA,
                              c(gene, ensembl_gene_id, depmap, 
                                contains('RNAseq_LFC'))),
                by = c('gene', 'ensembl_gene_id')) %>%
      mutate(RNAseq_LFC.x = RNAseq_LFC.y ) %>%
      dplyr::rename(RNAseq_LFC = RNAseq_LFC.x) %>%
      dplyr::select(-c(contains('RNAseq_LFC.y')))
  })
  
  output$TopTable <- DT::renderDataTable({
    
    # reuse reactive data, same as for TopDownload
    res <- topdata()
    radio <- input$SortMenu
    
    d <- res %>%
      dplyr::filter(.level == 1) %>%
      dplyr::select(-c(.level)) %>%
      arrange(desc(!!sym(radio)))
    
    ff <- formattable(
      d,
      list(
        #area(col = trct_ab:trct_om) ~ color_bar('#8dd3c7'),
        #area(col = c(lit_total)) ~ color_bar('#9ebcda'), 
        #lit_egfr_norm = color_bar('#9ebcda'),
        #lit_nsclc_norm = color_bar('#9ebcda'),
        lit_total = color_bar('#9ebcda'),
        full_screen_M1A = color_bar('#fb9a99'),
        #area(col = KO_osi:KO_all) ~ color_bar('#fdb863'), 
        #area(col = A_osi:A_all) ~ color_bar('#fee0b6'), 
        #area(col = L2_egfr:L2_nsclc) ~ color_bar('#d8daeb'), 
        area(col = n_neighbours:n_edges) ~ color_bar('#bc80bd'),
        degree = color_bar('#b2df8a'),
        pagerank = color_bar('#b2df8a'),
        betweenness = color_bar('#b2df8a'),
        
        
        #area(col = c(RNAseq_LFC)) ~ sign_formatter,
        area(col = c(RNAseq_LFC)) ~ color_bar('#d8daeb'),
        area(col = c(RNAseq_adjpval)) ~ color_bar('#d8daeb'),
        area(col = c(depmap)) ~ depmap_formatter,
        area(col = c(DNB)) ~ color_bar('#d8daeb')
      )) 
    
    as.datatable(
      ff,
      escape = F,
      rownames= F,
      extensions = list("ColReorder" = NULL,
                        "Buttons" = NULL,
                        "FixedColumns" = list(leftColumns=1)),
      options = list(
        dom = 'BRfrltpi',
        scrollX = TRUE,
        ordering = FALSE,
        lengthMenu = list(c(10, 20, 50, -1), c('10', '20', '50', 'All')),
        ColReorder = TRUE,
        buttons = list(I('colvis')) 
      )
    )
  })
  
  output$TopDownload <- downloadHandler(
    filename = function(){'optimal_genes.csv'},
    content = function(fname){
      write.csv(dplyr::filter(topdata(), .level == 1), fname)
    }
  )
  
}

shinyApp(ui, server)