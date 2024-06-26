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
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        includeMarkdown('docs/intro_rjh.md'),
        width = 12),
      tabBox(
        title = 'optimal genes (Pareto level 1)结果展示',
        id = 'tabset0',
        width = 12,
        tabPanel(
          title = tagList(shiny::icon("chart-bar"), "Distribution of Pareto levels"),
          plotOutput("ParetoHist")),
        tabPanel(
          title = tagList(shiny::icon("question"), "Plot explained"),
          includeMarkdown('docs/pareto_hist_explained_rjh.md')
        )),
      tabBox(
        #title = 'Optimal hits, tab view',
        title = 'optimal genes数据详细信息展示',
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
            options = list(size = 10)),
          DT::dataTableOutput("TopTable"),
          downloadButton('TopDownload',"Download Top results")),
        tabPanel(
          title = tagList(shiny::icon("question"), "Variables explained"),
          includeMarkdown('docs/variables_explained_rjh.md')
        )
      ),
      tabBox(
        
        #title = 'Optimal hits, relative view',
        title = '部分选择出的基因bar展示',
        id = 'tabset2',
        width = 12,
        tabPanel(
          title = tagList(shiny::icon("list-alt"), "Optimal hits"),
          plotOutput("ParetoBar", height = '1300px')),
        tabPanel(
          title = tagList(shiny::icon("question"), "Plot explained"),
          includeMarkdown('docs/pareto_bar_explained_rjh.md')
        )
      ),
      box(
        #title = "Full dataset",
        status = "primary",
        title = "Full dataset所以数据",
        solidHeader = TRUE,
        collapsible = TRUE,
        DT::dataTableOutput("table"),
        width = 12
      )#,
      # tabBox(
      #   id = 'tabset0',
      #   width = 12,
      #   tabPanel(
      #     title = tagList(shiny::icon("chart-bar"), "Distribution of Pareto levels"),
      #     plotOutput("ParetoHist")),
      #   tabPanel(
      #     title = tagList(shiny::icon("question"), "Plot explained"),
      #     includeMarkdown('docs/pareto_hist_explained.md')
      #   ))
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
        # area(col = c(RNAseq_LFC)) ~ sign_formatter,
        # area(col = c(RNAseq_adjpval)) ~ sign_formatter,
        # area(col = c(depmap)) ~ depmap_formatter,
        # area(col = c(DNB)) ~ sign_formatter
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
  
  output$ParetoBar <- renderPlot({
    
    res <- genericdata()
    rescale2 <- function(x){scales::rescale(x, to = c(-1,1))}
    
    # faceted bar plot:
    top_genes <- res %>%
      dplyr::filter(.level == 1) %>%
      arrange(desc(full_screen_M1A)) %>%   ##full_screen_M1A)从大到小排序
      head(n = 30) %>%
      # mutate_at(c(4:(ncol(res))), rescale2) %>%
      # dplyr::select(-c(contains('pval'),
      #                  ensembl_gene_id, .level, annotation))
    
      mutate_at(c(4:(ncol(res))), rescale2) %>%
      dplyr::select(-c(ensembl_gene_id, .level, annotation,DNB))
    
    # gather everything before ploting
    top_genes_tall <- gather(top_genes,
                             key = 'objective',value = 'value',
                             -c(gene))
    
    # lock levels
    top_genes_tall$gene <- factor(top_genes_tall$gene,
                                  levels = top_genes$gene)
    
    top_genes_tall$objective <- factor(top_genes_tall$objective,
                                       levels = rev(c(names(DATA[4:14]))))
    
    p <- ggplot(top_genes_tall,
                aes(x = objective,
                    y = value,
                    fill = value > 0)) +
      geom_bar(stat = 'identity') +
      facet_wrap(~ gene, nrow = 5, scales = 'free_y') +
      coord_flip() +
      theme(legend.position = 'none') +
      xlab('genes') +
      scale_fill_manual(values = c("#8da0cb", "#fc8d62")) +
      theme(legend.position = 'none',
            plot.title = element_text(hjust = 0.5, size = 18),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
    p
    
  })
  
  output$ParetoHist <- renderPlot({
    
    res <- genericdata()
    
    res %>%
      mutate(flag = ifelse(.level == 1, 'T', 'F')) %>%
      dplyr::filter(.level <= 30) %>%
      ggplot(aes(as.factor(.level), fill = flag)) +
      geom_bar(colour = 'black') +
      xlab('Pareto level') +
      ylab('Number of genes') +
      theme(legend.position = 'none',
            plot.title = element_text(hjust = 0.5, size = 18),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16)) +
      scale_fill_manual(values = c("#d9d9d9", "#66c2a5"))
  })
  
  output$table <-  DT::renderDataTable({
    # topdata, not filtered by levels
    res <- topdata()
    DT::datatable(res, options = list( scrollX = TRUE))
  })
  
  # slider resetting 
  initialInputs <- isolate(reactiveValuesToList(input))
  observe({
    # save initial values of dynamic inputs
    inputValues <- reactiveValuesToList(input)
    # modify a possibly nested list recursively
    initialInputs <<- utils::modifyList(inputValues, initialInputs)
  })
  
  observeEvent(input$reset, {
    update_helper <- function(my_input){
      updateSliderTextInput(
        session = session,
        inputId = my_input,
        selected = initialInputs[[my_input]]
      )
    }
    
    lapply(paste0('obj_', c(1:TOTAL_OBJECTIVES)), update_helper)
    
    # reset essentiality filter
    flt_value <- initialInputs[['flt_1']]
    session$sendInputMessage('flt_1', list(value = flt_value))
  })
}

shinyApp(ui, server)