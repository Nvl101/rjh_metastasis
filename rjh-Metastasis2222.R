library(shiny)
library(shinydashboard)
library(tidyverse)
library(magrittr)
library(tidyr)
library(scales)
library(shinyWidgets)
library(formattable)
library(DT)
# library(d3heatmap)
library(R.utils)
library(RColorBrewer)
source("R/server_utils.R", local = TRUE)
source("R/ui_utils.R", local = TRUE)

# constants & input files
USUAL_OPTIONS <- c('low', 'exclude', 'high')
DISTANCE_OPTIONS <- c('close', 'exclude', 'far') 
TOTAL_OBJECTIVES <- 10 
DATA <- read_csv('data/data_res_protein.csv')#, quoted_na = FALSE)
#HEATMAP_DATA <- read_tsv('data/heatmap_data.tsv')
# NLP <- read_csv('data/nlp_allowed.csv')
# NLP <- read_csv('data/data_res_protein.csv'), quoted_na = FALSE)
# configuration for data features and their sections

non_features <- c("id","name","type","exp_name")
rna_features <- c("m1a_hit","m5c_hit","m6a_hit","m7g_hit","full_hit","m1a_Regulation","m5c_Regulation","m6a_Regulation","m7g_Regulation","regulation","logFC","PValue","FDR","degree.x","closeness_centrality.x","betweenness_centrality.x","eigenvector_centrality.x","page_rank_score.x","clustering_coefficient.x","DNB","m1a_Foldchange","m5c_Foldchange","m6a_Foldchange","m7g_Foldchange")
dna_features <- c("CCF_diff","CCF_abs_diff","CCF_p_ttest","CCF_p_wilcoxon","divergene")
protein_features <- c("protein_degree","protein_closeness_centrality","protein_betweenness_centrality","protein_eigenvector_centrality","protein_page_rank_score","protein_clustering_coefficient","protein_m1.mean","protein_m0.mean","protein_Diff","protein_abs_Diff","protein_pvalue")
literature_features <- c("lit_meta","lit_meta_colon")
###
TOTAL_OBJECTIVES <- length(rna_features) + length(dna_features) + length(protein_features) + length(literature_features)

ui <- dashboardPage(
  
  skin = 'blue',
  dashboardHeader(title = "RJH-Metastasis"),
  dashboardSidebar(
    sidebarMenu(
      id = 'mysidebar',
      menuItem(
        "RNA features",
        tabName = "widgets",
        icon = icon("dna"),
        lapply(seq_along(rna_features), function(i) {
          sliderTextInput01(
            paste0("obj_", i),  # Generate inputId dynamically
            rna_features[i],    # Use feature name as label and default value
            ifelse(i == 1, "high", "exclude")              # Default value
          )
        })
        
      ),
      menuItem(
        "DNA features",
        tabName = "widgets",
        icon = icon("dna"),
        lapply(seq_along(dna_features), function(i) {
          sliderTextInput01(
            paste0("obj_", i + length(rna_features)),  # Generate inputId dynamically
            dna_features[i],    # Use feature name as label and default value
            "exclude"              # Default value
          )
        })
      ),
      menuItem(
        "Protein features",
        tabName = "widgets",
        icon = icon("dna"),
        lapply(seq_along(protein_features), function(i) {
          sliderTextInput01(
            paste0("obj_", i + length(rna_features) + length(dna_features)),  # Generate inputId dynamically
            protein_features[i],    # Use feature name as label and default value
            "exclude"              # Default value
          )
        })
      ),
      menuItem(
        "Literature support",
        tabName = "widgets",
        icon = icon("graduation-cap"),
        lapply(seq_along(literature_features), function(i) {
          sliderTextInput01(
            paste0("obj_", i + length(rna_features) + length(dna_features) + length(protein_features)),  # Generate inputId dynamically
            literature_features[i],    # Use feature name as label and default value
            "exclude"              # Default value
          )
        })
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
        title = 'Optimal genes (Pareto level 1) result',
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
        title = 'Optimal genes details',
        id = 'tabset1',
        width = 12,
        tabPanel(
          title = tagList(shiny::icon("list-alt"), "Result"),
          pickerInput(
            inputId = "SortMenu",
            label = "Sort top genes by", 
            choices = list(
              # 'essentiality (depmap)' = c("depmap"),
              #'tractability' = c("trct_ab", "trct_sm", 'trct_om'),
              'RNA features' = rna_features,
              'Protein features' = protein_features,
              'DNA features' = dna_features,
              'Literature support' = literature_features),
            
            #'clinical relevance' = c('clin_ES1', 'clin_ES2', 'clin_ES3'),
            # 'expression' = c('RNAseq_adjpval', 'RNAseq_LFC','DNB')),
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
        title = 'Optimal genes, relative view',
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
        title = "Full dataset",
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
  # STATUS: finished
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
            
      data.prt <- 
        # any_filter(DATA, flt_d = input$flt_1) %>%
        DATA %>%
        mutate_at(
          vars(c('m1a_Regulation', 'm5c_Regulation', 'm6a_Regulation', 'm7g_Regulation', 'regulation', 'divergene', 'DNB')),
          list(~ifelse(. %in% c('yes', 'up'), 1, 0))
        )
      # dplyr::select(-c('id', 'name', 'type','exp_name'))
      
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
      left_join(dplyr::select(DATA, c(id)),
                by = c('id')) # %>%
    # mutate(RNAseq_LFC.x = RNAseq_LFC.y ) %>%
    # dplyr::rename(RNAseq_LFC = RNAseq_LFC.x) %>%
    # dplyr::select(-c(contains('RNAseq_LFC.y')))
  })
  
  # STATUS: IPR
  output$TopTable <- DT::renderDataTable({
    
    # reuse reactive data, same as for TopDownload
    res <- topdata()
    radio <- input$SortMenu
    
    d <- res %>%
      dplyr::filter(.level == 1) %>%
      dplyr::select(-c(.level)) %>%
      arrange(desc(!!sym(radio)))
    
    ###
    # # a list of four features
    # feature_lists <- list(non_features, rna_features, dna_features, protein_features, literature_features)
    # # Define colors for each feature category
    # colors <- c("Non" = "#fc8d62", "RNA" = "#8dd3c7", "DNA" = "#fb9a99", "Protein" = "#fdb863", "Literature" = "#d8daeb")

    # # Iterate through the feature lists and assign colors
    # formatted_features <- lapply(feature_lists, function(feature_list) {
    #   formattable(d %>% dplyr::select(all_of(feature_list)), 
    #     list(feature = color_bar(colors[feature_category])))
    # })
    # formatted_features <- lapply(feature_lists, function(feature_list) {
    # formattable(d %>% dplyr::select(all_of(feature_list)), 
    #     list(feature = color_bar(colors[feature_category])))
    # })
    # ff <- do.call(cbind, formatted_features)
    ### CUTOVER: above iteration is not stable, so I'll use method below
    ff <- formattable(
      d,
      list(
        # Non-features
        id = color_bar('#fdae61'),
        name = color_bar('#fdae61'),
        type = color_bar('#fdae61'),
        exp_name = color_bar('#fdae61'),
        
        # RNA features
        m1a_hit = color_bar('#8dd3c7'),
        m5c_hit = color_bar('#8dd3c7'),
        m6a_hit = color_bar('#8dd3c7'),
        m7g_hit = color_bar('#8dd3c7'),
        full_hit = color_bar('#8dd3c7'),
        
        # DNA features
        CCF_diff = color_bar('#fee0b6'),
        CCF_abs_diff = color_bar('#fee0b6'),
        CCF_p_ttest = color_bar('#fee0b6'),
        CCF_p_wilcoxon = color_bar('#fee0b6'),
        divergene = color_bar('#fee0b6'),
        
        # Protein features
        protein_degree = color_bar('#fb9a99'),
        protein_closeness_centrality = color_bar('#fb9a99'),
        protein_betweenness_centrality = color_bar('#fb9a99'),
        protein_eigenvector_centrality = color_bar('#fb9a99'),
        protein_page_rank_score = color_bar('#fb9a99'),
        protein_clustering_coefficient = color_bar('#fb9a99'),
        
        # Literature features
        lit_meta = color_bar('#9ebcda'),
        lit_meta_colon = color_bar('#9ebcda')
      )
    )
    
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
  
  # STATUS: not done yet
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
      arrange(desc(full_hit)) %>%   ##full_hit)从大到小排序
      head(n = 30) %>%
      # mutate_at(c(4:(ncol(res))), rescale2) %>%
      # dplyr::select(-c(contains('pval'),
      #                  ensembl_gene_id, .level, annotation))
      
      mutate_at(c(5:(ncol(res))), rescale2)# %>%
      # dplyr::select(-c(ensembl_gene_id, .level, annotation,DNB))
    
    # gather everything before ploting
    top_genes_tall <- gather(top_genes,
                             key = 'objective',value = 'value')
                            #  -c(gene))
    
    # lock levels
    # create ordered factor of levels by genes, which we possibly don't need
    # top_genes_tall$gene <- factor(top_genes_tall$gene,
    #                               levels = top_genes$gene)
    
    top_genes_tall$objective <- factor(top_genes_tall$objective,
                                       levels = rev(c(names(DATA[5:length(names(DATA))]))))
    
    p <- ggplot(top_genes_tall,
                aes(x = objective,
                    y = value,
                    fill = value > 0)) +
      geom_bar(stat = 'identity') +
      # wraps same gene to a row of 5, which we possibly don't need
      # facet_wrap(~ gene, nrow = 5, scales = 'free_y') +
      coord_flip() +
      theme(legend.position = 'none') +
      xlab('genes') +
      scale_fill_manual(values = c("#8da0cb", "#fc8d62")) +
      theme(legend.position = 'none',
            plot.title = element_text(hjust = 0.5, size = 18),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
    scale_fill_manual(values = c("#d9d9d9", "#66c2a5"))
    p
    
  })
  
  output$ParetoHist <- renderPlot({
    
    res <- genericdata()
    # HELP: what is the use of .level in this argument
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
  
  # STATUS: in progress
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
    # flt_value <- initialInputs[['flt_1']]
    # session$sendInputMessage('flt_1', list(value = flt_value))
  })
}

shinyApp(ui, server)