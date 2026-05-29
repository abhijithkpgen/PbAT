# multivariate_analysis_module.R
#
# This file contains the UI and Server logic for the updated Multivariate Analysis module.
# PCA plots have been updated to use plotly for interactivity.
# Path analysis plotting is now handled by the tidySEM package.

# Required Libraries
library(shiny)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(lavaan)
library(tidySEM)
library(DT)
library(plotly)
library(htmlwidgets)

# ===================================================================
# MODULE UI FUNCTION
# ===================================================================
multivariate_analysis_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Multivariate Analysis",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               uiOutput(ns("multi_sidebar"))
             ),
             mainPanel(
               width = 9,
               uiOutput(ns("multi_mainpanel"))
             )
           )
  )
}

# ===================================================================
# MODULE SERVER FUNCTION
# ===================================================================
multivariate_analysis_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Reactive Values to store results ---
    pca_results  <- reactiveVal(NULL)
    path_results <- reactiveVal(NULL)
    corr_data    <- reactiveVal(NULL)
    
    # --- Reactives to access shared data ---
    multi_file_data <- reactive({ shared_data$file_data })
    multi_subtype_selected <- reactive({ shared_data$multi_subtype })
    
    # --- Dynamic Sidebar UI ---
    output$multi_sidebar <- renderUI({
      req(multi_subtype_selected(), multi_file_data())
      
      df <- multi_file_data()
      choices_numeric <- names(df)[sapply(df, is.numeric)]
      
      step_block <- function(label, ...) {
        tags$div(tags$b(label), br(), ..., style = "margin-bottom: 20px;")
      }
      
      div(style = "color: #142850; font-size: 15px;",
          if (multi_subtype_selected() == "pca") {
            tagList(
              step_block("Step 1: Select Traits for PCA",
                         checkboxGroupInput(ns("multi_pca_traits"), "Traits for PCA", choices = choices_numeric, selected = choices_numeric)
              ),
              step_block("Step 2: Plot Options",
                         selectInput(ns("pca_palette"), "Biplot Color Palette", 
                                     choices = c("Default" = "default", "Viridis" = "viridis", "Plasma" = "plasma", "Grey" = "grey"), 
                                     selected = "default"),
                         checkboxInput(ns("pca_repel"), "Use Text Repel for Labels", value = TRUE)
              ),
              step_block("Step 3: Run and Download",
                         actionButton(ns("multi_run_pca"), "Run PCA", class = "btn btn-success"),
                         uiOutput(ns("multi_pca_status")), br(),
                         downloadButton(ns("multi_download"), "Download Results (ZIP)", class = "btn btn-primary")
              )
            )
          } else if (multi_subtype_selected() == "correlation") {
            tagList(
              step_block("Step 1: Select Traits for Correlation",
                         checkboxGroupInput(ns("multi_corr_traits"), "Traits for Correlation", choices = choices_numeric, selected = choices_numeric)
              ),
              step_block("Step 2: Run and Download",
                         actionButton(ns("multi_run_corr"), "Run Correlation Analysis", class = "btn btn-success"),
                         uiOutput(ns("multi_corr_status")), br(),
                         downloadButton(ns("multi_download"), "Download Results (ZIP)", class = "btn btn-primary")
              )
            )
          } else if (multi_subtype_selected() == "path") {
            tagList(
              step_block("Step 1: Select Dependent and Independent Traits",
                         selectInput(ns("multi_path_dep"), "Dependent Trait (Y)", choices = choices_numeric),
                         selectInput(ns("multi_path_indep"), "Independent Traits (X)",
                                     choices = setdiff(choices_numeric, input$multi_path_dep),
                                     multiple = TRUE)
              ),
              step_block("Step 2: Run and Download",
                         actionButton(ns("multi_run_path"), "Run Path Analysis", class = "btn btn-success"),
                         uiOutput(ns("multi_path_status")), br(),
                         downloadButton(ns("multi_download"), "Download Results (ZIP)", class = "btn btn-primary")
              )
            )
          }
      )
    })
    
    # --- Dynamic Main Panel UI ---
    output$multi_mainpanel <- renderUI({
      req(multi_subtype_selected())
      if (multi_subtype_selected() == "pca") {
        tabsetPanel(
          tabPanel("Individual Biplot", plotlyOutput(ns("multi_pca_biplot"), height = "600px")),
          tabPanel("Scree Plot", plotlyOutput(ns("multi_pca_scree"), height = "600px")),
          tabPanel("Variable Contributions", plotOutput(ns("multi_pca_varcontrib"), height = "600px")),
          tabPanel("Summary Table", DT::DTOutput(ns("multi_pca_eigen")))
        )
      } else if (multi_subtype_selected() == "correlation") {
        tabsetPanel(
          tabPanel("Correlation Matrix Plot", plotOutput(ns("multi_corr_corrplot")))
        )
      } else if (multi_subtype_selected() == "path") {
        tabsetPanel(
          tabPanel("Path Diagram", plotOutput(ns("multi_path_diagram"))),
          tabPanel("Path Coefficients Table", tableOutput(ns("multi_path_coef")))
        )
      }
    })
    
    # --- Observers for Path Analysis Trait Selection ---
    observeEvent(input$multi_path_dep, {
      req(multi_subtype_selected() == "path", multi_file_data())
      df <- multi_file_data()
      choices_numeric <- names(df)[sapply(df, is.numeric)]
      indep_choices <- setdiff(choices_numeric, input$multi_path_dep)
      updateSelectInput(session, "multi_path_indep", choices = indep_choices)
    })
    
    # --- Analysis Logic ---
    
    # PCA
    observeEvent(input$multi_run_pca, {
      req(multi_file_data(), input$multi_pca_traits)
      df <- multi_file_data()
      df_pca_numeric <- df %>%
        dplyr::select(all_of(input$multi_pca_traits)) %>%
        na.omit()
      
      tryCatch({
        res.pca <- FactoMineR::PCA(df_pca_numeric, graph = FALSE)
        pca_results(list(pca = res.pca, data = df_pca_numeric))
        
        output$multi_pca_biplot <- renderPlotly({
          gradient_palettes <- list(
            default = c("#00AFBB", "#E7B800", "#FC4E07"),
            viridis = c("#440154FF", "#21908CFF", "#FDE725FF"),
            plasma  = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
            grey    = c("grey90", "grey10")
          )
          selected_gradient <- gradient_palettes[[input$pca_palette]]
          
          p <- fviz_pca_ind(res.pca, 
                            col.ind = "cos2", 
                            gradient.cols = selected_gradient,
                            repel = input$pca_repel)
          
          ggplotly(p, tooltip = "all")
        })
        
        output$multi_pca_scree <- renderPlotly({
          p <- fviz_screeplot(res.pca, addlabels = TRUE)
          ggplotly(p)
        })
        
        output$multi_pca_varcontrib <- renderPlot({
          fviz_pca_var(res.pca, repel = TRUE)
        })
        
        output$multi_pca_eigen <- DT::renderDT({
          datatable(round(factoextra::get_eigenvalue(res.pca), 2), rownames = TRUE, options = list(dom = 't'))
        })
        output$multi_pca_status <- renderUI({ span(style = "color: green;", icon("check"), " PCA Completed") })
      }, error = function(e) {
        showModal(modalDialog(title = "PCA Error", e$message))
      })
    })
    
    # Correlation
    observeEvent(input$multi_run_corr, {
      req(multi_file_data(), input$multi_corr_traits)
      df <- multi_file_data()
      df_corr <- df %>%
        dplyr::select(all_of(input$multi_corr_traits)) %>%
        na.omit()
      corr_data(df_corr)
      
      tryCatch({
        output$multi_corr_corrplot <- renderPlot({
          corrplot(cor(df_corr), method = "number", type = "upper", order = "hclust")
        })
        output$multi_corr_status <- renderUI({ span(style = "color: green;", icon("check"), " Correlation Completed") })
      }, error = function(e) {
        showModal(modalDialog(title = "Correlation Error", e$message))
      })
    })
    
    # Path Analysis
    observeEvent(input$multi_run_path, {
      req(multi_file_data(), input$multi_path_dep, input$multi_path_indep)
      df <- multi_file_data()
      dep <- input$multi_path_dep
      indep <- input$multi_path_indep
      
      if (length(indep) < 2) {
        showModal(modalDialog(title = "Selection Error", "Please select at least two Independent Traits."))
        return()
      }
      
      df_path <- df[, c(dep, indep), drop = FALSE] %>% na.omit()
      model_str <- paste0(dep, " ~ ", paste(indep, collapse = " + "))
      
      tryCatch({
        fit <- lavaan::sem(model_str, data = df_path, meanstructure = TRUE)
        path_results(fit)
        
        # New code using tidySEM
        output$multi_path_diagram <- renderPlot({
          tidySEM::graph_sem(fit)
        })
        output$multi_path_coef <- renderTable({
          subset(lavaan::parameterEstimates(fit, standardized = TRUE), op == "~")
        }, rownames = FALSE)
        
        output$multi_path_status <- renderUI({ span(style = "color: green;", icon("check"), " Path Analysis Completed") })
      }, error = function(e) {
        showModal(modalDialog(title = "Path Analysis Error", e$message))
      })
    })
    
    # --- Download Handler ---
    output$multi_download <- downloadHandler(
      filename = function() {
        paste0("Multivariate_Results_", multi_subtype_selected(), "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        # 1. Create a clean, isolated temporary subdirectory
        tmp_dir <- tempfile("multi_zip_")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        files <- c()
        
        tryCatch({
          if (multi_subtype_selected() == "pca" && !is.null(pca_results()$pca)) {
            res.pca <- pca_results()$pca
            
            gradient_palettes <- list(
              default = c("#00AFBB", "#E7B800", "#FC4E07"),
              viridis = c("#440154FF", "#21908CFF", "#FDE725FF"),
              plasma  = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
              grey    = c("grey90", "grey10")
            )
            selected_gradient <- gradient_palettes[[input$pca_palette]]
            
            p_biplot <- fviz_pca_ind(res.pca, col.ind = "cos2", gradient.cols = selected_gradient, repel = input$pca_repel)
            p_scree <- fviz_screeplot(res.pca, addlabels = TRUE)
            p_var <- fviz_pca_var(res.pca, repel = TRUE)
            
            # Save static PDFs to completely avoid the Pandoc/saveWidget crash issue
            fname_biplot <- file.path(tmp_dir, "PCA_Biplot.pdf")
            pdf(fname_biplot, width = 8, height = 7); print(p_biplot); dev.off()
            
            fname_scree <- file.path(tmp_dir, "PCA_Scree.pdf")
            pdf(fname_scree, width = 8, height = 6); print(p_scree); dev.off()
            
            fname_var <- file.path(tmp_dir, "PCA_Contributions.pdf")
            pdf(fname_var, width = 8, height = 7); print(p_var); dev.off()
            
            write.csv(factoextra::get_eigenvalue(res.pca), file.path(tmp_dir, "PCA_Eigenvalues.csv"))
            write.csv(pca_results()$data, file.path(tmp_dir, "PCA_Input_Data.csv"))
            
            files <- c(files, fname_biplot, fname_scree, fname_var, 
                       file.path(tmp_dir, "PCA_Eigenvalues.csv"), file.path(tmp_dir, "PCA_Input_Data.csv"))
          }
          
          if (multi_subtype_selected() == "correlation" && !is.null(corr_data())) {
            fname_corrplot <- file.path(tmp_dir, "Correlation_MatrixPlot.pdf")
            pdf(fname_corrplot, width = 8, height = 8)
            corrplot(cor(corr_data()), method = "number", type = "upper", order = "hclust")
            dev.off()
            
            write.csv(cor(corr_data()), file.path(tmp_dir, "Correlation_Matrix.csv"))
            files <- c(files, fname_corrplot, file.path(tmp_dir, "Correlation_Matrix.csv"))
          }
          
          if (multi_subtype_selected() == "path" && !is.null(path_results())) {
            fit <- path_results()
            path_plot <- tidySEM::graph_sem(fit)
            ggsave(file.path(tmp_dir, "Path_Diagram.pdf"), plot = path_plot, device = "pdf", width = 11, height = 8.5)
            write.csv(lavaan::parameterEstimates(fit, standardized = TRUE), file.path(tmp_dir, "Path_Coefficients.csv"))
            files <- c(files, file.path(tmp_dir, "Path_Diagram.pdf"), file.path(tmp_dir, "Path_Coefficients.csv"))
          }
          
        }, error = function(e) {
          showNotification(paste("Error building download zip:", e$message), type = "error")
        })
        
        # 2. Compress the isolated files safely using cherry-pick
        zip::zip(zipfile = file, files = files, mode = "cherry-pick")
      }
    )
    
  })
}

