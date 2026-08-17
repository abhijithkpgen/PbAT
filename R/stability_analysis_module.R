# stability_analysis_module.R
#
# PbAT: Stability Analysis module (AMMI + GGE)
# FINAL VERSION: This version uses a direct and robust method for GGE plotting
# to ensure environment vectors are always drawn correctly. Helper functions
# have been removed in favor of a simpler, more reliable implementation.

library(shiny)
library(dplyr)
library(metan)
library(DT)
library(colourpicker)
library(ggplot2)
library(grid)

# ===================================================================
# UI
# ===================================================================
stability_analysis_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Stability Analysis",
    div(
      class = "alert alert-info shadow-sm", 
      style = "margin: 0px 0px 20px 0px; border-left: 5px solid #1F4E79; background-color: #f8f9fa; color: #333; padding: 12px 20px; border-radius: 8px; font-size: 14px;",
      icon("lightbulb", style="color:#f39c12; font-size: 16px; margin-right: 8px;"), 
      tags$b("Feeling stuck?"), 
      " Check out ", 
      tags$a("this video tutorial.", href = "https://youtu.be/-VA_RWqC1eE", target = "_blank", style="color: #1F4E79; font-weight: bold; text-decoration: underline;")
    ),
    sidebarLayout(
      sidebarPanel(width = 3, uiOutput(ns("stab_sidebar"))),
      mainPanel(width = 9, uiOutput(ns("stab_mainpanel")))
    )
  )
}

# ===================================================================
# SERVER
# ===================================================================
stability_analysis_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ammi_results <- reactiveVal(NULL)
    gge_results  <- reactiveVal(NULL)
    
    stab_file_data <- reactive({ shared_data$file_data })
    stab_subtype_selected <- reactive({ shared_data$stab_subtype })
    
    # ---------------- Sidebar ----------------
    output$stab_sidebar <- renderUI({
      req(stab_subtype_selected(), stab_file_data())
      df <- stab_file_data()
      choices_numeric <- names(df)[sapply(df, is.numeric)]
      choices_all <- names(df)
      
      step_block <- function(label, ...) {
        tags$div(tags$b(label), br(), ..., style = "margin-bottom: 20px;")
      }
      
      div(
        style = "color:#142850;font-size:15px;",
        if (stab_subtype_selected() == "ammi") {
          tagList(
            step_block("Step 1: Select Columns for AMMI",
                       selectInput(ns("stab_ammi_genotype"), "Genotype Column", choices = choices_all),
                       selectInput(ns("stab_ammi_location"), "Location Column", choices = choices_all),
                       selectInput(ns("stab_ammi_rep"), "Replication Column", choices = choices_all)
            ),
            step_block("Step 2: Select Trait",
                       selectInput(ns("stab_ammi_trait"), "Trait", choices = choices_numeric)
            ),
            step_block("Step 3: Plot Options",
                       colourInput(ns("stab_ammi_col_gen"), "Genotype Color", value = "blue"),
                       colourInput(ns("stab_ammi_col_env"), "Environment Color", value = "red"),
                       numericInput(ns("stab_ammi_lab_size"), "Label Size:", value = 4, min = 1, max = 10, step = 0.5),
                       checkboxInput(ns("stab_ammi_repel"), "Repel Labels", value = TRUE)
            ),
            step_block("Step 4: Run and Download",
                       actionButton(ns("stab_run_ammi"), "Run AMMI Analysis", class = "btn btn-success"),
                       uiOutput(ns("stab_ammi_status")), br(),
                       downloadButton(ns("stab_download"), "Download Results (ZIP)", class = "btn btn-primary")
            )
          )
        } else {
          tagList(
            step_block("Step 1: Map Data Columns",
                       selectInput(ns("stab_gge_genotype"), "Genotype Column", choices = choices_all),
                       selectInput(ns("stab_gge_location"), "Location Column", choices = choices_all),
                       checkboxInput(ns("stab_gge_is_replicated"), "Is the data replicated?", value = FALSE),
                       conditionalPanel(
                         condition = paste0("input['", ns("stab_gge_is_replicated"), "']"),
                         selectInput(ns("stab_gge_rep"), "Replication Column", choices = choices_all)
                       )
            ),
            step_block("Step 2: Select Trait",
                       selectInput(ns("stab_gge_trait"), "Trait", choices = choices_numeric)
            ),
            step_block("Step 3: GGE Model Options",
                       selectInput(ns("stab_gge_centering"), "Centering Method",
                                   choices = c("none", "environment", "global"), selected = "environment"),
                       selectInput(ns("stab_gge_scaling"), "Scaling Method",
                                   choices = c("none", "sd"), selected = "sd"),
                       selectInput(ns("stab_gge_svp"), "SVP Method",
                                   choices = c("genotype", "environment", "symmetrical"), selected = "symmetrical")
            ),
            step_block("Step 4: Plot Options",
                       colourInput(ns("stab_gge_col_gen"), "Genotype Color", value = "blue"),
                       colourInput(ns("stab_gge_col_env"), "Environment Color", value = "red"),
                       numericInput(ns("stab_gge_gen_lab_size"), "Genotype Label Size:", value = 3, min = 1, max = 10, step = 0.5),
                       numericInput(ns("stab_gge_env_lab_size"), "Environment Label Size:", value = 4, min = 1, max = 10, step = 0.5),
                       checkboxInput(ns("stab_gge_repel"), "Repel Labels", value = TRUE)
            ),
            step_block("Step 5: Run and Download",
                       actionButton(ns("stab_run_gge"), "Run GGE Biplot", class = "btn btn-success"),
                       uiOutput(ns("stab_gge_status")), br(),
                       downloadButton(ns("stab_download"), "Download Results (ZIP)", class = "btn btn-primary")
            )
          )
        }
      )
    })
    
    # ---------------- Mainpanel ----------------
    output$stab_mainpanel <- renderUI({
      req(stab_subtype_selected())
      if (stab_subtype_selected() == "ammi") {
        tabsetPanel(
          tabPanel("AMMI Biplot", plotOutput(ns("stab_ammi_biplot"), height = "600px")),
          tabPanel("ANOVA", h4("AMMI ANOVA Table"), tableOutput(ns("stab_ammi_anova"))),
          tabPanel("Stability Indices", h4("AMMI Stability Value (ASV) and other Indices"), tableOutput(ns("stab_ammi_indices"))),
          tabPanel("Yield & Rank", h4("Genotype Mean Yield and Ranks"), tableOutput(ns("stab_ammi_yield_rank"))),
          tabPanel("Yield Stability Plot (WAASB)", plotOutput(ns("stab_ammi_waasb")))
        )
      } else {
        tabsetPanel(
          tabPanel("Which Won Where", plotOutput(ns("stab_gge_plot_type1"), height = "600px")),
          tabPanel("Mean vs Stability", plotOutput(ns("stab_gge_plot_type2"), height = "600px")),
          tabPanel("Representativeness vs Discriminativeness", plotOutput(ns("stab_gge_plot_type3"), height = "600px"))
        )
      }
    })
    
    # ---------------- AMMI ----------------
    observeEvent(input$stab_run_ammi, {
      req(stab_file_data(), input$stab_ammi_genotype, input$stab_ammi_location,
          input$stab_ammi_rep, input$stab_ammi_trait)
      
      df <- stab_file_data()
      df_ammi <- df %>%
        dplyr::select(
          gen  = all_of(input$stab_ammi_genotype),
          env  = all_of(input$stab_ammi_location),
          rep  = all_of(input$stab_ammi_rep),
          resp = all_of(input$stab_ammi_trait)
        ) %>% na.omit()
      
      tryCatch({
        ammi_fit  <- metan::performs_ammi(df_ammi, env = env, gen = gen, rep = rep, resp = resp, verbose = FALSE)
        waasb_fit <- metan::waasb(df_ammi, gen, env, rep, resp, verbose = FALSE)
        ammi_results(list(ammi = ammi_fit, waasb = waasb_fit))
        
        output$stab_ammi_biplot <- renderPlot({
          metan::plot_scores(ammi_fit,
                             type = 2,
                             col.gen = input$stab_ammi_col_gen,
                             col.env = input$stab_ammi_col_env,
                             size.tex.gen = input$stab_ammi_lab_size,
                             size.tex.env = input$stab_ammi_lab_size,
                             repel = input$stab_ammi_repel
          ) + theme_minimal(base_size = 16)
        })
        
        output$stab_ammi_waasb <- renderPlot({ plot(waasb_fit) })
        
        output$stab_ammi_anova <- renderTable({
          req(ammi_results())
          ammi_results()$ammi[[1]]$ANOVA %>%
            as.data.frame() %>%
            dplyr::select(-any_of(c("Proportion", "Accumulated"))) %>%
            mutate(across(where(is.numeric), ~ round(., 2)))
        }, rownames = TRUE)
        
        output$stab_ammi_indices <- renderTable({
          req(ammi_results())
          indices_to_select <- c("GEN", "ASV", "SIPC", "EV", "Za", "ASTAB", "ASI", "WAASY", "WAASB")
          metan::ammi_indexes(ammi_results()$ammi)[[1]] %>%
            dplyr::select(any_of(indices_to_select)) %>%
            mutate(across(where(is.numeric), ~ round(., 2)))
        })
        
        output$stab_ammi_yield_rank <- renderTable({
          req(ammi_results())
          ranks_to_select <- c("GEN", "Y", "Y_R", "ASV_R", "SIPC_R", "EV_R", "Za_R", "ASTAB_R")
          metan::ammi_indexes(ammi_results()$ammi)[[1]] %>%
            dplyr::select(any_of(ranks_to_select)) %>%
            mutate(across(where(is.numeric), ~ round(., 2)))
        })
        
        output$stab_ammi_status <- renderUI({ span(style = "color: green;", icon("check"), " AMMI Completed") })
      }, error = function(e) {
        showModal(modalDialog(title = "AMMI Error", e$message))
      })
    })
    
    # ---------------- GGE ----------------
    observeEvent(input$stab_run_gge, {
      req(stab_file_data(), input$stab_gge_genotype, input$stab_gge_location, input$stab_gge_trait)
      df <- stab_file_data()
      
      df_gge <- df %>%
        dplyr::select(
          gen  = all_of(input$stab_gge_genotype),
          env  = all_of(input$stab_gge_location),
          resp = all_of(input$stab_gge_trait),
          if (input$stab_gge_is_replicated) all_of(input$stab_gge_rep) else NULL
        ) %>% na.omit()
      
      if (input$stab_gge_is_replicated) {
        df_gge <- df_gge %>%
          dplyr::group_by(gen, env) %>%
          dplyr::summarise(resp = mean(resp, na.rm = TRUE), .groups = "drop")
      }
      
      tryCatch({
        gge_model <- metan::gge(
          df_gge,
          env = env, gen = gen, resp = resp,
          centering = input$stab_gge_centering,
          scaling   = input$stab_gge_scaling,
          svp       = input$stab_gge_svp
        )
        gge_results(gge_model)
        
        # DEFINITIVE FIX: This function builds the base plot and adds vectors robustly.
        create_gge_biplot <- function(model, type) {
          # 1. Create the base plot using the correct arguments
          base_plot <- plot(model,
                            type = type,
                            col.gen = input$stab_gge_col_gen,
                            col.env = input$stab_gge_col_env,
                            size.tex.gen = input$stab_gge_gen_lab_size,
                            size.tex.env = input$stab_gge_env_lab_size,
                            repel = input$stab_gge_repel)
          
          # 2. For types 2 and 3, manually add the environment vectors
          if (type %in% c(2, 3)) {
            env_coords <- as.data.frame(model$coordenv)
            
            # Ensure coordinates exist and have the right columns before plotting
            if (!is.null(env_coords) && all(c("PC1", "PC2") %in% names(env_coords))) {
              base_plot <- base_plot +
                geom_segment(
                  data = env_coords,
                  aes(x = 0, y = 0, xend = .data$PC1, yend = .data$PC2),
                  inherit.aes = FALSE,
                  color = "darkgreen", # Changed color for visibility
                  arrow = arrow(length = unit(2.5, "mm"))
                )
            }
          }
          return(base_plot)
        }
        
        # Render the plots using the new robust function
        output$stab_gge_plot_type1 <- renderPlot({ create_gge_biplot(gge_model, 3) }) # Which won where
        output$stab_gge_plot_type2 <- renderPlot({ create_gge_biplot(gge_model, 2) }) # Mean vs Stability
        output$stab_gge_plot_type3 <- renderPlot({ create_gge_biplot(gge_model, 4) }) # Discriminativeness
        
        output$stab_gge_status <- renderUI({ span(style = "color: green;", icon("check"), " GGE Completed") })
      }, error = function(e) {
        showModal(modalDialog(title = "GGE Error", e$message))
      })
    })
    
    # ---------------- Download ----------------
    output$stab_download <- downloadHandler(
      filename = function() paste0("Stability_Results_", stab_subtype_selected(), "_", Sys.Date(), ".zip"),
      content = function(file) {
        tmp_dir <- tempdir()
        files <- c()
        
        if (stab_subtype_selected() == "ammi" && !is.null(ammi_results())) {
          res <- ammi_results()
          capture.output(res$ammi[[1]]$ANOVA, file = file.path(tmp_dir, "AMMI_ANOVA.txt"))
          write.csv(metan::ammi_indexes(res$ammi)[[1]],
                    file.path(tmp_dir, "AMMI_Stability_Ranks_Full.csv"), row.names = FALSE)
          
          pdf(file.path(tmp_dir, "AMMI_Biplot.pdf"))
          print(
            plot_scores(res$ammi, type = 2,
                        col.gen = input$stab_ammi_col_gen,
                        col.env = input$stab_ammi_col_env,
                        size.tex.gen = input$stab_ammi_lab_size,
                        size.tex.env = input$stab_ammi_lab_size,
                        repel = input$stab_ammi_repel) +
              theme_minimal(base_size = 16)
          )
          dev.off()
          
          pdf(file.path(tmp_dir, "AMMI_WAASB_Plot.pdf")); print(plot(res$waasb)); dev.off()
          
          files <- c(files,
                     file.path(tmp_dir, "AMMI_ANOVA.txt"),
                     file.path(tmp_dir, "AMMI_Stability_Ranks_Full.csv"),
                     file.path(tmp_dir, "AMMI_Biplot.pdf"),
                     file.path(tmp_dir, "AMMI_WAASB_Plot.pdf"))
        }
        
        if (stab_subtype_selected() == "gge" && !is.null(gge_results())) {
          gge_model <- gge_results()
          
          create_gge_biplot_for_dl <- function(model, type) {
            base_plot <- plot(model, type = type,
                              col.gen = input$stab_gge_col_gen, col.env = input$stab_gge_col_env,
                              size.tex.gen = input$stab_gge_gen_lab_size, size.tex.env = input$stab_gge_env_lab_size,
                              repel = input$stab_gge_repel)
            if (type %in% c(2, 3)) {
              env_coords <- as.data.frame(model$coordenv)
              if (!is.null(env_coords) && all(c("PC1", "PC2") %in% names(env_coords))) {
                base_plot <- base_plot +
                  geom_segment(data = env_coords, aes(x = 0, y = 0, xend = .data$PC1, yend = .data$PC2),
                               inherit.aes = FALSE, color = "darkgreen",
                               arrow = arrow(length = unit(2.5, "mm")))
              }
            }
            return(base_plot)
          }
          
          for (i in c(2, 3, 4)) {
            p_file <- file.path(tmp_dir, paste0("GGE_Plot_Type", i, ".pdf"))
            pdf(p_file, width = 7, height = 6)
            print(create_gge_biplot_for_dl(gge_model, i))
            dev.off()
            files <- c(files, p_file)
          }
        }
        
        zip::zipr(zipfile = file, files = files, recurse = FALSE)
      }
    )
  })
}
