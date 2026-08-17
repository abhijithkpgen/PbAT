# R/selection_index_module.R

#' Selection Index Module UI
#' @param id The module namespace id
#' @export
selection_index_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Selection Index",
    fluidPage(
      div(
        class = "alert alert-info shadow-sm", 
        style = "margin: 20px 0px 20px 0px; border-left: 5px solid #1F4E79; background-color: #f8f9fa; color: #333; padding: 12px 20px; border-radius: 8px; font-size: 14px;",
        icon("lightbulb", style="color:#f39c12; font-size: 16px; margin-right: 8px;"), 
        tags$b("Feeling stuck?"), 
        " Check out the video guides in the ", 
        tags$a("Tutorials tab.", onclick = "$('a[data-value=\"Tutorials\"]').click();", style="cursor:pointer; color: #1F4E79; font-weight: bold; text-decoration: underline;")
      ),
    div(class = "container-fluid", style = "margin-top: 20px;",
        div(class = "row",
            # Sidebar
            div(class = "col-md-3",
                div(class = "panel panel-default",
                    div(class = "panel-heading", style = "background-color: #1F4E79; color: white;", 
                        tags$b("1. Index Settings")
                    ),
                    div(class = "panel-body",
                        selectInput(ns("si_method"), "Selection Index Method", 
                                    choices = c("MGIDI (Multi-Trait Genotype-Ideotype Distance)" = "mgidi",
                                                "Smith-Hazel Index" = "sh",
                                                "FAI-BLUP Index" = "fai_blup")),
                        selectInput(ns("si_intensity"), "Selection Intensity (%)", 
                                    choices = c(5, 10, 15, 20, 25, 30), selected = 15)
                    )
                ),
                
                div(class = "panel panel-default",
                    div(class = "panel-heading", tags$b("2. Data Setup")),
                    div(class = "panel-body",
                        selectInput(ns("si_genotype"), "Select Genotype Column", choices = NULL),
                        selectInput(ns("si_rep"), "Select Replication Column (Optional)", choices = NULL),
                        selectizeInput(ns("si_traits"), "Select Traits for Index", choices = NULL, multiple = TRUE)
                    )
                ),
                
                div(class = "panel panel-default",
                    div(class = "panel-heading", tags$b(uiOutput(ns("si_ideotype_title"), inline = TRUE))),
                    div(class = "panel-body",
                        uiOutput(ns("si_ideotype_desc")),
                        uiOutput(ns("si_ideotype_ui"))
                    )
                ),
                
                actionButton(ns("run_si"), "RUN SELECTION INDEX", 
                             class = "btn-primary", style = "width: 100%; font-weight: bold; margin-bottom: 15px;"),
                
                downloadButton(ns("download_si_zip"), "DOWNLOAD SI RESULTS (ZIP)", 
                               class = "btn-success", style = "width: 100%; font-weight: bold;")
            ),
            
            # Main Panel
            div(class = "col-md-9",
                uiOutput(ns("si_results_ui"))
            )
        )
    )
  )
  )
}

#' Selection Index Module Server
#' @param id The module namespace id
#' @param raw_data A reactive expression returning the uploaded dataset
#' @export
selection_index_server <- function(id, raw_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store results
    si_res <- reactiveValues(
      model = NULL,
      index = NULL,
      table = NULL,
      sel_dif = NULL,
      selected = NULL,
      plot_radar = NULL,
      plot_bar = NULL
    )
    
    # 1. Update Dropdowns based on dataset
    observe({
      req(raw_data())
      df <- raw_data()
      cols <- names(df)
      updateSelectInput(session, "si_genotype", choices = c("", cols))
      updateSelectInput(session, "si_rep", choices = c("None", cols))
      # Select numeric columns for traits
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "si_traits", choices = num_cols)
    })
    
    # 2. Dynamic Ideotype UI
    output$si_ideotype_title <- renderUI({
      if (input$si_method == "sh") return("3. Economic Weights")
      return("3. Ideotype Design")
    })
    
    output$si_ideotype_desc <- renderUI({
      if (input$si_method == "sh") {
        p(style = "font-size: 12px; color: gray;", "Specify the economic weight for each trait. Positive values for increase, negative for decrease.")
      } else {
        p(style = "font-size: 12px; color: gray;", "Specify the desired direction of selection for each trait.")
      }
    })
    
    output$si_ideotype_ui <- renderUI({
      req(input$si_traits)
      traits <- input$si_traits
      method <- input$si_method
      
      # Generate a select input or numeric input for each trait based on method
      lapply(traits, function(trait) {
        if (method == "sh") {
            numericInput(ns(paste0("dir_", make.names(trait))), 
                         label = paste(trait, "Weight"), 
                         value = 1)
        } else {
            selectInput(ns(paste0("dir_", make.names(trait))), 
                        label = paste(trait, "Direction"), 
                        choices = c("Increase", "Decrease"),
                        selected = "Increase")
        }
      })
    })
    
    # 3. Process data when button is clicked
    observeEvent(input$run_si, {
      req(input$si_genotype, input$si_traits)
      
      # Gather inputs
      gen <- input$si_genotype
      rep <- if (input$si_rep == "None" || input$si_rep == "") NULL else input$si_rep
      traits <- input$si_traits
      method <- input$si_method
      intensity <- as.numeric(input$si_intensity)
      
      # Extract directions or weights based on method
      ideotype_dirs <- sapply(traits, function(t) {
        input_val <- input[[paste0("dir_", make.names(t))]]
        
        if (method == "sh") {
           if(is.null(input_val)) return(1)
           return(as.numeric(input_val))
        } else {
           if(is.null(input_val)) return("h")
           if(input_val == "Increase") return("h") else return("l")
        }
      })
      
      waiter_show(html = spin_fading_circles(), color = "rgba(255,255,255,0.8)")
      on.exit(waiter_hide())
      
      tryCatch({
        library(metan)
        
        df_model <- as.data.frame(raw_data())
        df_model[[gen]] <- as.factor(df_model[[gen]])
        
        mod <- NULL
        use_means <- FALSE
        
        if (!is.null(rep)) {
           df_model[[rep]] <- as.factor(df_model[[rep]])
           # Fit mixed model using metan::gamem
           capture.output({
               mod <- gamem(df_model, gen = !!sym(gen), rep = !!sym(rep), resp = all_of(traits))
           })
        } else {
           # Compute means across genotypes if no rep provided
           use_means <- TRUE
           df_means <- df_model %>%
             group_by(!!sym(gen)) %>%
             summarise(across(all_of(traits), mean, na.rm = TRUE)) %>%
             as.data.frame()
           
           rownames(df_means) <- df_means[[gen]]
           df_means[[gen]] <- NULL
        }
        
        # Calculate Index
        idx_res <- NULL
        capture.output({
            if (method == "mgidi") {
                if (use_means) {
                    idx_res <- mgidi(df_means, ideotype = ideotype_dirs, SI = intensity)
                } else {
                    idx_res <- mgidi(mod, ideotype = ideotype_dirs, SI = intensity)
                }
            } else if (method == "sh") {
                if (use_means) {
                    showNotification("Smith-Hazel in metan requires replication data to estimate genetic variances. Please provide a replication column.", type = "error")
                    return(NULL)
                }
                # For SH, ideotype_dirs contains the actual numeric weights
                idx_res <- Smith_Hazel(mod, weights = ideotype_dirs, SI = intensity)
            } else if (method == "fai_blup") {
                fai_dirs <- ifelse(ideotype_dirs == "h", "max", "min")
                if (use_means) {
                    idx_res <- fai_blup(df_means, DI = fai_dirs, SI = intensity)
                } else {
                    idx_res <- fai_blup(mod, DI = fai_dirs, SI = intensity)
                }
            }
        })
        
        si_res$index <- idx_res
        
        # Extract tables and plots
        if (method == "mgidi") {
           si_res$table <- idx_res$MGIDI
           si_res$sel_dif <- idx_res$sel_dif
           si_res$plot_radar <- plot(idx_res, type = "index")
           si_res$selected <- data.frame(Selected_Genotypes = as.character(idx_res$MGIDI$Genotype[1:round(nrow(idx_res$MGIDI) * (intensity / 100))]))
        } else if (method == "sh") {
           si_res$table <- if(!is.null(idx_res$Index)) idx_res$Index else idx_res$index
           si_res$sel_dif <- idx_res$sel_dif
           si_res$plot_radar <- NULL
           
           sel_names <- if(!is.null(idx_res$selected)) idx_res$selected else si_res$table$Genotype[1:round(nrow(si_res$table) * (intensity / 100))]
           si_res$selected <- data.frame(Selected_Genotypes = as.character(sel_names))
        } else if (method == "fai_blup") {
           si_res$table <- idx_res$FAI
           si_res$sel_dif <- idx_res$sel_dif
           si_res$plot_radar <- plot(idx_res, type = "index")
           si_res$selected <- data.frame(Selected_Genotypes = as.character(idx_res$FAI$Genotype[1:round(nrow(idx_res$FAI) * (intensity / 100))]))
        }
        
        # Generate Custom Ranked Bar Plot
        if (method %in% c("mgidi", "fai_blup", "sh") && !is.null(si_res$table)) {
           library(ggplot2)
           score_df <- si_res$table
           score_col <- if(method == "mgidi") "MGIDI" else if(method == "fai_blup") "FAI" else "Index"
           
           if (score_col %in% names(score_df)) {
             # Sort lowest score to top (lowest is best in distance indices, but for SH higher is better)
             if (method == "sh") {
                score_df <- score_df[order(score_df[[score_col]], decreasing = TRUE), ]
             } else {
                score_df <- score_df[order(score_df[[score_col]], decreasing = FALSE), ]
             }
             
             # Identify top selected based on intensity
             n_sel <- round(nrow(score_df) * (intensity / 100))
             n_sel <- max(1, n_sel) # ensure at least 1
             score_df$Status <- "Unselected"
             score_df$Status[1:n_sel] <- "Selected"
             
             # Re-sort for ggplot (highest at top of factor means plotted at bottom)
             score_df <- score_df[order(score_df[[score_col]], decreasing = (method != "sh")), ]
             score_df$Genotype <- factor(score_df$Genotype, levels = score_df$Genotype)
             
             # Determine cutoff value
             if (method == "sh") {
                cutoff_val <- min(score_df[[score_col]][score_df$Status == "Selected"], na.rm = TRUE)
             } else {
                cutoff_val <- max(score_df[[score_col]][score_df$Status == "Selected"], na.rm = TRUE)
             }
             
             p_bar <- ggplot(score_df, aes(x = .data[[score_col]], y = Genotype)) +
               geom_segment(aes(x = 0, xend = .data[[score_col]], y = Genotype, yend = Genotype, color = Status), linewidth = 1.5, alpha = 0.8) +
               geom_point(aes(fill = Status), size = 4, shape = 21, color = "white", stroke = 1) +
               geom_vline(xintercept = cutoff_val, linetype = "dashed", color = "#dc3545", linewidth = 1) +
               scale_color_manual(values = c("Selected" = "#28a745", "Unselected" = "#adb5bd")) +
               scale_fill_manual(values = c("Selected" = "#28a745", "Unselected" = "#6c757d")) +
               theme_minimal(base_family = "sans") +
               labs(
                 x = paste(toupper(method), "Score"), 
                 y = "Genotype", 
                 title = paste("Ranked", toupper(method), "Scores"),
                 subtitle = "Genotypes selected based on multi-trait ideotype distance"
               ) +
               theme(
                 plot.title = element_text(face = "bold", size = 16, color = "#1F4E79"),
                 plot.subtitle = element_text(size = 12, color = "#6c757d"),
                 axis.title = element_text(face = "bold", size = 12),
                 axis.text.y = element_text(size = 10, face = "bold"),
                 legend.position = "top",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 12, face = "bold"),
                 panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank()
               )
               
             si_res$plot_bar <- p_bar
           }
        }
        
        showNotification(paste(toupper(method), "Index calculated successfully!"), type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error calculating index:", e$message), type = "error")
      })
    })
    
    # 4. Render UI
    output$si_results_ui <- renderUI({
      if (is.null(si_res$table)) {
        div(class = "alert alert-info", "Configure the settings on the left and click 'Run Selection Index'.")
      } else {
        # Dynamically determine plot tab title and layout
        method <- isolate(input$si_method)
        plot_tab_title <- if(is.null(si_res$plot_radar)) "Ranked Plot" else "Ranked & Circular Plots"
        
        tabsetPanel(id = ns("si_tabs"),
          tabPanel("Selection Tables",
                   br(),
                   h4("1. Selected Genotypes (Winners)"),
                   DT::DTOutput(ns("si_selected_table")),
                   hr(),
                   h4("2. Expected Genetic Gain (Selection Differential)"),
                   p("Shows the expected gain for each trait based on your selection intensity."),
                   DT::DTOutput(ns("si_seldif_table")),
                   hr(),
                   h4("3. Full Ranked Genotypes"),
                   DT::DTOutput(ns("si_table"))
          ),
          tabPanel(plot_tab_title,
                   br(),
                   fluidRow(
                     if(!is.null(si_res$plot_bar) && is.null(si_res$plot_radar)) {
                       # Full width if only Ranked Plot exists (Smith-Hazel)
                       column(12,
                              h4("Ranked Selection Scores"),
                              p("Lollipop plot of index scores sorted from best to worst, with the selection cutoff line."),
                              plotOutput(ns("plot_bar"), height = "700px")
                       )
                     } else if(!is.null(si_res$plot_bar) && !is.null(si_res$plot_radar)) {
                       # Side-by-side if both exist
                       list(
                         column(8,
                                h4("Ranked Selection Scores"),
                                p("Lollipop plot of index scores sorted from best to worst, with the selection cutoff line."),
                                plotOutput(ns("plot_bar"), height = "700px")
                         ),
                         column(4,
                                h4("Index Plot (Circular)"),
                                p("Circular visualization of index scores."),
                                plotOutput(ns("plot_radar"), height = "700px")
                         )
                       )
                     } else {
                       column(12, div(class = "alert alert-warning", "Plots not available for this index."))
                     }
                   )
          )
        )
      }
    })
    
    output$si_selected_table <- DT::renderDT({
      req(si_res$selected)
      DT::datatable(si_res$selected, options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'))
    })
    
    output$si_seldif_table <- DT::renderDT({
      req(si_res$sel_dif)
      df <- si_res$sel_dif
      
      # Handle if sel_dif is a list instead of a dataframe (can happen with FAI-BLUP or Smith-Hazel depending on version)
      if (!is.data.frame(df)) {
         if (is.list(df)) {
            tryCatch({
              df <- dplyr::bind_rows(df, .id = "Group")
            }, error = function(e) {
              df <- as.data.frame(df)
            })
         } else {
            df <- as.data.frame(df)
         }
      }
      
      df <- df %>% mutate(across(where(is.numeric), ~round(., 2)))
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE, dom = 't'))
    })
    
    output$si_table <- DT::renderDT({
      req(si_res$table)
      df <- si_res$table
      df <- df %>% mutate(across(where(is.numeric), ~round(., 2)))
      DT::datatable(df, options = list(pageLength = 20, scrollX = TRUE))
    })
    
    output$plot_contrib <- renderPlot({
      req(si_res$plot_contrib)
      print(si_res$plot_contrib)
    })
    
    output$plot_radar <- renderPlot({
      req(si_res$plot_radar)
      print(si_res$plot_radar)
    })
    
    output$plot_bar <- renderPlot({
      req(si_res$plot_bar)
      print(si_res$plot_bar)
    })
    
    # 5. Download Handler
    output$download_si_zip <- downloadHandler(
      filename = function() paste0("Selection_Index_", Sys.Date(), ".zip"),
      content = function(file) {
        req(si_res$index)
        tmp_dir <- tempfile("si_zip_")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        files_to_zip <- c()
        
        # Save tables
        if (!is.null(si_res$table)) {
          fname <- file.path(tmp_dir, "Selection_Index_Ranked.csv")
          write.csv(si_res$table, fname, row.names = FALSE)
          files_to_zip <- c(files_to_zip, fname)
        }
        
        if (!is.null(si_res$sel_dif)) {
          fname <- file.path(tmp_dir, "Selection_Differential.csv")
          write.csv(si_res$sel_dif, fname, row.names = FALSE)
          files_to_zip <- c(files_to_zip, fname)
        }
        
        if (!is.null(si_res$selected)) {
          fname <- file.path(tmp_dir, "Selected_Genotypes.csv")
          write.csv(si_res$selected, fname, row.names = FALSE)
          files_to_zip <- c(files_to_zip, fname)
        }
        
        # Save plots
        if (!is.null(si_res$plot_radar)) {
          fname <- file.path(tmp_dir, "Index_Plot_Circular.pdf")
          pdf(fname, width = 8, height = 6)
          print(si_res$plot_radar)
          dev.off()
          files_to_zip <- c(files_to_zip, fname)
        }
        
        if (!is.null(si_res$plot_bar)) {
          fname <- file.path(tmp_dir, "Ranked_Scores_Plot.pdf")
          pdf(fname, width = 8, height = 10)
          print(si_res$plot_bar)
          dev.off()
          files_to_zip <- c(files_to_zip, fname)
        }
        
        zip::zipr(file, files_to_zip)
      }
    )
    
  })
}
