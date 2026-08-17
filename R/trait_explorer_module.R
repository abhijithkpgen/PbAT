# R/trait_explorer_module.R

# ===================================================================
# -------- Main Module UI Definition --------
# ===================================================================
#' Trait Explorer Main UI
#'
#' This function creates a tabset panel to house the two sub-modules:
#' Spatial Trait Explorer and Data Curation.
#'
#' @param id A unique identifier for the module.
traitExplorerUI <- function(id) {
  ns <- NS(id)
  # This top-level UI now creates a tabset panel for the two explorers
  tabPanel("Trait Explorer",
           div(
             class = "alert alert-info shadow-sm", 
             style = "margin: 0px 0px 20px 0px; border-left: 5px solid #1F4E79; background-color: #f8f9fa; color: #333; padding: 12px 20px; border-radius: 8px; font-size: 14px;",
             icon("lightbulb", style="color:#f39c12; font-size: 16px; margin-right: 8px;"), 
             tags$b("Feeling stuck?"), 
             " Check out ", 
             tags$a("this video tutorial.", href = "https://youtu.be/jLcn08iG9gw", target = "_blank", style="color: #1F4E79; font-weight: bold; text-decoration: underline;")
           ),
           tabsetPanel(
             id = ns("explorer_tabs"),
             tabPanel("Spatial Trait Explorer", spatialExplorerUI(ns("spatial"))),
             tabPanel("Data Curation & Outlier Analysis", dataCurationUI(ns("curation")))
           )
  )
}

# ===================================================================
# -------- Main Module Server Definition --------
# ===================================================================
#' Trait Explorer Main Server
#'
#' This function calls the server logic for the two sub-modules.
#'
#' @param id A unique identifier for the module.
#' @param home_inputs A reactive list from the home module.
traitExplorerServer <- function(id, home_inputs) {
  moduleServer(id, function(input, output, session) {
    # Call the server logic for each of the sub-modules
    spatialExplorerServer("spatial", home_inputs = home_inputs)
    dataCurationServer("curation", home_inputs = home_inputs)
  })
}


# ===================================================================
# -------- 1. SPATIAL TRAIT EXPLORER (SUB-MODULE) --------
# ===================================================================

#' Spatial Explorer UI (Sub-module)
#' @param id A unique identifier for the sub-module.
spatialExplorerUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 3, uiOutput(ns("controls_ui"))),
    mainPanel(width = 9, uiOutput(ns("plot_area_ui")))
  )
}

#' Spatial Explorer Server (Sub-module)
#' @param id A unique identifier for the sub-module.
#' @param home_inputs A reactive list from the home module.
spatialExplorerServer <- function(id, home_inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    raw_data <- reactiveVal(NULL)
    plot_objects <- reactiveValues()
    
    observeEvent(home_inputs(), {
      req(home_inputs()$analysis_mode == "trait_explorer", home_inputs()$explorer_type == "spatial")
      raw_data(home_inputs()$file_data)
      for(name in names(plot_objects)) { plot_objects[[name]] <- NULL }
    })
    
    output$controls_ui <- renderUI({
      df <- raw_data(); req(df)
      all_cols <- names(df)
      numeric_cols <- all_cols[sapply(df, is.numeric)]
      
      available_traits <- if (!is.null(input$env_col) && !is.null(input$trial_type) && input$trial_type == "Multi Environment") {
        setdiff(numeric_cols, input$env_col)
      } else {
        numeric_cols
      }
      
      # *** THE FIX: Updated logic to preserve user's selection on UI refresh ***
      selected_genotype <- if (!is.null(input$geno_col) && input$geno_col %in% all_cols) {
        input$geno_col
      } else {
        intersect(c("Genotype", "GEN", "Entry"), all_cols)[1]
      }
      
      selected_rep <- if (!is.null(input$rep_col) && input$rep_col %in% all_cols) {
        input$rep_col
      } else {
        intersect(c("Replication", "Rep", "REP"), all_cols)[1]
      }
      
      selected_env <- if (!is.null(input$env_col) && input$env_col %in% all_cols) {
        input$env_col
      } else {
        intersect(c("Location", "LOC", "Environment"), all_cols)[1]
      }
      
      selected_check <- if (!is.null(input$check_col) && input$check_col %in% all_cols) {
        input$check_col
      } else {
        intersect(c("Type", "Entry_Type"), all_cols)[1]
      }
      
      selected_plot <- if (!is.null(input$plot_col) && input$plot_col %in% all_cols) {
        input$plot_col
      } else {
        intersect(c("Plot", "Plot_No", "Plot_number"), all_cols)[1]
      }
      
      selected_block <- if (!is.null(input$block_col_alpha) && input$block_col_alpha %in% all_cols) {
        input$block_col_alpha
      } else if (!is.null(input$block_col_aug) && input$block_col_aug %in% all_cols) {
        input$block_col_aug
      } else {
        intersect(c("Block", "BLOCK"), all_cols)[1]
      }
      # *** END OF FIX ***
      
      tagList(
        h4("Step 1: Define Layout"),
        selectInput(ns("design"), "Select Experimental Design",
                    choices = c("Alpha Lattice" = "alphalattice", "RCBD" = "rcbd", "Augmented RCBD" = "augmentedrcbd"),
                    selected = if(!is.null(input$design)) input$design else "alphalattice"), # <-- ADD THIS LINE
        selectInput(ns("trial_type"), "Select Trial Type",
                    choices = c("Single Environment", "Multi Environment"),
                    selected = if(!is.null(input$trial_type)) input$trial_type else "Single Environment"),
        conditionalPanel(
          condition = paste0("input['", ns("trial_type"), "'] == 'Multi Environment'"),
          selectInput(ns("env_col"), "Environment/Location Column", choices = all_cols, selected = selected_env)
        ),
        hr(),
        h4("Step 2: Map Data Columns"),
        selectInput(ns("plot_col"), "Plot Number Column", choices = all_cols, selected = selected_plot),
        selectInput(ns("geno_col"), "Genotype Column", choices = all_cols, selected = selected_genotype),
        
        conditionalPanel(
          condition = "input.design == 'alphalattice' || input.design == 'rcbd'",
          ns = ns,
          selectInput(ns("rep_col"), "Replication Column", choices = all_cols, selected = selected_rep)
        ),
        conditionalPanel(
          condition = "input.design == 'alphalattice'", ns = ns,
          tagList(
            selectInput(ns("block_col_alpha"), "Block Column", choices = all_cols, selected = selected_block)
          )
        ),
        conditionalPanel(
          condition = "input.design == 'augmentedrcbd'", ns = ns,
          tagList(
            selectInput(ns("block_col_aug"), "Block Column", choices = all_cols, selected = selected_block),
            selectInput(ns("check_col"), "Check/Test Column", choices = all_cols, selected = selected_check)
          )
        ),
        hr(),
        h4("Step 3: Select Trait & Options"),
        selectInput(ns("trait_col"), "Select Trait to Visualize", choices = available_traits),
        uiOutput(ns("highlight_ui")),
        selectInput(ns("palette"), "Color Palette",
                    choices = c("Viridis", "Plasma", "Inferno", "Magma", "RdYlBu", "Blues", "Greens", "Reds"),
                    selected = "Viridis"),
        hr(),
        actionButton(ns("run_plot"), "Generate Map", class = "btn btn-primary", style = "width: 100%;"),
        downloadButton(ns("download_plot"), "Download Plot(s)", class = "btn btn-success", style = "width: 100%; margin-top: 10px;")
      )
    })
    
    output$highlight_ui <- renderUI({
      req(input$geno_col, raw_data())
      df <- raw_data()
      genotype_choices <- c("None", sort(unique(df[[input$geno_col]])))
      selectInput(ns("highlight_geno"), "Highlight Genotype (Optional)", choices = genotype_choices, selected = "None")
    })
    
    output$plot_area_ui <- renderUI({
      plot_list <- reactiveValuesToList(plot_objects)
      if (length(plot_list) == 0) {
        return(tags$div(h4("Click 'Generate Map' to view results.", style = "color: grey; text-align: center; margin-top: 50px;")))
      }
      if (input$trial_type == "Multi Environment") {
        tabs <- lapply(names(plot_list), function(loc_name) {
          plot_output_id <- ns(paste0("spatial_plot_", make.names(loc_name)))
          output[[paste0("spatial_plot_", make.names(loc_name))]] <- plotly::renderPlotly({ plot_list[[loc_name]] })
          tabPanel(title = as.character(loc_name), plotly::plotlyOutput(plot_output_id, height = "700px"))
        })
        do.call(tabsetPanel, tabs)
      } else {
        output$spatial_plot_single <- plotly::renderPlotly({ plot_objects[["single"]] })
        plotly::plotlyOutput(ns("spatial_plot_single"), height = "700px")
      }
    })
    
    observeEvent(input$run_plot, {
      req(raw_data(), input$design, input$trait_col, input$geno_col, input$plot_col)
      for(name in names(plot_objects)) { plot_objects[[name]] <- NULL }
      
      df_orig <- raw_data()
      
      df_processed <- tryCatch({
        df_temp <- df_orig %>% arrange(!!sym(input$plot_col))
        
        if (input$design == "rcbd") {
          req(input$rep_col)
          df_temp$inferred_block <- as.factor(df_temp[[input$rep_col]])
        } else if (input$design == "augmentedrcbd") {
          req(input$block_col_aug)
          df_temp$inferred_block <- as.factor(df_temp[[input$block_col_aug]])
        } else if (input$design == "alphalattice") {
          req(input$block_col_alpha)
          df_temp$inferred_block <- as.factor(df_temp[[input$block_col_alpha]])
        }
        df_temp
      }, error = function(e) { showModal(modalDialog(title = "Layout Error", paste("Could not infer layout:", e$message))); return(NULL) })
      
      req(df_processed)
      
      locations <- if(input$trial_type == "Multi Environment" && !is.null(input$env_col)) unique(as.character(df_processed[[input$env_col]])) else "single"
      
      for (loc in locations) {
        df_location <- if (loc == "single") df_processed else df_processed[df_processed[[input$env_col]] == loc, ]
        
        df_location <- df_location %>% 
          group_by(inferred_block) %>% 
          arrange(!!sym(input$plot_col)) %>% 
          mutate(plot_in_block = row_number()) %>% 
          ungroup()
        
        cols_to_check <- c("plot_in_block", "inferred_block", input$trait_col)
        df_clean <- df_location %>% tidyr::drop_na(all_of(cols_to_check))
        if(nrow(df_clean) == 0) next
        
        df_clean$tooltip <- paste0("Plot: ", df_clean[[input$plot_col]], "<br>Genotype: ", df_clean[[input$geno_col]], "<br>Block: ", df_clean$inferred_block, "<br>", input$trait_col, ": ", round(df_clean[[input$trait_col]], 2))
        if (!is.null(input$rep_col) && input$rep_col %in% names(df_clean)) { df_clean$tooltip <- paste0(df_clean$tooltip, "<br>Replication: ", df_clean[[input$rep_col]]) }
        
        p <- ggplot(df_clean, aes(x = as.factor(plot_in_block), y = as.factor(inferred_block), fill = !!sym(input$trait_col), text = tooltip)) +
          geom_tile(color = "white", size = 0.5, width = 0.9, height = 0.9) +
          scale_fill_viridis_c(option = tolower(input$palette)) +
          labs(fill = input$trait_col, title = paste("Spatial Distribution of", input$trait_col, if(loc != "single") paste("in", loc) else ""), x = "Plot within Block", y = "Block") +
          theme_minimal() + theme(axis.title = element_text(size=12), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank())
        
        geom_layers <- list()
        color_values <- c()
        linetype_values <- c()
        
        if (input$design == "augmentedrcbd" && !is.null(input$check_col) && input$check_col %in% names(df_clean)) {
          check_data <- df_clean[df_clean[[input$check_col]] != "test", , drop = FALSE]
          if(nrow(check_data) > 0) {
            geom_layers <- c(geom_layers, list(
              geom_tile(data = check_data, aes(x = as.factor(plot_in_block), y = as.factor(inferred_block), linetype = "Check Plots"), fill = NA, color = "black", size = 1.2, width = 0.9, height = 0.9, inherit.aes = FALSE)
            ))
            linetype_values["Check Plots"] <- "dashed"
          }
        }
        
        if (!is.null(input$highlight_geno) && input$highlight_geno != "None") {
          highlight_data <- df_clean[df_clean[[input$geno_col]] == input$highlight_geno, , drop = FALSE]
          if(nrow(highlight_data) > 0) {
            geom_layers <- c(geom_layers, list(
              geom_tile(data = highlight_data, aes(x = as.factor(plot_in_block), y = as.factor(inferred_block), color = "Highlighted Genotype"), fill = NA, size = 1.2, width = 0.9, height = 0.9, inherit.aes = FALSE)
            ))
            color_values["Highlighted Genotype"] <- "red"
          }
        }
        
        if(length(geom_layers) > 0) p <- p + geom_layers
        
        if(length(color_values) > 0) {
          p <- p + scale_color_manual(name = "", values = color_values)
        }
        if(length(linetype_values) > 0) {
          p <- p + scale_linetype_manual(name = "", values = linetype_values)
        }
        
        plot_objects[[loc]] <- plotly::ggplotly(p, tooltip = "text")
      }
    })
    
    output$download_plot <- downloadHandler(
      filename = function() {
        req(input$trait_col)
        if (input$trial_type == "Multi Environment") paste0("Trait_Explorer_Plots_", input$trait_col, "_", Sys.Date(), ".zip") else paste0("Trait_Explorer_", input$trait_col, "_", Sys.Date(), ".html")
      },
      content = function(file) {
        plot_list <- reactiveValuesToList(plot_objects); req(length(plot_list) > 0)
        if (input$trial_type == "Multi Environment") {
          temp_dir <- tempfile(); dir.create(temp_dir); file_paths <- c()
          for (loc_name in names(plot_list)) {
            if (!is.null(plot_list[[loc_name]])) {
              path <- file.path(temp_dir, paste0("Plot_", loc_name, ".html"))
              htmlwidgets::saveWidget(plot_list[[loc_name]], path, selfcontained = TRUE)
              file_paths <- c(file_paths, path)
            }
          }
          zip::zipr(zipfile = file, files = file_paths, root = temp_dir)
        } else {
          htmlwidgets::saveWidget(plot_list[["single"]], file, selfcontained = TRUE)
        }
      }
    )
  })
}


# ===================================================================
# -------- 2. DATA CURATION & OUTLIER ANALYSIS (SUB-MODULE) --------
# ===================================================================

#' Data Curation UI (Sub-module)
#' @param id A unique identifier for the sub-module.
dataCurationUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 3, uiOutput(ns("curation_controls_ui"))),
    mainPanel(width = 9, uiOutput(ns("curation_main_ui")))
  )
}

#' Data Curation Server (Sub-module)
#' @param id A unique identifier for the sub-module.
#' @param home_inputs A reactive list from the home module.
dataCurationServer <- function(id, home_inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(original_data = NULL, outlier_report = NULL)
    
    observeEvent(home_inputs(), {
      req(home_inputs()$analysis_mode == "trait_explorer", home_inputs()$explorer_type == "curation")
      df <- home_inputs()$file_data
      df$pbat_row_id <- 1:nrow(df)
      rv$original_data <- df
      rv$outlier_report <- NULL
    })
    
    output$curation_controls_ui <- renderUI({
      df <- rv$original_data; req(df)
      all_cols <- names(df)
      numeric_cols <- all_cols[sapply(df, is.numeric)]
      
      selected_geno <- intersect(c("Genotype", "Entry", "GEN"), all_cols)[1]
      selected_env <- intersect(c("Location", "Environment", "LOC"), all_cols)[1]
      
      tagList(
        h4("Step 1: Map Data Columns"),
        selectInput(ns("geno_col"), "Genotype Column", choices = all_cols, selected = selected_geno),
        
        selectInput(ns("trial_type"), "Select Trial Type",
                    choices = c("Single Environment", "Multi Environment"),
                    selected = "Single Environment"),
        conditionalPanel(
          condition = "input.trial_type == 'Multi Environment'", ns = ns,
          selectInput(ns("env_col"), "Environment/Location Column", choices = all_cols, selected = selected_env)
        ),
        
        hr(),
        h4("Step 2: Define Analysis Scope & Method"),
        checkboxGroupInput(ns("traits_to_check"), "Trait(s) to Analyze", choices = numeric_cols, selected = numeric_cols[1]),
        
        conditionalPanel(
          condition = "input.trial_type == 'Multi Environment' && input.env_col != 'None'", ns = ns,
          radioButtons(ns("analysis_scope"), "Analysis Scope",
                       choices = c("Within each Environment" = "environment", "Globally (across all data)" = "global"),
                       selected = "environment")
        ),
        
        selectInput(ns("outlier_method"), "Outlier Detection Method",
                    choices = c("Standard Deviation" = "sd",
                                "Interquartile Range" = "iqr",
                                "SD and IQR Combined" = "combined",
                                "Bonferroni-Holm" = "holm")),
        conditionalPanel(
          condition = "input.outlier_method == 'sd' || input.outlier_method == 'combined'", ns = ns,
          numericInput(ns("sd_threshold"), "SD Threshold", value = 3, min = 1, step = 0.5)
        ),
        hr(),
        h4("Step 3: Run Analysis"),
        actionButton(ns("run_curation"), "Find Potential Outliers", class = "btn btn-primary", style = "width: 100%;")
      )
    })
    
    observeEvent(input$run_curation, {
      req(rv$original_data, input$traits_to_check)
      
      df <- rv$original_data
      grouping_env_col <- if(!is.null(input$env_col) && input$trial_type == 'Multi Environment') input$env_col else NULL
      
      factor_cols <- c(input$geno_col, grouping_env_col)
      for (col in factor_cols) { if (!is.null(col) && col %in% names(df)) df[[col]] <- as.factor(df[[col]]) }
      
      traits <- input$traits_to_check
      method <- input$outlier_method
      groups <- if (!is.null(input$analysis_scope) && input$analysis_scope == "environment" && !is.null(grouping_env_col)) {
        grouping_env_col
      } else {
        character(0)
      }
      
      find_outliers <- function(data, trait, group_vars, method, sd_thresh) {
        if (length(group_vars) > 0 && all(group_vars %in% names(data))) {
          data <- data %>% group_by(across(all_of(group_vars)))
        }
        
        if (method == "sd") {
          data <- data %>% mutate(
            sd_val = sd(.data[[trait]], na.rm = TRUE),
            is_outlier = if_else(is.na(sd_val) | sd_val == 0, FALSE, abs(.data[[trait]] - mean(.data[[trait]], na.rm = TRUE)) > (sd_thresh * sd_val)),
            Flagged_By = "SD"
          )
        } else if (method == "iqr") {
          data <- data %>% mutate(
            q1 = quantile(.data[[trait]], 0.25, na.rm = TRUE),
            q3 = quantile(.data[[trait]], 0.75, na.rm = TRUE),
            iqr_val = q3 - q1,
            is_outlier = if_else(is.na(iqr_val) | iqr_val == 0, FALSE, .data[[trait]] < q1 - 1.5 * iqr_val | .data[[trait]] > q3 + 1.5 * iqr_val),
            Flagged_By = "IQR"
          )
        } else if (method == "holm") {
          data <- data %>% mutate(
            sd_val = sd(.data[[trait]], na.rm = TRUE),
            p_val = if_else(is.na(sd_val) | sd_val == 0, 1, pnorm(abs(scale(.data[[trait]])), lower.tail = FALSE) * 2),
            p_adj = p.adjust(p_val, method = "holm"),
            is_outlier = p_adj < 0.05,
            Flagged_By = "Holm"
          )
        } else if (method == "combined") {
          data <- data %>% mutate(
            sd_val = sd(.data[[trait]], na.rm = TRUE),
            outlier_sd = if_else(is.na(sd_val) | sd_val == 0, FALSE, abs(.data[[trait]] - mean(.data[[trait]], na.rm = TRUE)) > (sd_thresh * sd_val)),
            q1 = quantile(.data[[trait]], 0.25, na.rm = TRUE),
            q3 = quantile(.data[[trait]], 0.75, na.rm = TRUE),
            iqr_val = q3 - q1,
            outlier_iqr = if_else(is.na(iqr_val) | iqr_val == 0, FALSE, .data[[trait]] < q1 - 1.5 * iqr_val | .data[[trait]] > q3 + 1.5 * iqr_val),
            is_outlier = outlier_sd | outlier_iqr,
            Flagged_By = case_when(outlier_sd & outlier_iqr ~ "SD & IQR", outlier_sd ~ "SD", outlier_iqr ~ "IQR", TRUE ~ NA_character_)
          )
        }
        
        data <- data %>% filter(is_outlier & !is.na(is_outlier))
        if (length(group_vars) > 0 && all(group_vars %in% names(data))) { data <- data %>% ungroup() }
        return(data)
      }
      
      all_outliers <- lapply(traits, function(trait) {
        res <- find_outliers(df, trait, groups, method, input$sd_threshold)
        if (nrow(res) > 0) {
          res$Trait_Flagged <- trait
        }
        return(res)
      }) %>% bind_rows()
      
      rv$outlier_report <- all_outliers
      showNotification(paste("Found", nrow(all_outliers), "potential outlier entries."), type = "message")
    })
    
    output$curation_main_ui <- renderUI({
      if (is.null(rv$outlier_report)) {
        return(tags$div(h4("Map your data columns and click 'Find Potential Outliers' to begin.", style = "color: grey; text-align: center; margin-top: 50px;")))
      }
      tabsetPanel(
        tabPanel("Visual Summary", uiOutput(ns("visual_summary_tabs"))),
        tabPanel("Outlier Report",
                 tableOutput(ns("outlier_table")),
                 hr(),
                 uiOutput(ns("manual_curation_instructions_ui"))
        )
      )
    })
    
    output$visual_summary_tabs <- renderUI({
      req(input$traits_to_check)
      plot_tabs <- lapply(input$traits_to_check, function(trait) {
        tabPanel(title = trait, plotly::plotlyOutput(ns(paste0("plot_for_", trait)), height = "600px"))
      })
      do.call(tabsetPanel, plot_tabs)
    })
    
    observe({
      req(rv$original_data, input$traits_to_check, input$geno_col, rv$outlier_report)
      
      lapply(input$traits_to_check, function(trait) {
        output[[paste0("plot_for_", trait)]] <- plotly::renderPlotly({
          df <- rv$original_data
          outliers <- rv$outlier_report
          
          if (nrow(outliers) > 0 && "Trait_Flagged" %in% names(outliers)) {
            outliers_for_this_trait <- outliers %>% filter(Trait_Flagged == trait)
            ids_for_this_trait <- outliers_for_this_trait$pbat_row_id
          } else {
            ids_for_this_trait <- integer(0) 
          }
          
          df$outlier_flag <- ifelse(df$pbat_row_id %in% ids_for_this_trait, "Outlier", "Normal")
          
          x_axis_var <- if (input$trial_type == "Multi Environment" && !is.null(input$analysis_scope) && input$analysis_scope == "environment" && !is.null(input$env_col) && input$env_col != "None") {
            input$env_col
          } else {
            "All Data"
          }
          
          df$x_group <- if(x_axis_var == "All Data") "All Data" else as.factor(df[[x_axis_var]])
          
          threshold_df <- df %>%
            group_by(x_group) %>%
            summarise(
              q1 = quantile(.data[[trait]], 0.25, na.rm = TRUE),
              q3 = quantile(.data[[trait]], 0.75, na.rm = TRUE),
              iqr = q3 - q1,
              upper_bound = q3 + 1.5 * iqr,
              lower_bound = q1 - 1.5 * iqr,
              .groups = 'drop'
            )
          
          # *** FIX 2: Corrected the ggplot aesthetic for the tooltip ***
          p <- ggplot(df, aes(x = x_group, y = !!sym(trait))) +
            geom_boxplot(outlier.shape = NA) +
            geom_jitter(
              aes(
                color = outlier_flag,
                text = paste0("Genotype: ", .data[[input$geno_col]], 
                              "<br>", trait, ": ", round(.data[[trait]], 2))
              ), 
              width = 0.2
            ) +
            {
              if (input$outlier_method %in% c("iqr", "combined")) {
                list(
                  geom_crossbar(data = threshold_df, aes(x = x_group, y = upper_bound, ymin = upper_bound, ymax = upper_bound), 
                                linetype = "dashed", color = "darkblue", width = 0.8),
                  geom_crossbar(data = threshold_df, aes(x = x_group, y = lower_bound, ymin = lower_bound, ymax = lower_bound), 
                                linetype = "dashed", color = "darkblue", width = 0.8)
                )
              }
            } +
            scale_color_manual(values = c("Normal" = "black", "Outlier" = "red")) +
            labs(
              title = paste("Outlier Visualization for", trait),
              subtitle = if (input$outlier_method %in% c("iqr", "combined")) "Dashed lines indicate the 1.5 * IQR outlier thresholds" else "",
              x = x_axis_var,
              y = trait
            ) +
            theme_minimal()
          
          ggplotly(p, tooltip = "text")
        })
      })
    })
    
    output$outlier_table <- renderTable({
      req(rv$outlier_report)
      
      report <- rv$outlier_report
      # Define columns to show, excluding the internal row ID
      cols_to_show <- c("Trait_Flagged", names(rv$original_data)[!names(rv$original_data) %in% "pbat_row_id"], "Flagged_By")
      
      # Select and rename columns for the final report
      final_report <- report %>% 
        select(
          any_of(cols_to_show), 
          any_of(c("sd_val", "iqr_val", "p_adj"))
        ) %>%
        rename(
          "SD_of_Group" = any_of("sd_val"),
          "IQR_of_Group" = any_of("iqr_val"),
          "Adj_P_Value" = any_of("p_adj")
        )
      
      final_report
    })
    
    output$manual_curation_instructions_ui <- renderUI({
      req(rv$outlier_report)
      
      if (nrow(rv$outlier_report) > 0) {
        tagList(
          tags$div(style = "padding: 20px; border: 1px solid #ddd; border-radius: 8px; background-color: #f8f9fa; margin-top: 20px;",
                   h4(style="color: #007bff;", "Manual Curation: Next Steps"),
                   p("This app has identified potential outliers but will ", tags$b("not"), " automatically change your original data. To curate your data, please follow this manual workflow:"),
                   tags$ol(
                     tags$li(HTML("<b>Download the Report:</b> Click the green button below to save a CSV file that lists only the flagged outliers.")),
                     tags$li(HTML("<b>Compare and Edit:</b> Open your <b>original dataset</b> and the <b>outlier report</b> side-by-side in a spreadsheet program (like Excel). Use the report to find the exact rows and specific trait values that were flagged.")),
                     tags$li(HTML("<b>Decide and Act:</b> For each flagged value in your original file, decide on an action:")),
                     tags$ul(
                       tags$li(HTML("<b>To treat as missing:</b> Replace them with NA")),
                       tags$li(HTML("<b>To remove the entire observation:</b> Delete the whole row if the record is unreliable.")),
                       tags$li(HTML("<b>To keep the value:</b> If you believe the outlier is a valid, true biological value, simply leave it as is."))
                     ),
                     tags$li(HTML("<b>Save Your Curated File:</b> Save your edited dataset as a <b>new CSV file</b>.")),
                     tags$li(HTML("<b>Re-upload for Analysis:</b> Go back to the <b>Home</b> tab and upload your new, curated file to proceed with your main analysis (e.g., Experimental Design, Stability)."))
                   ),
                   br(),
                   div(align = "center",
                       downloadButton(ns("download_report"), "Download Outlier Report (.csv)", class = "btn-success btn-lg")
                   )
          )
        )
      } else {
        tags$div(style = "padding: 15px; text-align: center;",
                 h4("No outliers were detected based on the selected criteria.")
        )
      }
    })
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("outlier_report_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(rv$outlier_report)
        report_to_download <- rv$outlier_report %>% select(-pbat_row_id)
        readr::write_csv(report_to_download, file, na = "")
      }
    )
    
  })
}