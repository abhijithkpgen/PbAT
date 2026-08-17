# R/design_experiment_module.R

# Required libraries for this module
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# ===================================================================
# CUSTOM DESIGN GENERATION FUNCTIONS
# ===================================================================

#' Generate a Randomized Complete Block Design (RCBD) Layout
#'
#' Creates an RCBD field book from a list of treatments and replications.
#'
#' @param treatments A character vector of treatment/genotype names.
#' @param reps The number of replications (blocks).
#' @return A data frame representing the field book.
custom_design_rcbd <- function(treatments, reps) {
  # Create the full list of plots for all replications
  full_design <- expand.grid(treatments = treatments, replication = 1:reps)
  
  # For each replication, randomize the order of treatments
  randomized_design <- full_design %>%
    group_by(replication) %>%
    mutate(treatments = sample(treatments)) %>%
    ungroup() %>%
    mutate(
      plots = 100 * replication + row_number() - (replication - 1) * length(treatments),
      block = replication # In a simple RCBD, blocks are equivalent to replications
    ) %>%
    select(plots, replication, block, treatments) %>%
    arrange(plots)
  
  return(randomized_design)
}


#' Generate an Augmented RCBD Layout (Robust Base R Version)
#'
#' Creates an Augmented RCBD field book using only base R to ensure robust randomization.
#'
#' @param test_treatments A character vector of the main treatments to be tested.
#' @param check_treatments A character vector of the check/control treatments.
#' @param blocks The total number of blocks in the experiment.
#' @return A data frame representing the field book.
custom_design_augmented_rcbd <- function(test_treatments, check_treatments, blocks) {
  t <- length(test_treatments)
  c <- length(check_treatments)
  
  # Add filler entries if necessary to ensure even distribution
  if (t %% blocks != 0) {
    n_fillers <- blocks - (t %% blocks)
    fillers <- paste0("Filler_", 1:n_fillers)
    test_treatments <- c(test_treatments, fillers)
    t <- length(test_treatments)
  }
  
  plots_per_block_test <- t / blocks
  
  # 1. Create a data frame with test entries assigned to blocks
  shuffled_tests <- sample(test_treatments)
  test_df <- data.frame(
    block = rep(1:blocks, each = plots_per_block_test),
    treatments = shuffled_tests,
    stringsAsFactors = FALSE
  )
  
  # 2. Create a data frame for the check treatments, replicated in each block
  check_df <- expand.grid(
    block = 1:blocks,
    treatments = check_treatments,
    stringsAsFactors = FALSE
  )
  
  # 3. Combine test and check data frames
  unrandomized_df <- rbind(test_df, check_df)
  
  # 4. For each block, shuffle the order of all treatments (test and checks together)
  randomized_list <- list()
  for (b in 1:blocks) {
    block_subset <- unrandomized_df[unrandomized_df$block == b, ]
    block_subset$treatments <- sample(block_subset$treatments)
    randomized_list[[b]] <- block_subset
  }
  
  # 5. Combine the randomized blocks back into one data frame
  field_book <- do.call(rbind, randomized_list)
  
  # 6. Assign plot numbers and Type, then finalize the order
  field_book <- field_book[order(field_book$block), ]
  field_book$plots <- 101:(100 + nrow(field_book))
  field_book$Type <- ifelse(field_book$treatments %in% check_treatments, "Check", "Test")
  
  field_book <- field_book[, c("plots", "block", "treatments", "Type")]
  
  return(field_book)
}


#' Generate an Alpha Lattice Design Layout
#'
#' Creates an Alpha Lattice design field book from scratch.
#'
#' @param treatments A character vector of treatment/genotype names.
#' @param k The number of plots per block (block size).
#' @param r The number of replications.
#' @return A data frame representing the field book.
custom_design_alpha <- function(treatments, k, r) {
  v <- length(treatments)
  if (v %% k != 0) {
    stop("The total number of treatments must be a multiple of the block size (k).")
  }
  
  b <- v / k # Number of blocks per replication
  
  # 1. Create a base plan (matrix of treatment numbers)
  base_plan <- matrix(1:v, nrow = k, byrow = TRUE)
  
  # 2. Generate the layout for all replications
  all_reps <- list()
  for (rep in 1:r) {
    if (rep == 1) {
      current_treatments <- sample(treatments)
    } else {
      permuted_indices <- as.vector(t(apply(base_plan, 1, sample)))
      current_treatments <- treatments[permuted_indices]
    }
    shuffled_blocks <- sample(1:b)
    rep_df <- data.frame(
      replication = rep,
      block = rep(shuffled_blocks, each = k),
      treatments = current_treatments
    )
    all_reps[[rep]] <- rep_df
  }
  
  # 3. Combine all replications and assign plot numbers
  field_book <- bind_rows(all_reps) %>%
    arrange(replication, block) %>%
    mutate(plots = 100 * replication + row_number() - (replication - 1) * v) %>%
    select(plots, replication, block, treatments) %>%
    arrange(plots)
  
  return(field_book)
}


# ===================================================================
# MODULE UI
# ===================================================================
designExperimentUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Design Your Trial",
           div(
             class = "alert alert-info shadow-sm", 
             style = "margin: 0px 0px 20px 0px; border-left: 5px solid #1F4E79; background-color: #f8f9fa; color: #333; padding: 12px 20px; border-radius: 8px; font-size: 14px;",
             icon("lightbulb", style="color:#f39c12; font-size: 16px; margin-right: 8px;"), 
             tags$b("Feeling stuck?"), 
             " Check out ", 
             tags$a("this video tutorial.", href = "https://youtu.be/obwfvVxWULI", target = "_blank", style="color: #1F4E79; font-weight: bold; text-decoration: underline;")
           ),
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h4("Trial Design Generator"),
               p("Select a design, input your genotypes, set parameters, and generate a field layout."),
               uiOutput(ns("design_sidebar_ui")) 
             ),
             mainPanel(
               width = 9,
               tabsetPanel(
                 tabPanel("Visualize Layout", 
                          h4("Interactive Field Plot"),
                          p("Hover over the plots to see details. Use the tools in the top-right corner to pan, zoom, and save a static image."),
                          plotlyOutput(ns("layout_plot"), height = "750px")
                 ),
                 tabPanel("Field Book", 
                          h4("Generated Field Layout Table"),
                          p("This table shows the randomized layout for your experiment. You can sort, search, and copy the data."),
                          tableOutput(ns("design_table"))
                 )
               )
             )
           )
  )
}

# ===================================================================
# MODULE SERVER
# ===================================================================
designExperimentServer <- function(id, home_inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(design_output = NULL, plot_object = NULL)
    
    geno_data <- reactiveVal(NULL)
    
    active_design <- reactive({
      req(home_inputs()$analysis_mode == "design_exp")
      home_inputs()$design_type_home
    })
    
    output$design_sidebar_ui <- renderUI({
      design <- active_design()
      req(design)
      
      step_header <- function(step, txt) h4(div(style="margin-bottom:6px;margin-top:10px;color:#216E60;font-size:16px;", paste0("Step ", step, ": ", txt)))
      
      tagList(
        step_header(1, "Genotype Input"),
        radioButtons(ns("geno_input_type"), "Genotype Input Method",
                     choices = c("Custom", "System Generated"), selected = "Custom"),
        
        conditionalPanel(
          condition = paste0("input['", ns("geno_input_type"), "'] == 'Custom'"),
          radioButtons(ns("custom_input_method"), "Custom Input Type",
                       choices = c("Upload CSV", "Paste Names"), selected = "Upload CSV"),
          
          conditionalPanel(
            condition = paste0("input['", ns("custom_input_method"), "'] == 'Upload CSV'"),
            fileInput(ns("geno_file"), "Upload Genotypes CSV", accept = ".csv"),
            uiOutput(ns("map_columns_ui"))
          ),
          
          conditionalPanel(
            condition = paste0("input['", ns("custom_input_method"), "'] == 'Paste Names'"),
            textAreaInput(ns("pasted_test_geno"), "Paste Test Genotypes (one per line)"),
            if (design == "augmented") {
              textAreaInput(ns("pasted_check_geno"), "Paste Check Genotypes (one per line)")
            }
          )
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("geno_input_type"), "'] == 'System Generated'"),
          numericInput(ns("num_test_geno"), "Number of Test Genotypes", value = 50, min = 1),
          textInput(ns("prefix_test_geno"), "Prefix for Test Genotypes", value = "G"),
          if (design == "augmented") {
            tagList(
              numericInput(ns("num_check_geno"), "Number of Check Genotypes", value = 5, min = 1),
              textInput(ns("prefix_check_geno"), "Prefix for Check Genotypes", value = "C")
            )
          }
        ),
        
        hr(),
        step_header(2, "Set Parameters"),
        if (design == "rcbd") {
          tagList(
            numericInput(ns("rcbd_reps"), "Number of Replications", value = 3, min = 2),
            uiOutput(ns("rcbd_reps_suggestion_ui"))
          )
        } else if (design == "augmented") {
          tagList(
            numericInput(ns("aug_blocks"), "Number of Blocks", value = 5, min = 2),
            uiOutput(ns("aug_block_suggestion_ui"))
          )
        } else if (design == "alpha") {
          tagList(
            numericInput(ns("alpha_reps"), "Number of Replications", value = 2, min = 2),
            numericInput(ns("alpha_b"), "Number of Blocks per Replication", value = 5, min = 2),
            uiOutput(ns("alpha_block_suggestion_ui"))
          )
        },
        hr(),
        step_header(3, "Generate & Download"),
        actionButton(ns("generate_design"), "Generate Design", class = "btn-primary", style="width:100%; margin-bottom: 15px;"),
        fluidRow(
          column(6, style = "padding-right: 5px;",
                 downloadButton(ns("download_csv"), "CSV Field Book", class="btn-outline-success", style="width:100%; padding: 6px; font-size: 13px;")),
          column(6, style = "padding-left: 5px;",
                 downloadButton(ns("download_pdf"), "Layout PDF", class="btn-outline-info", style="width:100%; padding: 6px; font-size: 13px;"))
        )
      )
    })
    
    observeEvent(input$geno_file, {
      req(input$geno_file)
      df <- read.csv(input$geno_file$datapath)
      geno_data(df)
    })
    
    output$map_columns_ui <- renderUI({
      df <- geno_data()
      design <- active_design()
      req(df, design)
      
      col_names <- names(df)
      
      if (design == "augmented") {
        tagList(
          selectInput(ns("test_col_aug"), "Test Genotypes Column", choices = col_names, selected = col_names[1]),
          selectInput(ns("check_col_aug"), "Check Genotypes Column", choices = col_names, selected = col_names[2])
        )
      } else {
        selectInput(ns("geno_col_std"), "Genotype Column", choices = col_names)
      }
    })
    
    output$rcbd_reps_suggestion_ui <- renderUI({
      helpText(HTML("<b>Statistical Tip:</b> Using at least 3-4 replications is generally recommended to ensure a reliable estimate of experimental error."))
    })
    
    output$aug_block_suggestion_ui <- renderUI({
      design <- active_design()
      req(design == "augmented")
      
      n_test <- 0
      n_check <- 0
      
      if (input$geno_input_type == "Custom") {
        if (input$custom_input_method == "Upload CSV") {
          df <- geno_data()
          req(df, input$test_col_aug, input$check_col_aug)
          n_test <- length(unique(na.omit(df[[input$test_col_aug]])))
          n_check <- length(unique(na.omit(df[[input$check_col_aug]])))
        } else { # Paste names
          n_test <- length(unique(scan(text = input$pasted_test_geno, what = "", quiet = TRUE)))
          n_check <- length(unique(scan(text = input$pasted_check_geno, what = "", quiet = TRUE)))
        }
      } else { # System generated
        req(input$num_test_geno, input$num_check_geno)
        n_test <- input$num_test_geno
        n_check <- input$num_check_geno
      }
      
      
      get_factors <- function(n) {
        if (n <= 1) return(integer(0))
        x <- 1:floor(sqrt(n))
        factors <- x[n %% x == 0]
        unique(sort(c(factors, n/factors)))
      }
      
      divisors <- get_factors(n_test)
      suggested_divisors <- divisors[divisors > 1 & divisors <= (n_test/2)]
      
      suggestion_text_filler <- if (length(suggested_divisors) > 0) {
        paste("To avoid filler plots with", n_test, "test entries, use one of these block numbers:", paste(suggested_divisors, collapse=", "))
      } else {
        paste("With", n_test, "test entries, filler plots will likely be needed to ensure equal block sizes.")
      }
      
      min_df_error <- 12
      suggestion_text_stats <- NULL
      if (n_check > 1) {
        min_blocks_for_df <- ceiling(min_df_error / (n_check - 1)) + 1
        suggestion_text_stats <- paste0("For statistical validity with ", n_check, " checks, it is recommended to use at least ", min_blocks_for_df, " blocks to achieve a reliable error estimate (>=12 df).")
      } else {
        suggestion_text_stats <- "Note: With only one check entry, the error degrees of freedom cannot be estimated from checks alone. Consider adding more checks for a more robust analysis."
      }
      
      tagList(
        helpText(HTML(paste0("<b>Layout Tip:</b> ", suggestion_text_filler))),
        helpText(HTML(paste0("<b>Statistical Tip:</b> ", suggestion_text_stats)))
      )
    })
    
    output$alpha_block_suggestion_ui <- renderUI({
      design <- active_design()
      req(design == "alpha")
      
      n_geno <- 0
      
      if (input$geno_input_type == "Custom") {
        if (input$custom_input_method == "Upload CSV") {
          df <- geno_data()
          req(df, input$geno_col_std)
          n_geno <- length(unique(na.omit(df[[input$geno_col_std]])))
        } else { # Paste names
          n_geno <- length(unique(scan(text = input$pasted_test_geno, what = "", quiet = TRUE)))
        }
      } else { # System generated
        req(input$num_test_geno)
        n_geno <- input$num_test_geno
      }
      
      get_factors <- function(n) {
        if (n <= 1) return(integer(0))
        x <- 1:floor(sqrt(n))
        factors <- x[n %% x == 0]
        unique(sort(c(factors, n/factors)))
      }
      
      divisors <- get_factors(n_geno)
      divisors <- divisors[divisors > 1]
      
      suggestion_text <- if (length(divisors) > 0) {
        paste("With", n_geno, "genotypes, valid choices for 'Number of Blocks per Replication' are:", paste(divisors, collapse=", "))
      } else {
        paste("With", n_geno, "genotypes, a valid Alpha Lattice design cannot be formed. Please adjust the number of genotypes.")
      }
      
      helpText(HTML(paste0("<b>Design Tip:</b> ", suggestion_text)))
    })
    
    
    observeEvent(input$generate_design, {
      
      design <- active_design()
      req(design)
      
      gen_names <- character(0)
      check_names <- character(0)
      
      tryCatch({
        if (input$geno_input_type == "Custom") {
          if (input$custom_input_method == "Upload CSV") {
            df <- geno_data()
            req(df)
            if (design %in% c("rcbd", "alpha")) {
              req(input$geno_col_std)
              gen_names <- unique(na.omit(df[[input$geno_col_std]]))
            } else if (design == "augmented") {
              req(input$test_col_aug, input$check_col_aug)
              gen_names <- unique(na.omit(df[[input$test_col_aug]]))
              check_names <- unique(na.omit(df[[input$check_col_aug]]))
            }
          } else { # Paste Names
            gen_names <- unique(scan(text = input$pasted_test_geno, what = "", quiet = TRUE))
            if (design == "augmented") {
              check_names <- unique(scan(text = input$pasted_check_geno, what = "", quiet = TRUE))
            }
          }
        } else { # System Generated
          gen_names <- paste0(input$prefix_test_geno, 1:input$num_test_geno)
          if (design == "augmented") {
            check_names <- paste0(input$prefix_check_geno, 1:input$num_check_geno)
          }
        }
        
        if (length(gen_names) == 0) {
          stop("No valid test genotypes were found.")
        }
        if (design == "augmented" && length(check_names) == 0) {
          stop("Augmented design requires at least one check genotype.")
        }
        
        layout <- switch(
          design,
          "rcbd" = custom_design_rcbd(treatments = gen_names, reps = input$rcbd_reps),
          "augmented" = custom_design_augmented_rcbd(test_treatments = gen_names, check_treatments = check_names, blocks = input$aug_blocks),
          "alpha" = {
            b <- input$alpha_b
            v <- length(gen_names)
            if (v %% b != 0) {
              stop("The total number of genotypes must be a multiple of the number of blocks.")
            }
            k <- v / b
            custom_design_alpha(treatments = gen_names, k = k, r = input$alpha_reps)
          }
        )
        
        plot_data <- layout
        names(plot_data)[names(plot_data) == "plots"] <- "Plot"
        names(plot_data)[names(plot_data) == "treatments"] <- "Genotype"
        names(plot_data)[names(plot_data) == "replication"] <- "Replication"
        names(plot_data)[names(plot_data) == "block"] <- "Block"
        
        total_blocks <- length(unique(plot_data$Block))
        block_positions <- data.frame(Block = unique(plot_data$Block))
        block_positions <- block_positions[sample(nrow(block_positions)), , drop = FALSE]
        block_positions$Row <- 1:nrow(block_positions)
        
        plot_data <- merge(plot_data, block_positions, by = "Block")
        
        plot_data <- plot_data %>%
          group_by(Block) %>%
          mutate(Column = 1:n()) %>%
          ungroup()
        
        plot_data$box_label <- if("Replication" %in% names(plot_data)) {
          paste0(plot_data$Genotype, "\n(R", plot_data$Replication, ")")
        } else {
          plot_data$Genotype
        }
        
        plot_data$tooltip <- paste0(
          "Plot: ", plot_data$Plot,
          "<br>Genotype: ", plot_data$Genotype,
          "<br>Row: ", plot_data$Row,
          "<br>Column: ", plot_data$Column,
          if ("Block" %in% names(plot_data)) paste("<br>Block:", plot_data$Block) else "",
          if ("Replication" %in% names(plot_data)) paste("<br>Replication:", plot_data$Replication) else ""
        )
        
        final_df <- plot_data[order(plot_data$Plot), ]
        
        if (design %in% c("rcbd", "alpha")) {
          final_cols <- c("Plot", "Row", "Column", "Replication", "Block", "Genotype")
        } else {
          final_cols <- c("Plot", "Row", "Column", "Block", "Genotype", "Type")
        }
        final_cols_exist <- final_cols[final_cols %in% names(final_df)]
        rv$design_output <- final_df[, final_cols_exist]
        
        # Dynamically scale text size based on number of columns so it fits in PDF
        num_cols <- max(plot_data$Column, na.rm = TRUE)
        dynamic_text_size <- ifelse(num_cols > 40, 1.5, ifelse(num_cols > 20, 2.2, 3))
        
        p <- ggplot(plot_data, aes(x = as.factor(Column), y = as.factor(Row), text = tooltip, fill = as.factor(Genotype))) +
          geom_tile(color = "black", linewidth = 0.6, width = 0.98, height = 0.98) +
          geom_text(aes(label = box_label), size = dynamic_text_size, color = "black", fontface = "bold", angle = 90, lineheight = 0.8) +
          labs(title = paste("Field Layout for", toupper(design)), x = "Column", y = "Row", fill = "Genotype") +
          theme_minimal(base_size = 14) +
          theme(
            panel.grid = element_blank(), 
            legend.position = "none",
            axis.text = element_text(color = "#333333"),
            plot.title = element_text(face = "bold", color = "#1F4E79")
          )
        
        if (design == "augmented") {
          check_data <- plot_data[plot_data$Genotype %in% check_names, ]
          if (nrow(check_data) > 0) {
            p <- p + geom_tile(
              data = check_data, aes(color = "Check Plots"), 
              fill = NA, linewidth = 1.5, width = 0.98, height = 0.98, inherit.aes = TRUE
            ) + scale_color_manual(name = "", values = c("Check Plots" = "#e74c3c"))
          }
        }
        
        rv$plot_object <- p
        showNotification("Design and plot generated successfully!", type = "message")
        
      }, error = function(e) {
        showModal(modalDialog(title = "Error Generating Design", e$message))
        rv$design_output <- NULL
        rv$plot_object <- NULL
      })
    })
    
    # --- Render Outputs ---
    output$design_table <- renderTable({
      req(rv$design_output)
      rv$design_output
    })
    
    output$layout_plot <- plotly::renderPlotly({
      req(rv$plot_object)
      p <- plotly::ggplotly(rv$plot_object, tooltip = "text") %>%
        plotly::layout(margin = list(t = 50, b = 50, l = 50, r = 50))
      
      # Force text rotation in Plotly (since ggplotly ignores geom_text angle)
      for (i in seq_along(p$x$data)) {
        if (!is.null(p$x$data[[i]]$mode) && grepl("text", p$x$data[[i]]$mode)) {
          p$x$data[[i]]$textangle <- -90
        }
      }
      p
    })
    
    # --- Download Handlers ---
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0(active_design(), "_field_book_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(rv$design_output)
        write.csv(rv$design_output, file, row.names = FALSE)
      }
    )
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0(active_design(), "_layout_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(rv$plot_object)
        ggsave(file, plot = rv$plot_object, device = "pdf", width = 11, height = 8.5, units = "in")
      }
    )
    
  })
}

