# R/home_module.R

homeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Absolutely positioned container for the GIF
    tags$div(
      style = "position: absolute; top: 120px; right: 0px; z-index: 1;",
      tags$img(src = "www/pbat_loop.gif", style = "width: 100%; max-width: 250px; height: auto;")
    ),
    
    # Original container for the interactive cards
    div(
      class = "home-container",
      div(class = "card-panel",
          # This div creates the box around the radio buttons
          div(class = "workflow-box",
              radioButtons(
                ns("analysis_mode"),
                "Select Analysis Workflow",
                choiceNames = list(
                  tagList(icon("ruler-combined"), "Design Your Trial"),
                  tagList(icon("search"), "Trait Explorer"),
                  tagList(icon("table"), "Experimental Design"),
                  tagList(icon("chart-line"), "Stability Analysis"),
                  tagList(icon("project-diagram"), "Multivariate Analysis"),
                  tagList(icon("dna"), "Mating Design")
                ),
                choiceValues = c(
                  "design_exp", "trait_explorer", "eda", 
                  "stability", "multivariate", "mating"
                ),
                selected = "design_exp"
              )
          ),
          
          # --- Conditional UIs for workflow-specific settings ---
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'design_exp'"),
            selectInput(ns("design_type_home"), "Select Design Type",
                        choices = c("Randomized Complete Block Design (RCBD)" = "rcbd",
                                    "Augmented RCBD" = "augmented",
                                    "Alpha Lattice Design" = "alpha"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'trait_explorer'"),
            selectInput(ns("explorer_type"), "Select Explorer Type", choices = c("Spatial Trait Explorer" = "spatial", "Data Curation & Outlier Analysis" = "curation"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'stability'"),
            selectInput(ns("stab_subtype"), "Select The Stability Analysis?", choices = c("AMMI Analysis" = "ammi", "GGE Biplot" = "gge"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'multivariate'"),
            selectInput(ns("multi_subtype"), "Select The Multivariate Analysis", choices = c("Principal Component Analysis (PCA)" = "pca", "Correlation Analysis" = "correlation", "Path Analysis" = "path"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'eda'"),
            selectInput(ns("design"), "Select Experimental Design", choices = c("Alpha Lattice", "RCBD", "CRD", "Augmented RCBD"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'mating'"),
            selectInput(ns("md_mating_design"), "Select Mating Design", choices = c("Griffing Method I (Full Diallel: Parents, F1s, Reciprocals)" = "griffing_m1", "Griffing Method II (Parents & F1s, No Reciprocals)" = "griffing_m2", "Griffing Method III (F1s & Reciprocals, No Parents)" = "griffing_m3", "Griffing Method IV (F1s Only, No Parents, No Reciprocals)" = "griffing_m4", "Partial Diallel" = "diallel_partial", "Line x Tester" = "line_tester"))
          ),
          
          # --- MODIFIED: File Input is now at the bottom for relevant modes ---
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] != 'design_exp'"),
            div(class="file-upload-area",
                fileInput(ns("file"), tagList(icon("upload"), "Upload CSV File for Analysis"), accept = c("text/csv", ".csv"))
            )
          ),
          
          actionButton(ns("go_to_analysis"), "Proceed to Analysis", class = "btn btn-primary btn-block"),
          br(),br(),
          tags$div(style = "font-size: 11px; text-align: center; color: #6c757d;",
                   HTML("Developed by <b>Abhijith et al. (2025)</b><br>
                   ICAR-Indian Agricultural Research Institute, Assam<br>"),
                   p("Contact for reporting bugs, queries, or feedback:"),
                   tags$a(href = "mailto:pbatinfo@gmail.com", 
                          style = "font-weight: bold; color: #1F4E79;",
                          icon("envelope"), "pbatinfo@gmail.com"),
                   br(), br(),
                   tags$div(
                     class = "custom-footer",
                     HTML("Released under the <a href='https://github.com/abhijithkpgen/PBAT/blob/main/LICENSE' target='_blank'>GPL-3.0 License</a>.")
                   )
          )
      ),
      
      uiOutput(ns("workflow_overview_ui"))
    )
  )
}

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    results_to_return <- reactiveVal()
    
    observeEvent(input$analysis_mode, {
      js_selector <- paste0("#", ns("analysis_mode"), " .radio label")
      js_checked_selector <- paste0("#", ns("analysis_mode"), " input:checked")
      
      shinyjs::runjs(
        sprintf("
          $('%s').removeClass('active-workflow');
          $('%s').parent('label').addClass('active-workflow');
        ", js_selector, js_checked_selector)
      )
    }, ignoreNULL = FALSE, ignoreInit = TRUE) 
    
    observeEvent(input$go_to_analysis, {
      
      mode <- input$analysis_mode
      df <- NULL
      
      if (mode != "design_exp") {
        req(input$file)
        df <- readr::read_csv(input$file$datapath, na = c("", "NA"), show_col_types = FALSE)
      }
      
      settings <- list(
        file_data = df,
        analysis_mode = mode,
        design_type_home = input$design_type_home,
        explorer_type = input$explorer_type,
        design = input$design,
        stab_subtype = input$stab_subtype,
        multi_subtype = input$multi_subtype,
        mating_design = input$md_mating_design,
        trigger = runif(1)
      )
      results_to_return(settings)
    })
    
    output$workflow_overview_ui <- renderUI({
      
      mode <- input$analysis_mode
      
      panel_content <- switch(
        mode,
        "design_exp" = tagList(
          h4(tags$b("Design Your Trial Workflow")),
          div(class="workflow-overview", tags$ol(
            tags$li(span(class="step-number", "1"), div(tags$b("Select Design Type:"), "Choose from RCBD, Augmented RCBD, or Alpha Lattice.")),
            tags$li(span(class="step-number", "2"), div(tags$b("Input Genotypes:"), "Upload a CSV or paste names for your test (and check) genotypes in the next screen.")),
            tags$li(span(class="step-number", "3"), div(tags$b("Set Parameters:"), "Specify the number of replications, blocks, etc.")),
            tags$li(span(class="step-number", "4"), div(tags$b("Generate & Download:"), "Create and visualize the randomized field plan and download the field book as a CSV."))
          ))
        ),
        "trait_explorer" = tagList(
          h4(tags$b("Trait Explorer Workflow")),
          div(class="workflow-overview", tags$ol(
            tags$li(span(class="step-number", "1"), div(tags$b("Upload Data:"), "Provide your raw field data CSV.")),
            tags$li(span(class="step-number", "2"), div(tags$b("Select Explorer Type:"), tags$ul(
              tags$li(tags$b("Spatial Explorer:"), "Visualize the distribution of a trait across the physical layout of your field to identify spatial trends or gradients."),
              tags$li(tags$b("Data Curation:"), "Automatically detect potential outliers in your trait data using statistical methods (IQR, SD, etc.).")
            ))),
            tags$li(span(class="step-number", "3"), div(tags$b("Map Columns & Run:"), "Assign columns and generate plots or reports.")),
            tags$li(span(class="step-number", "4"), div(tags$b("Curate & Re-upload:"), "Download the outlier report, clean your original dataset, and re-upload the curated file for further analysis."))
          ))
        ),
        "eda" = tagList(
          h4(tags$b("Experimental Design Analysis Workflow")),
          div(class="workflow-overview", tags$ol(
            tags$li(span(class="step-number", "1"), div(tags$b("Upload Data:"), "Provide your collected trial data.")),
            tags$li(span(class="step-number", "2"), div(tags$b("Select Design:"), "Choose the design you used (e.g., RCBD, Alpha Lattice).")),
            tags$li(span(class="step-number", "3"), div(tags$b("Map Columns:"), "Assign columns for genotype, block, traits, etc.")),
            tags$li(span(class="step-number", "4"), div(tags$b("Run Analysis:"), "Generate descriptive statistics, ANOVA, variance components, heritability, and diagnostic plots.")),
            tags$li(span(class="step-number", "5"), div(tags$b("Calculate Estimates:"), "Compute Best Linear Unbiased Estimates (BLUEs) for fixed models or Predictors (BLUPs) for random models.")),
            tags$li(span(class="step-number", "6"), div(tags$b("Download:"), "Export all tables and plots in a publication-ready format."))
          ))
        ),
        "stability" = tagList(
          h4(tags$b("Stability Analysis Workflow")),
          div(class="workflow-overview", tags$ol(
            tags$li(span(class="step-number", "1"), div(tags$b("Upload Data:"), "Provide data from a multi-environment trial.")),
            tags$li(span(class="step-number", "2"), div(tags$b("Select Analysis:"), "Choose between AMMI or GGE Biplot analysis.")),
            tags$li(span(class="step-number", "3"), div(tags$b("Map Columns:"), "Assign columns for genotype, environment, replication, and the trait of interest.")),
            tags$li(span(class="step-number", "4"), div(tags$b("Run & Interpret:"), "Generate biplots to visualize GxE patterns, identify stable/adapted genotypes, and understand the 'which-won-where' scenario.")),
            tags$li(span(class="step-number", "5"), div(tags$b("Download:"), "Export stability indices, ANOVA tables, and high-quality biplots."))
          ))
        ),
        "multivariate" = tagList(
          h4(tags$b("Multivariate Analysis Workflow")),
          div(class="workflow-overview", tags$ol(
            tags$li(span(class="step-number", "1"), div(tags$b("Upload Data:"), "Provide your trial data with multiple measured traits.")),
            tags$li(span(class="step-number", "2"), div(tags$b("Select Analysis:"), "Choose PCA, Correlation, or Path Analysis.")),
            tags$li(span(class="step-number", "3"), div(tags$b("Select Traits:"), "Choose two or more traits for the analysis.")),
            tags$li(span(class="step-number", "4"), div(tags$b("Run & Visualize:"), "Generate plots (e.g., PCA biplots, correlograms, path diagrams) to understand trait associations, identify patterns, and determine direct/indirect effects.")),
            tags$li(span(class="step-number", "5"), div(tags$b("Download:"), "Export all plots and underlying data tables."))
          ))
        ),
        "mating" = tagList(
          h4(tags$b("Mating Design Analysis Workflow")),
          div(class="workflow-overview", tags$ol(
            tags$li(span(class="step-number", "1"), div(tags$b("Upload Data:"), "Provide your data from a mating design experiment.")),
            tags$li(span(class="step-number", "2"), div(tags$b("Select Design:"), "Choose from Line x Tester or various Diallel methods.")),
            tags$li(span(class="step-number", "3"), div(tags$b("Map Columns:"), "Assign columns for parents (or lines/testers), replication, and traits.")),
            tags$li(span(class="step-number", "4"), div(tags$b("Run Analysis:"), "Calculate ANOVA and estimate General (GCA) and Specific (SCA) Combining Ability effects.")),
            tags$li(span(class="step-number", "5"), div(tags$b("Interpret Results:"), "Identify the best general combiners for breeding programs and the best specific crosses for hybrid development."))
          ))
        )
      )
      
      div(class = "card-panel", panel_content)
    })
    
    return(results_to_return)
  })
}
