#' @importFrom shiny hideTab showTab updateNavbarPage updateTabsetPanel
# ===================================================================
# 4. DEFINE THE SERVER LOGIC
# ===================================================================
app_server <- function(input, output, session) {
  
  # --- START: Add this code block for the welcome pop-up ---
  
  # This observer triggers once when the app loads for a new user
  observeEvent(session$clientData$url_hostname, {
    showModal(modalDialog(
      title = tags$div(style = "display: flex; align-items: center;", 
                       icon("info-circle", style = "margin-right: 10px; color: #1F4E79;"), 
                       "Welcome to PbAT!"),
      HTML("New to the app? <br><br> Check out the <b>Help & Guide</b> section for tutorials and to download sample data files with the correct input formats.<br> Tip:Running into issues with output not showing? A quick app reload should do the trick."),
      footer = tagList(
        modalButton("Dismiss"),
        actionButton("go_to_help", "Take me to the Guide", class = "btn-success")
      ),
      easyClose = TRUE,
      size = "m"
    ))
  }, once = TRUE)
  
  # This observer listens for the button click inside the modal
  observeEvent(input$go_to_help, {
    updateNavbarPage(session, "main_navbar", selected = "Help & Guide")
    removeModal()
  })
  
  # --- END: Welcome Pop-up ---
  
  # --- Hide the pre-loader once the main UI is fully ready ---
  observeEvent(input$main_navbar, {
    shinyjs::delay(500, {
      waiter::waiter_hide()
    })
  }, once = TRUE)
  
  # --- START: JAVASCRIPT TO HIGHLIGHT ACTIVE TAB ---
  observeEvent(input$main_navbar, {
    # This JavaScript code finds the active tab link and adds your custom class
    # while removing it from any other tab that might have had it before.
    shinyjs::runjs(
      "
      // First, remove the custom class from all tab links
      $('#main_navbar .nav-link').removeClass('custom-active-tab');
      
      // Then, find the link inside the currently active list item and add the class
      $('#main_navbar .nav-item.active .nav-link').addClass('custom-active-tab');
      "
    )
  }, ignoreNULL = FALSE) # ignoreNULL = FALSE ensures it runs on app load
  # --- END: JAVASCRIPT HIGHLIGHT ---
  
  # --- Call the Home Module Server ---
  home_inputs <- homeServer(id = "home")
  
  # --- Create Reactive Values for each downstream module ---
  eda_shared_data <- reactiveVal()
  trait_explorer_shared_data <- reactiveVal()
  design_exp_shared_data <- reactiveVal()
  stability_shared_data <- reactiveValues(file_data = NULL, stab_subtype = NULL)
  mating_shared_data <- reactiveValues(file_data = NULL, mating_design = NULL)
  multi_shared_data  <- reactiveValues(file_data = NULL, multi_subtype = NULL)
  si_shared_data     <- reactiveValues(file_data = NULL)
  
  # --- Dynamic Tab Navigation (Hide/Show Tabs) ---
  observe({
    hideTab("main_navbar", "Design Your Trial")
    hideTab("main_navbar", "Experimental Design")
    hideTab("main_navbar", "Trait Explorer")
    hideTab("main_navbar", "Stability Analysis")
    hideTab("main_navbar", "Mating Design Analysis")
    hideTab("main_navbar", "Multivariate Analysis")
    hideTab("main_navbar", "Selection Index")
  })
  
  # --- Observer to Route Data and Switch Tabs ---
  observeEvent(home_inputs(), {
    req(home_inputs())
    mode <- home_inputs()$analysis_mode
    
    # Hide all analysis tabs first
    hideTab("main_navbar", "Design Your Trial")
    hideTab("main_navbar", "Experimental Design")
    hideTab("main_navbar", "Trait Explorer")
    hideTab("main_navbar", "Stability Analysis")
    hideTab("main_navbar", "Mating Design Analysis")
    hideTab("main_navbar", "Multivariate Analysis")
    hideTab("main_navbar", "Selection Index")
    
    # Show and select the right tab(s)
    if (mode == "design_exp") {
      design_exp_shared_data(home_inputs())
      showTab("main_navbar", "Design Your Trial")
      updateNavbarPage(session, "main_navbar", selected = "Design Your Trial")
      
    } else if (mode == "eda") {
      eda_shared_data(home_inputs())
      showTab("main_navbar", "Experimental Design")
      updateNavbarPage(session, "main_navbar", selected = "Experimental Design")
      
    } else if (mode == "trait_explorer") {
      trait_explorer_shared_data(home_inputs())
      showTab("main_navbar", "Trait Explorer")
      updateNavbarPage(session, "main_navbar", selected = "Trait Explorer")
      
      explorer_type <- home_inputs()$explorer_type
      if (!is.null(explorer_type)) {
        sub_tab_name <- if (explorer_type == "spatial") "Spatial Trait Explorer" else "Data Curation & Outlier Analysis"
        updateTabsetPanel(session, "trait_explorer-explorer_tabs", selected = sub_tab_name)
      }
      
    } else if (mode == "stability") {
      stability_shared_data$file_data <- home_inputs()$file_data
      stability_shared_data$stab_subtype <- home_inputs()$stab_subtype
      showTab("main_navbar", "Stability Analysis")
      updateNavbarPage(session, "main_navbar", selected = "Stability Analysis")
      
    } else if (mode == "mating") {
      mating_shared_data$file_data <- home_inputs()$file_data
      mating_shared_data$mating_design <- home_inputs()$mating_design
      showTab("main_navbar", "Mating Design Analysis")
      updateNavbarPage(session, "main_navbar", selected = "Mating Design Analysis")
      
    } else if (mode == "multivariate") {
      multi_shared_data$file_data <- home_inputs()$file_data
      multi_shared_data$multi_subtype <- home_inputs()$multi_subtype
      showTab("main_navbar", "Multivariate Analysis")
      updateNavbarPage(session, "main_navbar", selected = "Multivariate Analysis")
      
    } else if (mode == "selection_index") {
      si_shared_data$file_data <- home_inputs()$file_data
      showTab("main_navbar", "Selection Index")
      updateNavbarPage(session, "main_navbar", selected = "Selection Index")
    }
  })
  
  # --- Call the Server Logic for each Analysis Module ---
  designExperimentServer(id = "design_experiment", home_inputs = design_exp_shared_data) 
  
  exp_design_out <- analysisServer(id = "eda", home_inputs = eda_shared_data)
  
  observeEvent(exp_design_out$export_stab_event(), {
    req(exp_design_out$export_stab_event() > 0)
    data <- exp_design_out$export_stab_data()
    req(data)
    stability_shared_data$file_data <- data
    stability_shared_data$stab_subtype <- "gge" 
    showTab("main_navbar", "Stability Analysis")
    updateNavbarPage(session, "main_navbar", selected = "Stability Analysis")
    showNotification("Successfully exported Environment-wise results to Stability Analysis (GGE).", type = "message")
  })
  
  observeEvent(exp_design_out$export_multi_event(), {
    req(exp_design_out$export_multi_event() > 0)
    data <- exp_design_out$export_multi_data()
    req(data)
    multi_shared_data$file_data <- data
    multi_shared_data$multi_subtype <- exp_design_out$export_multi_type()
    showTab("main_navbar", "Multivariate Analysis")
    updateNavbarPage(session, "main_navbar", selected = "Multivariate Analysis")
    showNotification("Successfully exported Combined results to Multivariate Analysis.", type = "message")
  })
  
  traitExplorerServer(id = "trait_explorer", home_inputs = trait_explorer_shared_data)
  stability_analysis_server(id = "stability", shared_data = stability_shared_data)
  mating_design_server(id = "mating", shared_data = mating_shared_data)
  multivariate_analysis_server(id = "multivariate", shared_data = multi_shared_data)
  selection_index_server(id = "selection_index", raw_data = reactive({ si_shared_data$file_data }))
  
}