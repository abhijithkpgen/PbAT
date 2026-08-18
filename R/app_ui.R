# R/app_ui.R
app_ui <- function() {
  navbarPage(
    title = div(
      style = "display: flex; align-items: center;",
      tags$img(src = "www/LogoNobg.png", height = "50px", style = "margin-right: 15px;"),
      span("PbAT: Plant breeding Analytical Tools", style = "font-weight: 800; font-size: 1.4rem; color: #1F4E79; margin-right: 10px;"),
      span("v1.1.0", style = "background: linear-gradient(135deg, #3FA796, #218370); color: white; padding: 4px 12px; border-radius: 20px; font-size: 0.85rem; font-weight: 700; box-shadow: 0 3px 8px rgba(63,167,150,0.35); letter-spacing: 0.5px; text-transform: uppercase; margin-bottom: 2px;")
    ),
    id = "main_navbar", 
    theme = bslib::bs_theme(
      version = 5,
      bg = "#FFFFFF", 
      fg = "black",
      primary = "#1F4E79", 
      secondary = "#3FA796",
      base_font = "Inter",
      heading_font = "Inter",
      "navbar-light-bg" = "white"
    ),
    header = tagList(
      shinyjs::useShinyjs(),
      waiter::use_waiter(),
      shinyjs::hidden(
        DT::DTOutput("hidden_dt_dependency"),
        plotly::plotlyOutput("hidden_plotly_dependency")
      ),
      
      waiter::waiter_show_on_load(
        html = tagList(
          tags$div(
            style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
            tags$img(src = "www/LogoNobg.png", height = "200px", style = "margin-bottom: 20px;"),
            waiter::spin_fading_circles()
          )
        ),
        color = "#3FA796" 
      ),
      
      tags$head(
        # --- Google Analytics Script ---
        tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-5NMMGN97MY"),
        tags$script(HTML("
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());
          gtag('config', 'G-5NMMGN97MY');
        ")),
        
        # --- ADD THIS LINE FOR THE FAVICON ---
        tags$link(rel = "icon", type = "image/png", href = "www/LogoNobg.png"),
        
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
        tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
        
        tags$style(HTML("
      /* --- Main Body & Layout --- */
      :root {
        --bs-body-font-family: 'Inter', sans-serif !important;
        --bs-heading-font-family: 'Inter', sans-serif !important;
      }
      body {
        background: #5EBEB0;
        background: linear-gradient(135deg, #7CD1C4 0%, #4FB3A4 100%);
        font-family: 'Inter', sans-serif !important;
      }
      .home-container {
        display: flex;
        justify-content: flex-start; /* Pushes content to the left */
        padding-left: 50px; /* Adds some space from the edge */
        align-items: flex-start;
        flex-wrap: wrap; 
        min-height: 85vh;
        gap: 30px; 
        margin-top: 40px;
        background: none;
      }
      
      /* --- Navbar Styling --- */
      .navbar .navbar-brand, .navbar .nav-link, .navbar .navbar-nav .nav-link {
        color: #1F4E79 !important;
        font-weight: 1000;
      }
      .navbar .nav-link:hover {
        color: #1F4E79 !important;
      }
      
      /* --- CORRECTED RULE: Add space to the LEFT of the tabs --- */
          .navbar-nav {
            margin-left: 50px !important; /* Adjust this value for more/less space */
          }
      /* --- NEW: Custom class for the active navbar tab --- */
      .custom-active-tab {
        font-weight: 700 !important;
        color: #E67E22 !important; 
        background-color: #FEF5E7 !important; /* Light orange background */
        border-bottom: 3px solid #E67E22 !important;
        border-radius: 5px 5px 0 0;
      }
      
      /* --- Panel Styling (Cards) --- */
      .card-panel {
        background-color: #FFFFFF !important;
        padding: 30px;
        border-radius: 16px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.05);
        flex: 1; 
        min-width: 360px; 
        max-width: 480px; 
        font-size: 14.5px;
        color: #333333 !important;
        border: none;
      }
      
      /* --- Button Styling --- */
      .btn {
        border-radius: 8px;
        font-weight: 700;
        letter-spacing: 0.5px;
        text-transform: uppercase;
        transition: all 0.25s ease-in-out;
      }
      .btn-primary {
        background: linear-gradient(135deg, #1F4E79, #3FA796) !important;
        border: none !important;
        color: #ffffff !important;
        font-size: 15px;
        padding: 14px 24px;
        box-shadow: 0 4px 15px rgba(31, 78, 121, 0.25);
      }
      .btn-primary:hover {
        box-shadow: 0 6px 20px rgba(31, 78, 121, 0.4);
        transform: translateY(-2px);
      }
      /* --- Workflow Selection Box --- */
      .workflow-box .shiny-input-radiogroup > label {
        font-size: 22px;
        font-weight: 800;
        color: #1F4E79;
        margin-bottom: 20px;
        letter-spacing: -0.5px;
      }
      .workflow-box .radio label {
        display: flex;
        align-items: center;
        width: 100%;
        padding: 14px 18px;
        border-radius: 10px;
        border: 1px solid #e2e8f0;
        background-color: #f8fafc;
        color: #475569;
        cursor: pointer;
        transition: all 0.2s ease-in-out;
        margin-bottom: 10px;
      }
      .workflow-box .radio label:hover {
        background-color: #f1f5f9;
        border-left: 4px solid #3FA796;
        padding-left: 15px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.03);
      }
      .workflow-box .radio input[type='radio'] {
        display: none;
      }
      .workflow-box .radio label.active-workflow {
        background-color: #3FA796 !important;
        border-color: #3FA796 !important;
        color: white !important;
        font-weight: 600;
        box-shadow: 0 2px 6px rgba(63, 167, 150, 0.4);
      }
      .workflow-box .radio i {
        margin-right: 12px;
        width: 20px;
      }
      
      /* --- File Upload Area --- */
      .file-upload-area {
        border: 2px dashed #ced4da;
        border-radius: 8px;
        padding: 20px;
        text-align: center;
        background-color: #F1F3F4;
        margin-top: 15px;
        margin-bottom: 15px;
        transition: background-color 0.2s ease-in-out;
      }
      .file-upload-area:hover {
        background-color: #E3F2FD;
      }
      /* --- Sidebar & Forms Styling for High Contrast --- */
      .well {
        background-color: rgba(255, 255, 255, 0.95) !important;
        border: none !important;
        border-radius: 12px !important;
        box-shadow: 0 6px 20px rgba(0,0,0,0.1) !important;
        padding: 25px !important;
      }
      .well h3, .well h4 {
        color: #216E60 !important;
        font-weight: 800 !important;
        margin-bottom: 10px;
      }
      .well .control-label, .well label {
        color: #1F4E79 !important;
        font-weight: 700 !important;
        font-size: 14px;
        margin-bottom: 8px;
      }
      .well p, .well .help-block {
        color: #333333 !important;
      }
      
      /* --- Tabs --- */
      .nav-tabs {
        border-bottom: none;
        margin-bottom: 15px;
      }
      .nav-tabs .nav-link {
        color: #1F4E79;
        font-weight: 600;
        background-color: rgba(255, 255, 255, 0.6);
        border: none !important;
        margin-right: 8px;
        border-radius: 8px 8px 0 0;
        padding: 10px 20px;
        transition: all 0.2s ease;
      }
      .nav-tabs .nav-link:hover {
        background-color: rgba(255, 255, 255, 0.85);
      }
      .nav-tabs .nav-link.active, .nav-tabs .nav-item.show .nav-link {
        color: #1F4E79 !important;
        background-color: #FFFFFF !important;
        border-bottom: 3px solid #3FA796 !important;
        font-weight: 800;
        box-shadow: 0 -4px 10px rgba(0,0,0,0.05);
      }
      
      /* --- Help Text / Tips --- */
      .help-block {
        background-color: #F5F9FF;
        border-left: 4px solid #007bff;
        padding: 10px;
        border-radius: 4px;
        font-size: 13px;
      }
      /* --- Workflow Overview Styling --- */
      .workflow-overview ol {
        list-style: none;
        padding-left: 0;
      }
      .workflow-overview li {
        display: flex;
        align-items: flex-start;
        margin-bottom: 12px;
        padding-bottom: 12px;
        border-bottom: 1px solid #e9ecef;
      }
      .workflow-overview li:last-child {
        border-bottom: none;
      }
      .workflow-overview .step-number {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 24px;
        height: 24px;
        border-radius: 50%;
        background-color: #1F4E79;
        color: white;
        font-weight: bold;
        margin-right: 15px;
        flex-shrink: 0;
      }
      
      /* --- Video Tutorial Cards Hover Effect --- */
      .video-card {
        transition: transform 0.3s ease, box-shadow 0.3s ease;
      }
      .video-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 12px 24px rgba(0,0,0,0.15) !important;
      }
      
      ")))
    ),
    
    tabPanel("Home", homeUI(id = "home")),
    
    designExperimentUI(id = "design_experiment"),
    
    analysisUI(id = "eda"),
    
    traitExplorerUI(id = "trait_explorer"),
    
    stability_analysis_ui(id = "stability"),
    
    mating_design_ui(id = "mating"),
    
    selection_index_ui(id = "selection_index"),
    
    multivariate_analysis_ui(id = "multivariate"),
    
    # <<< ABOUT TAB >>>
    tabPanel("About",
             fluidPage(
               div(style = "padding: 30px;",
                   h2("About PbAT", style = "color: #1F4E79; font-weight: bold;"),
                   tabsetPanel(
                     id = "about_tabs",
                     tabPanel("Our Mission",
                              div(style = "padding: 20px; max-width: 800px; margin: 20px auto; background-color: transparent; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.06);",
                                  br(),
                                  p("Hi,"),
                                  p("Thanks for checking out, PbAT."),
                                  p("Statistical analysis lies at the heart of plant breeding, but too often, the complexity of coding stands in the way. We understand that, sometimes researchers end up spending more time wrestling with programming than advancing their science."),
                                  p("Thats why we created PbAT (Plant Breeding Analytical Tools)."),
                                  p("PbAT is a free, open-access web application designed to break down those barriers. It unifies the entire analytical pipeline, trial design, data curation, experimental design analyses, stability assessments, multivariate approaches, and mating designs into one seamless, code free workflow."),
                                  p("We hope PbAT makes your analyses simpler, and we would be delighted if you could share your feedback, constraints, suggestions and obviously the bugs at ",
                                    tags$a(href="mailto:abhijithkpgen@gmail.com", "abhijithkpgen@gmail.com"),
                                    " to help us further improve the application."),
                                  p("We invite you to explore PbAT, streamline your analysis, and spend more time where it matters most, on discovery and innovation in plant breeding."),
                                  br(),
                                  p("Sincerely,"),
                                  p(tags$b("The PbAT Team"))
                              )
                     ),
                     tabPanel("Citation Recommendation",
                              div(style = "padding-top: 20px;",
                                  h3("Citation Recommendations"),
                                  p("If you use PbAT in your research, please cite this application and if you happen to use any of these below analyses please cite the core R packages that perform the analyses."),
                                  
                                  div(
                                    h4("For the PbAT Application:", style="color:#1F4E79;"),
                                    tags$blockquote("Abhijith, K. P., K. K. Vinod, R. K. Ellur, K. T. Ravikiran, R. K. Saxena, V. Muthusamy, and S. G. Krishnan. (2026). PbAT: A user friendly R/Shiny platform for data driven decision support in crop improvement. Applications in Plant Sciences, 14, e70068.")
                                  ),
                                  
                                  div(
                                    h4("For Path Analysis:", style="color:#1F4E79;"),
                                    tags$blockquote("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36.")
                                  ),
                                  
                                  div(
                                    h4("For Stability Analysis (AMMI/GGE):", style="color:#1F4E79;"),
                                    tags$blockquote("Olivoto, T., & Gabriel, L. (2019). metan: An R package for multi-environment trial analysis. Methods in Ecology and Evolution, 10(6), 760-768.")
                                  ),
                                  
                                  div(
                                    h4("For Mixed Model Analysis:", style="color:#1F4E79;"),
                                    tags$blockquote("Bates, D., Maechler, M., Bolker, B., & Walker, S. (2015). Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1-48.")
                                  ),
                                  
                                  div(
                                    h4("For Principal Component Analysis (PCA):", style="color:#1F4E79;"),
                                    tags$blockquote("Kassambara, A., & Mundt, F. (2020). factoextra: Extract and Visualize the Results of Multivariate Data Analyses. R package version 1.0.7.")
                                  ),
                                  
                                  div(
                                    h4("For Graphical Outputs:", style="color:#1F4E79;"),
                                    tags$blockquote("Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York.")
                                  )
                              )
                     )
                   )
               )
             )
    ),
    
    # <<< NEW TUTORIALS TAB (3-COLUMN COMPACT LAYOUT) >>>
    tabPanel("Tutorials",
             fluidPage(
               div(style = "padding: 30px; max-width: 1400px; margin: 0 auto;",
                   h2(icon("youtube", class = "fab"), " Video Tutorials", style = "color: #1F4E79; font-weight: bold; margin-bottom: 5px;"),
                   p("Watch these step-by-step guides to understand how to prepare your data and perform analysis. ", 
                     tags$b("Tip: Click the 'Fullscreen' icon in the bottom right corner of any video to enlarge it."), 
                     style = "font-size: 15px; color: #555; margin-bottom: 30px;"),
                   
                   # Row 1
                   fluidRow(
                     column(4,
                            div(class = "video-card", style = "margin-bottom: 30px; padding: 15px; background-color: #FFFFFF; border-radius: 12px; box-shadow: 0 4px 8px rgba(0,0,0,0.05); border-top: 4px solid #3FA796;",
                                h5("1. Design Your Trial", style="color:#1F4E79; font-weight:bold; margin-top:0; font-size: 16px;"),
                                p("Learn how to design a plant breeding trial and generate field layouts.", style="font-size: 12px; color: #666;"),
                                tags$div(style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden; border-radius: 8px;",
                                         tags$iframe(src = "https://www.youtube.com/embed/obwfvVxWULI", style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: 0;", allowfullscreen = NA)
                                )
                            )
                     ),
                     column(4,
                            div(class = "video-card", style = "margin-bottom: 30px; padding: 15px; background-color: #FFFFFF; border-radius: 12px; box-shadow: 0 4px 8px rgba(0,0,0,0.05); border-top: 4px solid #3FA796;",
                                h5("2. Trait Explorer", style="color:#1F4E79; font-weight:bold; margin-top:0; font-size: 16px;"),
                                p("Curate your data, perform outlier analysis, and visualize spatial trends.", style="font-size: 12px; color: #666;"),
                                tags$div(style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden; border-radius: 8px;",
                                         tags$iframe(src = "https://www.youtube.com/embed/jLcn08iG9gw", style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: 0;", allowfullscreen = NA)
                                )
                            )
                     ),
                     column(4,
                            div(class = "video-card", style = "margin-bottom: 30px; padding: 15px; background-color: #FFFFFF; border-radius: 12px; box-shadow: 0 4px 8px rgba(0,0,0,0.05); border-top: 4px solid #3FA796;",
                                h5("3. Experimental Design Analysis", style="color:#1F4E79; font-weight:bold; margin-top:0; font-size: 16px;"),
                                p("Perform linear mixed models, generate BLUEs/BLUPs, and view diagnostics.", style="font-size: 12px; color: #666;"),
                                div(class = "video-container", style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden; max-width: 100%; border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.1);",
                                         tags$iframe(src = "https://www.youtube.com/embed/fwLlfDG8hYk", style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: 0;", allowfullscreen = NA)
                                      )
                            )
                     )
                   ),
                   
                   # Row 2
                   fluidRow(
                     column(4,
                            div(class = "video-card", style = "margin-bottom: 30px; padding: 15px; background-color: #FFFFFF; border-radius: 12px; box-shadow: 0 4px 8px rgba(0,0,0,0.05); border-top: 4px solid #3FA796;",
                                h5("4. Stability Analysis", style="color:#1F4E79; font-weight:bold; margin-top:0; font-size: 16px;"),
                                p("Perform AMMI and GGE biplot analysis for multi-environment trials.", style="font-size: 12px; color: #666;"),
                                tags$div(style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden; border-radius: 8px;",
                                         tags$iframe(src = "https://www.youtube.com/embed/-VA_RWqC1eE", style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: 0;", allowfullscreen = NA)
                                )
                            )
                     ),
                     column(4,
                            div(class = "video-card", style = "margin-bottom: 30px; padding: 15px; background-color: #FFFFFF; border-radius: 12px; box-shadow: 0 4px 8px rgba(0,0,0,0.05); border-top: 4px solid #3FA796;",
                                h5("5. Multivariate Analysis", style="color:#1F4E79; font-weight:bold; margin-top:0; font-size: 16px;"),
                                p("Learn how to conduct PCA, Correlation, and Path analysis.", style="font-size: 12px; color: #666;"),
                                tags$div(style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden; border-radius: 8px;",
                                         tags$iframe(src = "https://www.youtube.com/embed/pRT17EjHCOM", style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: 0;", allowfullscreen = NA)
                                )
                            )
                     ),
                     column(4,
                            div(class = "video-card", style = "margin-bottom: 30px; padding: 15px; background-color: #FFFFFF; border-radius: 12px; box-shadow: 0 4px 8px rgba(0,0,0,0.05); border-top: 4px solid #3FA796;",
                                h5("6. Mating Design Analysis", style="color:#1F4E79; font-weight:bold; margin-top:0; font-size: 16px;"),
                                p("Estimate GCA/SCA using Line x Tester and Griffing Diallel methods.", style="font-size: 12px; color: #666;"),
                                tags$div(style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden; border-radius: 8px;",
                                         tags$iframe(src = "https://www.youtube.com/embed/PTAxnGZWr1o", style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: 0;", allowfullscreen = NA)
                                )
                            )
                     )
                   )
               )
             )
    ),
    
    # <<< HELP & GUIDE TAB >>>
    tabPanel("Help & Guide",
             fluidPage(
               div(style = "padding: 30px;",
                   h2("Help & Guide", style = "color: #23272b;"),
                   tabsetPanel(
                     id = "help_tabs",
                     tabPanel("Sample Data",
                              div(style = "padding-top: 20px;",
                                  h3("Sample Data Downloads"),
                                  p("Disclaimer: The example datasets provided in this application are simulated for demonstration purposes only. They do not represent actual experimental results and should not be used for research conclusions."),                                  tags$style("
                                    .sample-data-grid {
                                      display: grid;
                                      grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
                                      gap: 20px;
                                      margin-top: 25px;
                                    }
                                    .sample-tile {
                                      background: white;
                                      border-radius: 12px;
                                      padding: 18px 20px;
                                      box-shadow: 0 4px 12px rgba(0,0,0,0.05);
                                      border-left: 5px solid #3FA796;
                                      text-decoration: none !important;
                                      color: #1F4E79;
                                      transition: all 0.25s ease;
                                      display: flex;
                                      align-items: center;
                                      gap: 15px;
                                    }
                                    .sample-tile:hover {
                                      transform: translateY(-4px);
                                      box-shadow: 0 8px 24px rgba(31,78,121,0.12);
                                      background: #f8fafc;
                                    }
                                    .sample-tile i {
                                      font-size: 24px;
                                      color: #3FA796;
                                      transition: color 0.25s ease;
                                    }
                                    .sample-tile:hover i {
                                      color: #1F4E79;
                                    }
                                    .sample-tile .title {
                                      font-weight: 700;
                                      font-size: 14.5px;
                                      line-height: 1.3;
                                    }
                                  "),
                                  div(class = "sample-data-grid",
                                    tags$a(class = "sample-tile", href = "www/Alpha_lattice_sample.csv", download = NA, target = "_blank",
                                           icon("file-csv"), tags$div(class="title", "Alpha Lattice Sample")),
                                    tags$a(class = "sample-tile", href = "www/Augmented_RCBD_Sample.csv", download = NA, target = "_blank",
                                           icon("file-csv"), tags$div(class="title", "Augmented RCBD Sample")),
                                    tags$a(class = "sample-tile", href = "www/Diallel_Griffing_Method1_Sample.csv", download = NA, target = "_blank",
                                           icon("file-csv"), tags$div(class="title", "Griffing Method I Diallel")),
                                    tags$a(class = "sample-tile", href = "www/Diallel_Griffing_Method2_Sample.csv", download = NA, target = "_blank",
                                           icon("file-csv"), tags$div(class="title", "Griffing Method II Diallel")),
                                    tags$a(class = "sample-tile", href = "www/Diallel_Griffing_Method3_Sample.csv", download = NA, target = "_blank",
                                           icon("file-csv"), tags$div(class="title", "Griffing Method III Diallel")),
                                    tags$a(class = "sample-tile", href = "www/Diallel_Griffing_Method4_Sample.csv", download = NA, target = "_blank",
                                           icon("file-csv"), tags$div(class="title", "Griffing Method IV Diallel")),
                                    tags$a(class = "sample-tile", href = "www/Factorial_CRD_sample.csv", download = NA, target = "_blank",
                                           icon("file-csv"), tags$div(class="title", "Factorial CRD Sample")),
                                    tags$a(class = "sample-tile", href = "www/Line_x_Tester_Sample.csv", download = NA, target = "_blank",
                                           icon("file-csv"), tags$div(class="title", "Line x Tester Sample")),
                                    tags$a(class = "sample-tile", href = "www/Partial_diallel_dummy.csv", download = NA, target = "_blank",
                                           icon("file-csv"), tags$div(class="title", "Partial Diallel Sample")),
                                    tags$a(class = "sample-tile", href = "www/AMMI_GGE_Sample_Data.csv", download = NA, target = "_blank",
                                           icon("file-csv"), tags$div(class="title", "Biplot Sample Format")),
                                    tags$a(class = "sample-tile", href = "www/Mult_Variate_sample_format.csv", download = NA, target = "_blank",
                                           icon("file-csv"), tags$div(class="title", "Multivariate Analysis Sample")),
                                    tags$a(class = "sample-tile", href = "www/RCBD_sample.csv", download = NA, target = "_blank",
                                           icon("file-csv"), tags$div(class="title", "RCBD Sample"))
                                  )
                               )
                     ),
                     tabPanel("Troubleshooting",
                              div(style = "padding-top: 20px;",
                                  h3("Troubleshooting Common Issues"),
                                  p("Encountering an issue? Most problems with complex model analyses are related to network connection timeouts or temporary rendering glitches. Here are a few simple steps you can take to resolve common errors."),
                                  h4("Problem: Results Not Appearing After Running an Analysis or  incomplete user interface appear after proceeding to analysis"),
                                  p(HTML("<b>Cause:</b> This can happen when either the app hasn't loaded full or incases where an analysis takes a while to complete, especially on a slower internet connection, or if there's a temporary glitch while displaying the results. The analysis likely finished successfully on the server, but the results weren't displayed correctly in your browser.")),
                                  h4("Solutions (Try these in order):"),
                                  tags$ul(
                                    tags$li(HTML("<b>1. Reload and Rerun:</b><br>This is the easiest and most common fix. If the results area or input area is blank, simply reload the entire web page and run the analysis again. This resolves most temporary rendering issues.")),
                                    tags$li(HTML("<b>2. Ensure a Stable Internet Connection:</b><br>Since these analyses involve sending data and waiting for results, a stable connection is key. If you are on a weak Wi-Fi signal, try moving closer to your router or connect to a more reliable network before rerunning the analysis.")),
                                    
                                    tags$li(HTML("<b>4. Be Patient:</b><br>A complex mixed-model analysis on a large dataset can take some time. After clicking 'Run,' please allow up to a minute for the server to process before assuming there is an error.")),
                                    tags$li(HTML("<b>5. Use the R Package Locally:</b><br>For very large datasets or complex multi-trait analyses, consider installing the PbAT R package  and running it on your own computer for the smoothest and fastest experience."))
                                    
                                  )
                              )
                     )
                   )
               )
             )
    )
  )
}