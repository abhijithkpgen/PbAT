# R/app_ui.R

app_ui <- function() {
  navbarPage(
    title = div(
      style = "display: flex; align-items: center; gap: 16px;",
      tags$img(src = "www/LogoNobg.png", height = "50px", style = "margin-right: 8px;"),
      span("PbAT: Plant breeding Analytical Tools v1.0.5", style = "font-weight: 800; font-size: 1.4rem;")
    ),
    id = "main_navbar", 
    theme = bslib::bs_theme(
      version = 5,
      bg = "#FFFFFF", 
      fg = "black",
      primary = "#1F4E79", 
      secondary = "#3FA796",
      "navbar-light-bg" = "white"
    ),
    header = tagList(
      shinyjs::useShinyjs(),
      waiter::use_waiter(),
      
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
      body {
        background: #4AC29A;
        background: -webkit-linear-gradient(to right, #BDFFF3, #4AC29A);
        background: linear-gradient(to right, #BDFFF3, #4AC29A);
        font-family: 'Inter', sans-serif;
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
        padding: 25px;
        border-radius: 12px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.08);
        flex: 1; 
        min-width: 360px; 
        max-width: 480px; 
        font-size: 14px;
        color: #333333 !important;
        border: 1px solid #e9ecef;
      }
      
      /* --- Button Styling --- */
      .btn {
        border-radius: 8px;
        font-weight: bold;
        text-transform: uppercase;
        padding: 10px 15px;
        transition: all 0.2s ease-in-out;
      }
      .btn-primary {
        background-image: linear-gradient(to right, #1565C0, #1E88E5) !important;
        border: none !important;
        font-size: 16px;
        padding: 12px 20px;
      }
      .btn-primary:hover {
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        transform: translateY(-2px);
      }

      /* --- Workflow Selection Box --- */
      .workflow-box .shiny-input-radiogroup > label {
        font-size: 22px;
        font-weight: 700;
        color: #1F4E79;
        margin-bottom: 15px;
      }
      .workflow-box .radio label {
        display: flex;
        align-items: center;
        width: 100%;
        padding: 12px 15px;
        border-radius: 6px;
        border: 1px solid #ced4da;
        background-color: #F1F3F4;
        color: #495057;
        cursor: pointer;
        transition: all 0.2s ease-in-out;
        margin-bottom: 8px;
      }
      .workflow-box .radio label:hover {
        background-color: #E3F2FD;
        border-left: 4px solid #3FA796;
        padding-left: 11px;
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

      /* --- Tabs --- */
      .nav-tabs .nav-link {
        border-bottom: 2px solid transparent;
      }
      .nav-tabs .nav-link.active, .nav-tabs .nav-item.show .nav-link {
        color: #1F4E79 !important;
        border-color: transparent transparent #1F4E79 transparent !important;
        font-weight: 600;
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
      
    ")))
    ),
    
    tabPanel("Home", homeUI(id = "home")),
    
    designExperimentUI(id = "design_experiment"), 
    
    navbarMenu("Experimental Design",
               analysisUI(id = "eda")[[1]], 
               analysisUI(id = "eda")[[2]]
    ),
    
    traitExplorerUI(id = "trait_explorer"),
    
    stability_analysis_ui(id = "stability"),
    
    mating_design_ui(id = "mating"),
    
    multivariate_analysis_ui(id = "multi"),
    
    # <<< START OF MODIFIED SECTION >>>
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
                                    tags$blockquote("Abhijith, K. P., Vinod, K. K., Ellur, R. K., Ravikiran, K. T., Saxena, R. K., Muthusamy, V., & S, Gopalakrishnan. (2025).PbAT: Plant Breeding Analytical Tools (v1.0.5). Zenodo. https://doi.org/10.5281/zenodo.17020132")
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
                                    tags$blockquote("Bates, D., Mchler, M., Bolker, B., & Walker, S. (2015). Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1-48.")
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
    # <<< END OF MODIFIED SECTION >>>
    
    tabPanel("Help & Guide",
             fluidPage(
               div(style = "padding: 30px;",
                   h2("Help & Guide", style = "color: #23272b;"),
                   tabsetPanel(
                     id = "help_tabs",
                     tabPanel("Sample Data",
                              div(style = "padding-top: 20px;",
                                  h3("Sample Data Downloads"),
                                  p("Disclaimer: The example datasets provided in this application are simulated for demonstration purposes only. They do not represent actual experimental results and should not be used for research conclusions."),
                                  tags$ul(
                                    tags$li(tags$a(href = "www/Alpha_lattice_sample.csv", "Alpha Lattice Sample CSV", download = NA, target = "_blank")),
                                    tags$li(tags$a(href = "www/Augmented_RCBD_Sample.csv", "Augmented RCBD Sample CSV", download = NA, target = "_blank")),
                                    tags$li(tags$a(href = "www/Diallel_Griffing_Method1_Sample.csv", "Griffing Method I Diallel Sample CSV", download = NA, target = "_blank")),
                                    tags$li(tags$a(href = "www/Diallel_Griffing_Method2_Sample.csv", "Griffing Method II Diallel Sample CSV", download = NA, target = "_blank")),
                                    tags$li(tags$a(href = "www/Diallel_Griffing_Method3_Sample.csv", "Griffing Method III Diallel Sample CSV", download = NA, target = "_blank")),
                                    tags$li(tags$a(href = "www/Diallel_Griffing_Method4_Sample.csv", "Griffing Method IV Diallel Sample CSV", download = NA, target = "_blank")),
                                    tags$li(tags$a(href = "www/Factorial_CRD_sample.csv", "Factorial CRD Sample CSV", download = NA, target = "_blank")),
                                    tags$li(tags$a(href = "www/Line_x_Tester_Sample.csv", "Line x Tester Sample CSV", download = NA, target = "_blank")),
                                    tags$li(tags$a(href = "www/Partial_diallel_dummy.csv", "Partial Diallel Sample CSV", download = NA, target = "_blank")),
                                    tags$li(tags$a(href = "www/AMMI_GGE_Sample_Data.csv", "Biplot Sample Format CSV", download = NA, target = "_blank")),
                                    tags$li(tags$a(href = "www/Mult_Variate_sample_format.csv", "Multivariate Analysis Sample Format CSV", download = NA, target = "_blank")),
                                    tags$li(tags$a(href = "www/RCBD_sample.csv", "RCBD Sample CSV", download = NA, target = "_blank"))
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