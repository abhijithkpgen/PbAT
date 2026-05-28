# mating_design_module.R
#
# This file contains the complete, corrected UI and Server logic for the Mating Design Analysis module.
# This version includes all statistical functions and incorporates all previous suggestions,
# including a database of equations and explanations with show/hide functionality.

# Required Libraries
library(shiny)
library(shinyjs)
library(dplyr)
library(emmeans)
library(tibble)

# ===================================================================
# MODULE UI FUNCTION
# ===================================================================
mating_design_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Mating Design Analysis",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(style = "color: #142850; font-size: 15px;",
            tags$div(
              tags$b("Analysis Setup"),
              br(),
              p("Please select your columns below based on the design chosen on the Home tab.")
            ),
            # This conditionalPanel's condition will be met based on a hidden input updated by the server
            conditionalPanel(
              condition = "input.is_griffing_design",
              ns = ns,
              tags$div(
                style = "background-color: #eef6fa; color: #115370; padding: 10px; margin-bottom: 10px; border-radius: 7px; font-size: 13px; border-left: 4px solid #1d7ec5;",
                HTML("<b>Note:</b> All Griffing diallel analyses here use <b>fixed effects models</b> (as per Griffing, 1956).")
              )
            ),
            tags$div(
              tags$b("Step 1: Select Columns"),
              br(),
              uiOutput(ns("md_dynamic_columns")),
              style = "margin-bottom: 18px;"
            ),
            tags$div(
              tags$b("Step 2: Run Analysis"),
              br(),
              actionButton(
                ns("md_run_analysis"),
                "Run Mating Analysis",
                class = "btn btn-primary",
                style = "width: 100%;"
              ),
              style = "margin-bottom: 18px;"
            ),
            tags$div(
              tags$b("Step 3: Download Results"),
              br(),
              downloadButton(ns("md_download_results"), "Download All Results", class = "btn btn-success", style = "width: 100%;"),
              style = "margin-bottom: 0px;"
            )
        )
      ),
      mainPanel(
        width = 9,
        # Use shinyjs to enable show/hide functionality
        useShinyjs(),
        div(style = "color: #142850;",
            tabsetPanel(
              id = ns("md_main_tabs"),
              tabPanel("ANOVA Table", uiOutput(ns("anova_output_ui"))),
              tabPanel("GCA Effects", uiOutput(ns("gca_output_ui"))),
              tabPanel("SCA Effects", uiOutput(ns("sca_output_ui")))
            )
        )
      )
    )
  )
}

# ===================================================================
# EQUATION AND EXPLANATION DATABASE
# ===================================================================
mating_design_equations <- list(
  griffing_m1 = list(
    model = "Yij =  + gi + gj + sij + rij",
    model_explanation = HTML("Where:<br><b></b>: Overall mean<br><b>gi, gj</b>: GCA effects for parents i and j<br><b>sij</b>: SCA effect for the cross<br><b>rij</b>: Reciprocal effect (difference between cross ij and ji)"),
    gca_formula = "i = (1/2p) * (Yi. + Y.i) - (1/p) * Y..",
    gca_explanation = HTML("The GCA effect for parent 'i'.<br><b>p</b>: Number of parents<br><b>Yi.</b>: Sum of values for parent 'i' as a female<br><b>Y.i</b>: Sum of values for parent 'i' as a male<br><b>Y..</b>: Grand total of the data matrix"),
    sca_formula = "ij = (1/2) * (Yij + Yji) - (1/2p) * (Yi. + Y.i) - (1/2p) * (Yj. + Y.j) + (1/p) * Y..",
    sca_explanation = "The SCA effect for the cross between 'i' and 'j'. It is the average performance of the cross and its reciprocal, adjusted for the GCA effects of both parents and the overall mean."
  ),
  griffing_m2 = list(
    model = "Yij =  + gi + gj + sij",
    model_explanation = "Where:  is the overall mean, gi and gj are the GCA effects, and sij is the SCA effect. Reciprocal effects are assumed to be absent.",
    gca_formula = "i = [1/(p+2)] * [Yi. + Yii - (2/p) * Y..]",
    gca_explanation = HTML("The GCA for parent 'i'.<br><b>p</b>: Number of parents<br><b>Yi.</b>: Sum of values for parent 'i' in all its crosses<br><b>Yii</b>: Performance of parent 'i' itself<br><b>Y..</b>: Grand total"),
    sca_formula = "ij = Yij - [1/(p+2)] * (Yi. + Yii) - [1/(p+2)] * (Yj. + Yjj) + [2/((p+1)(p+2))] * Y..",
    sca_explanation = "The SCA effect is the performance of the cross (Yij) adjusted for the GCA effects of its parents ('i' and 'j') and the grand mean."
  ),
  griffing_m3 = list(
    model = "Yij =  + gi + gj + sij + rij",
    model_explanation = "Where:  is the overall mean, gi and gj are GCA effects, sij is the SCA effect, and rij is the reciprocal effect. Parents are not included in the analysis.",
    gca_formula = "i = [1/(2(p-2))] * (Yi. + Y.i) - [1/(p(p-2))] * Y..",
    gca_explanation = HTML("The GCA for parent 'i'.<br><b>p</b>: Number of parents<br><b>Yi.</b>: Sum of values for parent 'i' as a female<br><b>Y.i</b>: Sum of values for parent 'i' as a male<br><b>Y..</b>: Grand total. The formula is adjusted because parental data is absent."),
    sca_formula = "ij = (1/2) * (Yij + Yji) - [1/(2(p-2))] * (Yi. + Y.i + Yj. + Y.j) + [1/((p-1)(p-2))] * Y..",
    sca_explanation = "The SCA effect is the average performance of the cross and its reciprocal, adjusted for the GCA effects of both parents and the overall mean, with formulas adapted for the absence of parental data."
  ),
  griffing_m4 = list(
    model = "Yij =  + gi + gj + sij",
    model_explanation = "Where:  is the overall mean, gi and gj are GCA effects, and sij is the SCA effect. Only F1 crosses (no parents or reciprocals) are analyzed.",
    gca_formula = "i = [1/(p-2)] * [Yi. - (2/p) * Y..]",
    gca_explanation = HTML("The GCA for parent 'i'.<br><b>p</b>: Number of parents<br><b>Yi.</b>: Sum of values for parent 'i' in all its crosses<br><b>Y..</b>: Grand total of all crosses."),
    sca_formula = "ij = Yij - [1/(p-2)] * (Yi. + Yj.) + [2/((p-1)(p-2))] * Y..",
    sca_explanation = "The SCA effect is the performance of the cross (Yij) adjusted for the GCA effects of its parents ('i' and 'j') and the grand mean."
  ),
  line_tester = list(
    model = "Yijk =  + rk + gi + gj + sij + eijk",
    model_explanation = "Where:  is the mean, rk is the replication effect, gi is the GCA of the i-th line, gj is the GCA of the j-th tester, sij is the SCA of their cross, and eijk is the error.",
    gca_formula = "GCA (Line i) = Yi.. - Y... | GCA (Tester j) = Y.j. - Y...",
    gca_explanation = HTML("The GCA of a line or tester is its average performance across all its crosses, expressed as a deviation from the grand mean.<br><b>Yi..</b>: Mean of line 'i'<br><b>Y.j.</b>: Mean of tester 'j'<br><b>Y...</b>: Grand mean"),
    sca_formula = "SCA (ij) = Yij. - Yi.. - Y.j. + Y...",
    sca_explanation = "The SCA of a specific cross is its mean performance (Yij.) adjusted for the GCA of the line, the GCA of the tester, and the grand mean."
  ),
  diallel_partial = list(
    model = "Yij =  + gi + gj + sij",
    model_explanation = "Where:  is the overall mean, gi and gj are GCA effects, and sij is the SCA effect.",
    gca_formula = "Effects are estimated via least-squares matrix algebra.",
    gca_explanation = "Due to the unbalanced nature of the partial diallel (not all crosses are made), GCA and SCA effects cannot be calculated with simple summation formulas. Instead, they are estimated simultaneously using a system of linear equations (least-squares method) to find the best fit for the observed data.",
    sca_formula = "Effects are estimated via least-squares matrix algebra.",
    sca_explanation = "Similar to GCA, SCA effects are estimated using the least-squares method. The SCA for a cross represents the deviation of its performance from the value predicted by the GCA of its two parents."
  )
)


# ===================================================================
# HELPER AND ANALYSIS FUNCTIONS
# ===================================================================

#' Create a Show/Hide UI for Explanations
#'
#' A helper function to generate a consistent UI for showing and hiding
#' explanatory text content.
#'
#' @param ns The namespace function from the module server.
#' @param base_id A unique string to identify the UI elements.
#' @param content The HTML content to be shown or hidden.
#' @return A tagList containing the action link and the hidden div.
create_explanation_ui <- function(ns, base_id, content) {
  link_id <- paste0("toggle_", base_id)
  div_id <- paste0("div_", base_id)
  tagList(
    actionLink(ns(link_id), "Show/Hide Explanation", style = "font-size: 12px; font-style: italic;"),
    shinyjs::hidden(
      div(id = ns(div_id),
          class = "alert alert-info",
          style = "margin-top: 10px; font-size: 14px;",
          content
      )
    )
  )
}


add_significance_stars_robust <- function(df) {
  pval_col_name <- dplyr::case_when(
    "Pr(>F)" %in% names(df) ~ "Pr(>F)", "p_value" %in% names(df) ~ "p_value",
    "Pr(>|t|)" %in% names(df) ~ "Pr(>|t|)", TRUE ~ NA_character_
  )
  if (is.na(pval_col_name)) return(df)
  p_values <- df[[pval_col_name]]
  stars <- dplyr::case_when(
    is.na(p_values) ~ "", p_values < 0.001 ~ "***", p_values < 0.01 ~ "**",
    p_values < 0.05 ~ "*", p_values < 0.1 ~ ".", TRUE ~ ""
  )
  df$Signif <- stars
  return(df)
}

griffing_method1 <- function(df, rep_col = "Rep", male_col = "Male", female_col = "Female", trait_col = "Trait", blk_col = NULL) {
  data_ab1 <- df
  data_ab1$Rep    <- as.factor(data_ab1[[rep_col]])
  if (!is.null(blk_col)) data_ab1$Blk <- as.factor(data_ab1[[blk_col]])
  data_ab1$Male   <- as.character(data_ab1[[male_col]])
  data_ab1$Female <- as.character(data_ab1[[female_col]])
  data_ab1$YVAR   <- as.numeric(as.character(data_ab1[[trait_col]]))
  data_ab1 <- data_ab1[!is.na(data_ab1$Male) & !is.na(data_ab1$Female) & !is.na(data_ab1$YVAR), ]
  data_ab1$Male <- factor(data_ab1$Male)
  data_ab1$Female <- factor(data_ab1$Female)
  bc <- nlevels(data_ab1$Rep)
  ptypes <- sort(unique(c(as.character(data_ab1$Male), as.character(data_ab1$Female))))
  p <- length(ptypes)
  means_df <- aggregate(YVAR ~ Male + Female, data = data_ab1, mean)
  means_df <- merge(means_df, expand.grid(Male = ptypes, Female = ptypes), all.y = TRUE)
  myMatrix <- matrix(NA, nrow = p, ncol = p, dimnames = list(ptypes, ptypes))
  for (i in 1:nrow(means_df)) {
    myMatrix[as.character(means_df$Male[i]), as.character(means_df$Female[i])] <- means_df$YVAR[i]
  }
  if (any(is.na(myMatrix))) stop("Missing values in means matrix for Griffing I. Ensure all p^2 crosses are present in the data.")
  modelg1 <- lm(YVAR ~ factor(Rep) + factor(paste(Male, Female, sep="_x_")), data = data_ab1)
  anmodel <- anova(modelg1); rownames(anmodel)[nrow(anmodel)] <- "Residual"
  MSEAD <- as.numeric(anmodel[nrow(anmodel), "Mean Sq"]); error_DF <- as.numeric(anmodel[nrow(anmodel), "Df"])
  Xi.    <- rowSums(myMatrix)
  X.j    <- colSums(myMatrix)
  Xbar   <- sum(myMatrix)
  acon   <- sum((Xi. + X.j)^2) / (2*p)
  ssgca <- acon - (2/(p^2))*(Xbar^2)
  sssca <- sum(myMatrix * (myMatrix + t(myMatrix)))/2 - acon + (Xbar^2)/(p^2)
  ssrecp <- sum((myMatrix - t(myMatrix))^2)/4
  ssmat <- sum((Xi. - X.j)^2)/(2*p)
  ssnomat <- ssrecp - ssmat
  
  Df_display <- c(p-1, p*(p-1)/2, p*(p-1)/2)
  SSS_display <- c(ssgca, sssca, ssrecp) * bc
  MSSS_display <- SSS_display / Df_display
  names(SSS_display) <- names(MSSS_display) <- c("GCA", "SCA", "Reciprocal")
  FVAL_display <- MSSS_display / MSEAD
  pval_display <- 1 - pf(FVAL_display, Df_display, error_DF)
  
  anova_diallel <- data.frame(Df=Df_display, `Sum Sq`=SSS_display, `Mean Sq`=MSSS_display, `F value`=FVAL_display, `Pr(>F)`=pval_display, row.names=names(SSS_display), check.names=FALSE)
  anova_diallel <- add_significance_stars_robust(anova_diallel)
  anova_error <- data.frame(Df=error_DF, `Sum Sq`=MSEAD*error_DF, `Mean Sq`=MSEAD, `F value`=NA, `Pr(>F)`=NA, Signif="", row.names="Residual", check.names=FALSE)
  anova_final <- rbind(anova_diallel, anova_error)
  
  gca <- (Xi. + X.j) / (2*p) - Xbar/(p^2)
  gca_se <- sqrt(((p-1) * MSEAD) / (2*p*p*bc))
  parental_means <- diag(myMatrix)
  gca_df <- data.frame(Parent=ptypes, Parental_Mean = parental_means, GCA=gca, SE=gca_se, T_value = gca/gca_se)
  gca_df$p_value <- 2 * pt(-abs(gca_df$T_value), df = Df_display[1])
  gca_df <- add_significance_stars_robust(gca_df)
  
  sca <- (myMatrix + t(myMatrix))/2 - (matrix(Xi. + X.j, nrow=p, ncol=p, byrow=TRUE) + matrix(Xi. + X.j, nrow=p, ncol=p, byrow=FALSE))/(2*p) + Xbar/(p^2)
  sca[lower.tri(sca)] <- NA
  
  # --- FIX: Define sca_se before using it ---
  sca_se <- sqrt(((p-1) * MSEAD) / (2 * bc))
  
  sca_df <- data.frame(expand.grid(Female=ptypes, Male=ptypes), SCA=as.vector(sca))
  sca_df <- sca_df[!is.na(sca_df$SCA),]
  cross_means_vec <- as.vector(myMatrix)
  sca_df$Cross_Mean <- cross_means_vec[!is.na(as.vector(sca))]
  sca_df$SE <- sca_se
  sca_df$T_value <- sca_df$SCA / sca_df$SE
  sca_df$p_value <- 2 * pt(-abs(sca_df$T_value), df = Df_display[2])
  sca_df <- add_significance_stars_robust(sca_df)
  
  return(list(method="I", anova=anova_final, gca=gca_df, sca=sca_df))
}


griffing_method2 <- function(df, rep_col = "Rep", male_col = "Male", female_col = "Female", trait_col = "Trait", blk_col = NULL) {
  data_ab1 <- df
  data_ab1$Rep    <- as.factor(data_ab1[[rep_col]])
  if (!is.null(blk_col)) data_ab1$Blk <- as.factor(data_ab1[[blk_col]])
  data_ab1$Male   <- as.character(data_ab1[[male_col]])
  data_ab1$Female <- as.character(data_ab1[[female_col]])
  data_ab1$YVAR   <- as.numeric(as.character(data_ab1[[trait_col]]))
  data_ab1 <- data_ab1[!is.na(data_ab1$Male) & !is.na(data_ab1$Female) & !is.na(data_ab1$YVAR), ]
  data_ab1$Male <- factor(data_ab1$Male)
  data_ab1$Female <- factor(data_ab1$Female)
  bc <- nlevels(data_ab1$Rep)
  ptypes <- sort(unique(c(as.character(data_ab1$Male), as.character(data_ab1$Female))))
  p <- length(ptypes)
  data_ab1$Cross <- factor(paste(pmin(data_ab1$Female, data_ab1$Male), pmax(data_ab1$Female, data_ab1$Male), sep = "_x_"))
  means_df <- aggregate(YVAR ~ Male + Female, data = data_ab1, mean)
  means_df <- merge(means_df, expand.grid(Male = ptypes, Female = ptypes), all.y = TRUE)
  myMatrix <- matrix(NA, nrow = p, ncol = p, dimnames = list(ptypes, ptypes))
  for (i in 1:nrow(means_df)) {
    myMatrix[as.character(means_df$Male[i]), as.character(means_df$Female[i])] <- means_df$YVAR[i]
  }
  myMatrix[lower.tri(myMatrix, diag=FALSE)] <- t(myMatrix)[lower.tri(t(myMatrix), diag=FALSE)] # Symmetrize
  if (any(is.na(myMatrix[upper.tri(myMatrix, diag=TRUE)]))) stop("Missing values in means matrix for Griffing II. Ensure p(p+1)/2 crosses are present.")
  modelg <- lm(YVAR ~ Cross + Rep, data = data_ab1)
  anmodel <- anova(modelg)
  rownames(anmodel)[nrow(anmodel)] <- "Residual"
  MSEAD <- as.numeric(anmodel["Residual", "Mean Sq"])
  error_DF <- as.numeric(anmodel["Residual", "Df"])
  Xi. <- rowSums(myMatrix)
  Xbar <- sum(myMatrix[upper.tri(myMatrix, diag=TRUE)])
  acon <- sum((Xi. + diag(myMatrix))^2) / (p + 2)
  ssgca <- acon - (4 * Xbar^2) / (p * (p + 2))
  sssca <- sum((myMatrix[upper.tri(myMatrix, diag=TRUE)])^2) - acon + (2 * Xbar^2) / ((p + 1) * (p + 2))
  Df <- c(p - 1, p * (p - 1) / 2)
  SSS <- c(ssgca, sssca) * bc
  MSSS <- SSS / Df
  FVAL <- MSSS / MSEAD
  pval <- 1 - pf(FVAL, Df, error_DF)
  anova_diallel <- data.frame(Df=Df, `Sum Sq`=SSS, `Mean Sq`=MSSS, `F value`=FVAL, `Pr(>F)`=pval, row.names=c("GCA", "SCA"), check.names=FALSE)
  anova_diallel <- add_significance_stars_robust(anova_diallel)
  anova_error <- data.frame(Df=error_DF, `Sum Sq`=MSEAD*error_DF, `Mean Sq`=MSEAD, `F value`=NA, `Pr(>F)`=NA, Signif="", row.names="Residual", check.names=FALSE)
  anova_final <- rbind(anova_diallel, anova_error)
  gca <- (Xi. + diag(myMatrix) - 2 * Xbar / p) / (p + 2)
  gca_se <- sqrt(((p - 1) * MSEAD) / (p * (p + 2) * bc))
  parental_means <- diag(myMatrix)
  gca_df <- data.frame(Parent=ptypes, Parental_Mean = parental_means, GCA=gca, SE=gca_se, T_value=gca/gca_se)
  gca_df$p_value <- 2 * pt(-abs(gca_df$T_value), df = Df[1])
  gca_df <- add_significance_stars_robust(gca_df)
  sca_mat <- myMatrix - (matrix(Xi.+diag(myMatrix),nrow=p,ncol=p,byrow=TRUE) + matrix(Xi.+diag(myMatrix),nrow=p,ncol=p,byrow=FALSE))/(p+2) + 2*Xbar/((p+1)*(p+2))
  sca_mat[lower.tri(sca_mat)] <- NA
  sca_df <- expand.grid(Male=ptypes, Female=ptypes)
  sca_df$SCA <- as.vector(sca_mat)
  sca_df <- sca_df[!is.na(sca_df$SCA), ]
  cross_means_vec <- as.vector(myMatrix)
  sca_df$Cross_Mean <- cross_means_vec[!is.na(as.vector(sca_mat))]
  sca_se <- sqrt(((p*p + p + 2) * MSEAD) / ((p + 1) * (p + 2) * bc))
  sca_df$SE <- sca_se
  sca_df$T_value <- sca_df$SCA / sca_df$SE
  sca_df$p_value <- 2 * pt(-abs(sca_df$T_value), df = Df[2])
  sca_df <- add_significance_stars_robust(sca_df)
  return(list(method="II", anova=anova_final, gca=gca_df, sca=sca_df, griffing_anova = anova(modelg)))
}

griffing_method3 <- function(df, rep_col = "Rep", male_col = "Male", female_col = "Female", trait_col = "Trait", blk_col = NULL) {
  data_ab1 <- df
  data_ab1$Rep    <- as.factor(data_ab1[[rep_col]])
  if (!is.null(blk_col)) data_ab1$Blk <- as.factor(data_ab1[[blk_col]])
  data_ab1$Male   <- as.character(data_ab1[[male_col]])
  data_ab1$Female <- as.character(data_ab1[[female_col]])
  data_ab1$YVAR   <- as.numeric(as.character(data_ab1[[trait_col]]))
  data_ab1 <- data_ab1[!is.na(data_ab1$Male) & !is.na(data_ab1$Female) & !is.na(data_ab1$YVAR), ]
  data_ab1 <- data_ab1[data_ab1$Male != data_ab1$Female, ] # Method 3 excludes selfs
  data_ab1$Male <- factor(data_ab1$Male)
  data_ab1$Female <- factor(data_ab1$Female)
  bc <- nlevels(data_ab1$Rep)
  ptypes <- sort(unique(c(as.character(data_ab1$Male), as.character(data_ab1$Female))))
  p <- length(ptypes)
  means_df <- aggregate(YVAR ~ Male + Female, data = data_ab1, mean)
  means_df <- merge(means_df, expand.grid(Male = ptypes, Female = ptypes), all.y = TRUE)
  myMatrix <- matrix(NA, nrow = p, ncol = p, dimnames = list(ptypes, ptypes))
  for (i in 1:nrow(means_df)) {
    myMatrix[means_df$Male[i], means_df$Female[i]] <- means_df$YVAR[i]
  }
  diag(myMatrix) <- NA
  if (any(is.na(myMatrix[upper.tri(myMatrix)]) | is.na(myMatrix[lower.tri(myMatrix)]))) {
    stop("Griffing Method III: Incomplete matrix; missing F1 or reciprocal data (excluding selfs).")
  }
  modelg <- lm(YVAR ~ factor(Rep) + factor(paste(Male, Female, sep = "_x_")), data = data_ab1)
  anmodel <- anova(modelg); rownames(anmodel)[nrow(anmodel)] <- "Residual"
  MSEAD <- as.numeric(anmodel[nrow(anmodel), "Mean Sq"]); error_DF <- as.numeric(anmodel[nrow(anmodel), "Df"])
  Xi.    <- rowSums(myMatrix, na.rm = TRUE)
  X.j    <- colSums(myMatrix, na.rm = TRUE)
  Xbar   <- sum(myMatrix, na.rm = TRUE)
  acon <- sum((Xi. + X.j)^2) / (2 * (p - 2))
  ssgca <- acon - (2 / (p * (p - 2))) * (Xbar^2)
  sssca <- sum((myMatrix + t(myMatrix))^2, na.rm=TRUE)/4 - acon + (Xbar^2)/((p-1)*(p-2))
  ssrecp <- sum((myMatrix - t(myMatrix))^2, na.rm=TRUE)/4
  
  Df_display <- c((p - 1), (p * (p - 3) / 2), (p * (p - 1) / 2))
  SSS_display <- c(ssgca, sssca, ssrecp) * bc
  MSSS_display <- SSS_display / Df_display
  names(SSS_display) <- names(MSSS_display) <- c("GCA", "SCA", "Reciprocal")
  FVAL_display <- MSSS_display / MSEAD
  pval_display <- 1 - pf(FVAL_display, Df_display, error_DF)
  
  anova_diallel <- data.frame(Df=Df_display, `Sum Sq`=SSS_display, `Mean Sq`=MSSS_display, `F value`=FVAL_display, `Pr(>F)`=pval_display, row.names=names(SSS_display), check.names=FALSE)
  anova_diallel <- add_significance_stars_robust(anova_diallel)
  anova_error <- data.frame(Df=error_DF, `Sum Sq`=MSEAD*error_DF, `Mean Sq`=MSEAD, `F value`=NA, `Pr(>F)`=NA, Signif="", row.names="Residual", check.names=FALSE)
  anova_final <- rbind(anova_diallel, anova_error)
  
  gca <- (p * (Xi. + X.j) - 2 * Xbar) / (2 * p * (p - 2))
  gca_se <- sqrt(((p - 1) * MSEAD) / (2 * p * (p - 2) * bc))
  gca_df <- data.frame(Parent=ptypes, GCA=gca, SE=gca_se, T_value=gca/gca_se)
  gca_df$p_value <- 2 * pt(-abs(gca_df$T_value), df = Df_display[1])
  gca_df <- add_significance_stars_robust(gca_df)
  
  sca <- (myMatrix + t(myMatrix))/2 - (matrix(Xi.+X.j,nrow=p,ncol=p,byrow=TRUE) + matrix(Xi.+X.j,nrow=p,ncol=p,byrow=FALSE))/(2*(p-2)) + Xbar/((p-1)*(p-2))
  sca[lower.tri(sca, diag = TRUE)] <- NA
  sca_df <- expand.grid(Female=ptypes, Male=ptypes)
  sca_df$SCA <- as.vector(sca)
  sca_df <- sca_df[!is.na(sca_df$SCA), ]
  cross_means_vec <- as.vector(myMatrix)
  sca_df$Cross_Mean <- cross_means_vec[!is.na(as.vector(sca))]
  sca_se <- sqrt(((p - 3) * MSEAD) / (2 * (p - 1) * bc))
  sca_df$SE <- sca_se
  sca_df$T_value <- sca_df$SCA / sca_df$SE
  sca_df$p_value <- 2 * pt(-abs(sca_df$T_value), df = Df_display[2])
  sca_df <- add_significance_stars_robust(sca_df)
  
  return(list(method="III", anova=anova_final, gca=gca_df, sca=sca_df))
}


griffing_method4 <- function(df, rep_col, male_col, female_col, trait_col, blk_col = NULL) {
  dat <- df[, c(rep_col, male_col, female_col, trait_col)]
  names(dat) <- c("Rep", "Male", "Female", "YVAR")
  dat$YVAR <- as.numeric(as.character(dat$YVAR))
  dat <- dat[!is.na(dat$Male) & !is.na(dat$Female) & !is.na(dat$YVAR), ]
  dat <- dat[dat$Male != dat$Female, ] # Method 4 excludes selfs
  dat$Rep <- factor(dat$Rep)
  dat$Cross <- factor(paste(pmin(dat$Female, dat$Male), pmax(dat$Female, dat$Male), sep = "_x_"))
  bc <- nlevels(dat$Rep)
  ptypes <- sort(unique(c(dat$Male, dat$Female)))
  p <- length(ptypes)
  means_df <- aggregate(YVAR ~ Male + Female, data = dat, mean)
  myMatrix <- matrix(NA, nrow = p, ncol = p, dimnames = list(ptypes, ptypes))
  for (i in 1:nrow(means_df)) {
    myMatrix[as.character(means_df$Male[i]), as.character(means_df$Female[i])] <- means_df$YVAR[i]
  }
  myMatrix[lower.tri(myMatrix)] <- t(myMatrix)[lower.tri(t(myMatrix))]
  if (any(is.na(myMatrix[upper.tri(myMatrix)]))) {
    stop("Incomplete data: One or more cross combinations are missing for Method 4.")
  }
  model_g4 <- lm(YVAR ~ Cross + Rep, data = dat)
  anova_g4 <- anova(model_g4)
  MSEAD <- anova_g4["Residuals", "Mean Sq"]
  Randoms_Df <- anova_g4["Residuals", "Df"]
  diag(myMatrix) <- NA
  Xi. <- rowSums(myMatrix, na.rm = TRUE)
  Xbar <- sum(myMatrix, na.rm = TRUE) / 2
  ssgca <- (1/(p-2)) * sum(Xi.^2) - (4*Xbar^2)/(p*(p-2))
  sssca <- sum(myMatrix[upper.tri(myMatrix)]^2) - (1/(p-2))*sum(Xi.^2) + (2*Xbar^2)/((p-1)*(p-2))
  Df <- c(p - 1, p * (p - 3) / 2)
  SSS <- c(ssgca, sssca) * bc
  MSSS <- SSS / Df
  FVAL <- MSSS / MSEAD
  pval <- 1 - pf(FVAL, Df, Randoms_Df)
  anova_diallel <- data.frame(Df=Df, `Sum Sq`=SSS, `Mean Sq`=MSSS, `F value`=FVAL, `Pr(>F)`=pval, row.names=c("GCA", "SCA"), check.names=FALSE)
  anova_diallel <- add_significance_stars_robust(anova_diallel)
  anova_error <- data.frame(Df=Randoms_Df, `Sum Sq`=MSEAD*Randoms_Df, `Mean Sq`=MSEAD, `F value`=NA, `Pr(>F)`=NA, Signif="", row.names="Residual", check.names=FALSE)
  anova_final <- rbind(anova_diallel, anova_error)
  gcaeff <- (1/(p-2)) * (Xi. - (2 * Xbar / p))
  gca_se <- sqrt(((p-1)*MSEAD)/(bc*p*(p-2)))
  gca_tab <- data.frame(Parent=ptypes, GCA=gcaeff, SE=gca_se)
  gca_tab$T_value <- gca_tab$GCA / gca_tab$SE
  gca_tab$p_value <- 2 * pt(-abs(gca_tab$T_value), df = Df[1])
  gca_tab <- add_significance_stars_robust(gca_tab)
  mu_hat <- (2*Xbar)/(p*(p-1))
  gca_m1 <- matrix(gcaeff, nrow=p, ncol=p, byrow=TRUE)
  gca_m2 <- matrix(gcaeff, nrow=p, ncol=p, byrow=FALSE)
  scaeffmat <- myMatrix - gca_m1 - gca_m2 - mu_hat
  scaeffmat[lower.tri(scaeffmat, diag=TRUE)] <- NA
  sca_tab <- data.frame(expand.grid(Female=ptypes, Male=ptypes), SCA=as.vector(scaeffmat))
  sca_tab <- sca_tab[!is.na(sca_tab$SCA), ]
  cross_means_vec <- as.vector(myMatrix)
  sca_tab$Cross_Mean <- cross_means_vec[!is.na(as.vector(scaeffmat))]
  sca_tab$SE <- sqrt(((p-3)*MSEAD)/(bc*(p-1)))
  sca_tab$T_value <- sca_tab$SCA / sca_tab$SE
  sca_tab$p_value <- 2 * pt(-abs(sca_tab$T_value), df = Df[2])
  sca_tab <- add_significance_stars_robust(sca_tab)
  return(list(method="IV", anova=anova_final, gca=gca_tab, sca=sca_tab))
}

diallel_partial_manual <- function(df, trait, p1, p2, rep) {
  Y <- df[[trait]]
  P1 <- as.character(df[[p1]])
  P2 <- as.character(df[[p2]])
  Rep <- as.factor(df[[rep]])
  mask <- !is.na(Y)
  Y <- Y[mask]; P1 <- P1[mask]; P2 <- P2[mask]; Rep <- Rep[mask]
  parents <- sort(unique(c(P1, P2)))
  n_par <- length(parents)
  mu <- mean(Y, na.rm = TRUE)
  n_obs <- length(Y)
  s_tab <- table(c(P1, P2))
  s_vals <- as.numeric(s_tab)
  s <- mean(s_vals)
  is_balanced <- all(s_vals == s)
  if (!is_balanced) warning(
    paste0("[Partial Diallel] Unbalanced design detected: crosses per parent are not equal (s values: ",
           paste(unique(s_vals), collapse = ","), "). Results use average s = ", s, "."))
  cross_ids <- paste(pmin(P1, P2), pmax(P1, P2), sep = ":")
  reps_per_cross <- as.numeric(median(table(cross_ids)))
  if (any(table(cross_ids) != reps_per_cross)) {
    warning("[Partial Diallel] Unbalanced replication detected across crosses! Using modal # of reps.")
  }
  Xg <- matrix(0, nrow = n_obs, ncol = n_par)
  for (i in 1:n_obs) {
    Xg[i, which(parents == P1[i])] <- 1
    Xg[i, which(parents == P2[i])] <- 1
  }
  Xg_red <- Xg[, -n_par, drop = FALSE]
  Y_c <- Y - mu
  gca_hat_red <- solve(t(Xg_red) %*% Xg_red, t(Xg_red) %*% Y_c)
  gca_hat <- c(gca_hat_red, -sum(gca_hat_red))
  names(gca_hat) <- parents
  cross_names_unique <- unique(cross_ids)
  sca_cross <- numeric(length(cross_names_unique))
  names(sca_cross) <- cross_names_unique
  for (cr_name in cross_names_unique) {
    rows <- which(cross_ids == cr_name)
    ptemp <- unlist(strsplit(cr_name, ":"))
    expected <- mu + gca_hat[ptemp[1]] + gca_hat[ptemp[2]]
    sca_cross[cr_name] <- mean(Y[rows]) - expected
  }
  sca_fitted_for_each_obs <- sca_cross[cross_ids]
  rep_means <- tapply(Y, Rep, mean)
  rep_effects <- rep_means - mu
  rep_effect_for_each_obs <- rep_effects[as.character(Rep)]
  fitted_values <- mu + rep_effect_for_each_obs + gca_hat[P1] + gca_hat[P2] + sca_fitted_for_each_obs
  residuals <- Y - fitted_values
  ss_rep <- sum(table(Rep) * (rep_means - mu)^2)
  df_rep <- length(unique(Rep)) - 1
  ss_gca <- 2 * s * sum(gca_hat^2)
  df_gca <- n_par - 1
  ss_sca <- sum(sca_cross^2) * reps_per_cross
  df_sca <- length(unique(cross_ids)) - n_par
  ss_err <- sum(residuals^2)
  df_err <- n_obs - (df_rep + df_gca + df_sca + 1)
  ss_total <- sum((Y - mu)^2)
  df_total <- n_obs - 1
  ms_rep <- ss_rep / df_rep
  ms_gca <- ss_gca / df_gca
  ms_sca <- ss_sca / df_sca
  ms_err <- ss_err / df_err
  f_gca <- ms_gca / ms_err
  f_sca <- ms_sca / ms_err
  f_rep <- ms_rep / ms_err
  p_gca <- pf(f_gca, df_gca, df_err, lower.tail = FALSE)
  p_sca <- pf(f_sca, df_sca, df_err, lower.tail = FALSE)
  p_rep <- pf(f_rep, df_rep, df_err, lower.tail = FALSE)
  stars_func <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else if (p < 0.1) "." else ""
  }
  anova_tab <- data.frame(
    Source = c("Replication", "GCA", "SCA", "Error", "Total"),
    Df = c(df_rep, df_gca, df_sca, df_err, df_total),
    `Sum Sq` = c(ss_rep, ss_gca, ss_sca, ss_err, ss_total),
    `Mean Sq` = c(ms_rep, ms_gca, ms_sca, ms_err, NA),
    `F value` = c(f_rep, f_gca, f_sca, NA, NA),
    `Pr(>F)` = c(p_rep, p_gca, p_sca, NA, NA),
    Signif = sapply(c(p_rep, p_gca, p_sca, NA, NA), stars_func),
    check.names = FALSE
  )
  r <- reps_per_cross
  n <- n_par
  sigma2_sca <- (ms_sca - ms_err) / r
  sigma2_gca <- ((ms_gca - ms_sca) * (n - 1)) / (r * s * (n - 2))
  var_tab <- data.frame(
    Variance_Component = c("GCA", "SCA"),
    Value = c(sigma2_gca, sigma2_sca)
  )
  gca_tab <- data.frame(Parent = parents, GCA = gca_hat)
  sca_tab <- data.frame(Cross = names(sca_cross), SCA = sca_cross)
  list( anova = anova_tab, gca = gca_tab, sca = sca_tab, var = var_tab)
}

line_tester_manual <- function(data, line_col, tester_col, rep_col, trait_col, type_col) {
  get_stars <- function(p_values) {
    dplyr::case_when(
      is.na(p_values)   ~ "", p_values < 0.001  ~ "***", p_values < 0.01   ~ "**",
      p_values < 0.05   ~ "*", p_values < 0.1    ~ ".", TRUE              ~ ""
    )
  }
  tryCatch({
    df <- data.frame(
      Rep    = as.factor(data[[rep_col]]), Line   = as.factor(data[[line_col]]),
      Tester = as.factor(data[[tester_col]]), Type   = as.factor(data[[type_col]]),
      Y      = as.numeric(data[[trait_col]])
    )
    df <- na.omit(df)
    if (!any(df$Type == "cross")) {
      stop("The 'Type' column must contain entries with the exact value 'cross'.")
    }
    parents <- df[df$Type != "cross", ]
    crosses <- df[df$Type == "cross", ]
    crosses$Line   <- droplevels(crosses$Line)
    crosses$Tester <- droplevels(crosses$Tester)
    has_parents <- nrow(parents) > 0
    l <- nlevels(crosses$Line)
    t <- nlevels(crosses$Tester)
    if (l < 2 || t < 1) {
      stop("Analysis requires at least two lines and one tester in the cross data.")
    }
    df$TreatmentID <- interaction(df$Line, df$Tester, df$Type, drop = TRUE)
    model_overall <- aov(Y ~ Rep + TreatmentID, data = df)
    anova_overall <- anova(model_overall)
    MS_Error <- anova_overall["Residuals", "Mean Sq"]
    DF_Error <- anova_overall["Residuals", "Df"]
    model_crosses <- aov(Y ~ Line * Tester, data = crosses)
    anova_crosses <- anova(model_crosses)
    DF_Parents <- 0; SS_Parents <- 0
    if (has_parents && nlevels(droplevels(parents$Line)) > 1) {
      parents$Parent <- droplevels(parents$Line)
      model_parents <- aov(Y ~ Parent, data = parents)
      anova_parents <- anova(model_parents)
      DF_Parents <- anova_parents["Parent", "Df"]
      SS_Parents <- anova_parents["Parent", "Sum Sq"]
    }
    DF_PvC <- 0; SS_PvC <- 0
    if (has_parents) {
      df$PvC <- factor(ifelse(df$Type == "cross", "Cross", "Parent"), levels = c("Parent", "Cross"))
      model_pvc <- aov(Y ~ PvC, data = df)
      anova_pvc <- anova(model_pvc)
      DF_PvC <- anova_pvc["PvC", "Df"]
      SS_PvC <- anova_pvc["PvC", "Sum Sq"]
    }
    ss_crosses_total <- sum(anova_crosses[c("Line", "Tester", "Line:Tester"), "Sum Sq"])
    df_crosses_total <- sum(anova_crosses[c("Line", "Tester", "Line:Tester"), "Df"])
    source_names <- c("Replications", "Treatments", "  Parents", "  Parents vs. Crosses", "  Crosses",
                      "    Lines", "    Testers", "    Lines X Testers", "Error", "Total")
    DF <- c(anova_overall["Rep", "Df"], anova_overall["TreatmentID", "Df"], DF_Parents, DF_PvC,
            df_crosses_total, anova_crosses["Line", "Df"], anova_crosses["Tester", "Df"],
            anova_crosses["Line:Tester", "Df"], DF_Error, sum(anova_overall[, "Df"], na.rm = TRUE))
    SS <- c(anova_overall["Rep", "Sum Sq"], anova_overall["TreatmentID", "Sum Sq"], SS_Parents, SS_PvC,
            ss_crosses_total, anova_crosses["Line", "Sum Sq"], anova_crosses["Tester", "Sum Sq"],
            anova_crosses["Line:Tester", "Sum Sq"], anova_overall["Residuals", "Sum Sq"],
            sum(anova_overall[, "Sum Sq"], na.rm = TRUE))
    MS <- ifelse(DF > 0, SS / DF, 0)
    F_value <- MS / MS_Error
    P_value <- pf(F_value, DF, DF_Error, lower.tail = FALSE)
    anova_final <- data.frame(Source = source_names, Df = DF, `Sum Sq` = SS, `Mean Sq` = MS,
                              `F value` = F_value, `Pr(>F)` = P_value, check.names = FALSE)
    anova_final[anova_final$Source == "Error", "Mean Sq"] <- MS_Error
    rows_to_blank <- c("Total")
    cols_to_blank <- c("Mean Sq", "F value", "Pr(>F)")
    anova_final[anova_final$Source %in% rows_to_blank, cols_to_blank] <- NA
    anova_final$Signif <- get_stars(anova_final$`Pr(>F)`)
    numeric_cols <- c("Sum Sq", "Mean Sq", "F value", "Pr(>F)")
    anova_final[numeric_cols] <- lapply(anova_final[numeric_cols], function(x) sprintf("%.2f", x))
    anova_final[is.na(anova_final) | anova_final == "NA"] <- ""
    grand_mean <- mean(crosses$Y)
    
    parental_means_lines <- aggregate(Y ~ Line, data = parents, FUN = mean)
    names(parental_means_lines)[2] <- "Parental_Mean"
    
    parental_means_testers <- aggregate(Y ~ Tester, data = parents, FUN = mean)
    names(parental_means_testers)[2] <- "Parental_Mean"
    
    emm_lines <- emmeans(model_crosses, ~ Line)
    summary_lines <- as.data.frame(summary(emm_lines))
    gca_lines_out <- data.frame(Line = summary_lines$Line, GCA = summary_lines$emmean - grand_mean, SE = summary_lines$SE)
    gca_lines_out <- merge(gca_lines_out, parental_means_lines, by = "Line", all.x = TRUE)
    gca_lines_out$`t value` <- gca_lines_out$GCA / gca_lines_out$SE
    gca_lines_out$`Pr(>|t|)` <- 2 * pt(-abs(gca_lines_out$`t value`), df = DF_Error)
    gca_lines_out$Signif <- get_stars(gca_lines_out$`Pr(>|t|)`)
    
    emm_testers <- emmeans(model_crosses, ~ Tester)
    summary_testers <- as.data.frame(summary(emm_testers))
    gca_testers_out <- data.frame(Tester = summary_testers$Tester, GCA = summary_testers$emmean - grand_mean, SE = summary_testers$SE)
    gca_testers_out <- merge(gca_testers_out, parental_means_testers, by = "Tester", all.x = TRUE)
    gca_testers_out$`t value` <- gca_testers_out$GCA / gca_testers_out$SE
    gca_testers_out$`Pr(>|t|)` <- 2 * pt(-abs(gca_testers_out$`t value`), df = DF_Error)
    gca_testers_out$Signif <- get_stars(gca_testers_out$`Pr(>|t|)`)
    
    cross_means <- aggregate(Y ~ Line + Tester, data = crosses, FUN = mean)
    names(cross_means)[3] <- "Cross_Mean"
    
    emm_sca <- emmeans(model_crosses, ~ Line:Tester)
    summary_sca <- as.data.frame(summary(emm_sca))
    summary_sca <- merge(summary_sca, gca_lines_out[, c("Line", "GCA")], by = "Line")
    names(summary_sca)[names(summary_sca) == "GCA"] <- "GCA_Line"
    summary_sca <- merge(summary_sca, gca_testers_out[, c("Tester", "GCA")], by = "Tester")
    names(summary_sca)[names(summary_sca) == "GCA"] <- "GCA_Tester"
    summary_sca$SCA <- summary_sca$emmean - summary_sca$GCA_Line - summary_sca$GCA_Tester - grand_mean
    sca_out <- data.frame(Line = summary_sca$Line, Tester = summary_sca$Tester, SCA = summary_sca$SCA, SE = summary_sca$SE)
    sca_out <- merge(sca_out, cross_means, by = c("Line", "Tester"))
    sca_out$`t value` <- sca_out$SCA / sca_out$SE
    sca_out$`Pr(>|t|)` <- 2 * pt(-abs(sca_out$`t value`), df = DF_Error)
    sca_out$Signif <- get_stars(sca_out$`Pr(>|t|)`)
    list(
      anova_full  = anova_final, gca_lines   = gca_lines_out,
      gca_testers = gca_testers_out, sca         = sca_out
    )
  }, error = function(e) {
    list(error = paste("Line x Tester analysis failed:", e$message))
  })
}

run_mating_ui <- function(df, input, design) {
  tryCatch({
    if (design == "diallel_partial") {
      req_cols <- c(input$md_trait, input$md_parent1, input$md_parent2, input$md_rep)
      if (any(!req_cols %in% names(df))) stop(paste("Missing column(s):", paste(req_cols[!req_cols %in% names(df)], collapse = ", ")))
      res <- diallel_partial_manual(df, input$md_trait, input$md_parent1, input$md_parent2, input$md_rep)
    } else if (design == "line_tester") {
      req_cols <- c(input$md_line, input$md_tester, input$md_rep, input$md_trait, input$md_type)
      if (any(!req_cols %in% names(df))) stop(paste("Missing column(s):", paste(req_cols[!req_cols %in% names(df)], collapse = ", ")))
      res <- line_tester_manual(df, input$md_line, input$md_tester, input$md_rep, input$md_trait, input$md_type)
    } else {
      blk <- NULL
      req_cols <- c(input$md_trait, input$md_parent1, input$md_parent2, input$md_rep)
      if (any(!req_cols %in% names(df))) stop(paste("Missing column(s):", paste(req_cols[!req_cols %in% names(df)], collapse = ", ")))
      if (design == "griffing_m1") {
        res <- griffing_method1(df, rep_col=input$md_rep, male_col=input$md_parent1, female_col=input$md_parent2, trait_col=input$md_trait, blk_col=blk)
      } else if (design == "griffing_m2") {
        df$Parent1_std <- pmin(as.character(df[[input$md_parent1]]), as.character(df[[input$md_parent2]]))
        df$Parent2_std <- pmax(as.character(df[[input$md_parent1]]), as.character(df[[input$md_parent2]]))
        res <- griffing_method2(df, rep_col=input$md_rep, male_col="Parent1_std", female_col="Parent2_std", trait_col=input$md_trait, blk_col=blk)
      } else if (design == "griffing_m3") {
        res <- griffing_method3(df, rep_col=input$md_rep, male_col=input$md_parent1, female_col=input$md_parent2, trait_col=input$md_trait, blk_col=blk)
      } else if (design == "griffing_m4") {
        res <- griffing_method4(df, rep_col=input$md_rep, male_col=input$md_parent1, female_col=input$md_parent2, trait_col=input$md_trait, blk_col=blk)
      } else {
        stop("Selected mating design is not recognized.")
      }
    }
    return(res)
  }, error = function(e) {
    return(list(error = e$message))
  })
}

# ===================================================================
# MODULE SERVER FUNCTION
# ===================================================================
mating_design_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    md_results <- reactiveVal(NULL)
    
    md_file_data <- reactive({ shared_data$file_data })
    active_mating_design <- reactive({ shared_data$mating_design })
    
    output$md_dynamic_columns <- renderUI({
      df <- md_file_data()
      design <- active_mating_design()
      req(df, design)
      all_cols <- names(df)
      num_cols <- all_cols[sapply(df, is.numeric)]
      switch(design,
             griffing_m1 = , griffing_m2 = , griffing_m3 = , griffing_m4 = ,
             diallel_partial = tagList(
               selectInput(ns("md_parent1"), "Parent 1 Column", choices = all_cols),
               selectInput(ns("md_parent2"), "Parent 2 Column", choices = all_cols),
               selectInput(ns("md_rep"), "Replication Column", choices = all_cols),
               selectInput(ns("md_trait"), "Trait", choices = num_cols)
             ),
             line_tester = tagList(
               selectInput(ns("md_line"), "Line Column", choices = all_cols),
               selectInput(ns("md_tester"), "Tester Column", choices = all_cols),
               selectInput(ns("md_rep"), "Replication Column", choices = all_cols),
               selectInput(ns("md_type"), "Type Column", choices = all_cols, selected = "type"),
               selectInput(ns("md_trait"), "Trait", choices = num_cols)
             ),
             h5("Select a mating design on the Home tab.")
      )
    })
    
    observeEvent(input$md_run_analysis, {
      req(md_file_data(), active_mating_design())
      md_results(NULL)
      showNotification("Running analysis...", type = "message", duration = 3)
      res <- run_mating_ui(md_file_data(), input, active_mating_design())
      if (!is.null(res$error)) {
        showModal(modalDialog(title = "Analysis Error", res$error))
      } else {
        md_results(res)
        showNotification("Analysis Complete!", type = "message", duration = 5)
      }
    })
    
    # --- Observers to handle Show/Hide clicks ---
    observeEvent(input$toggle_anova_model, { shinyjs::toggle("div_anova_model", anim = TRUE) })
    observeEvent(input$toggle_gca_formula, { shinyjs::toggle("div_gca_formula", anim = TRUE) })
    observeEvent(input$toggle_sca_formula, { shinyjs::toggle("div_sca_formula", anim = TRUE) })
    
    
    output$md_anova <- renderTable({
      req(md_results())
      res <- md_results()
      if (!is.null(res$anova_full)) res$anova_full else if (!is.null(res$anova)) res$anova else NULL
    }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$md_griffing2_anova <- renderTable({
      req(md_results())
      res <- md_results()
      if (!is.null(res$griffing_anova) && isTRUE(res$method == "II")) res$griffing_anova else NULL
    }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$md_gca <- renderTable({
      req(md_results())
      res <- md_results()
      if (active_mating_design() != "line_tester" && !is.null(res$gca)) res$gca else NULL
    }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$md_gca_lines <- renderTable({
      req(md_results())
      if (!is.null(md_results()$gca_lines)) md_results()$gca_lines else NULL
    }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$md_gca_testers <- renderTable({
      req(md_results())
      if (!is.null(md_results()$gca_testers)) md_results()$gca_testers else NULL
    }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$md_sca <- renderTable({
      req(md_results())
      if (!is.null(md_results()$sca)) md_results()$sca else NULL
    }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$md_variance_components <- renderTable({
      req(md_results())
      res <- md_results()
      if (!is.null(res$var)) res$var else if (!is.null(res$variance_components)) res$variance_components else NULL
    }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$md_interpretations <- renderUI({
      req(md_results())
      res <- md_results()
      design <- active_mating_design()
      
      if (!is.null(res$error)) {
        return(tags$p("Interpretations could not be generated due to an analysis error."))
      }
      
      generate_interpretations <- function(results, design_type) {
        interpretations <- list()
        
        if (design_type == "line_tester") {
          req(results$anova_full, results$gca_lines, results$sca)
          anova <- results$anova_full
          source_col <- anova$Source
          anova$Signif <- NULL; anova$Source <- NULL
          anova <- data.frame(lapply(anova, function(x) as.numeric(as.character(x))))
          anova$Source <- source_col
          p_lines   <- anova$`Pr..F.`[trimws(anova$Source) == "Lines"]
          p_testers <- anova$`Pr..F.`[trimws(anova$Source) == "Testers"]
          p_lxt     <- anova$`Pr..F.`[trimws(anova$Source) == "Lines X Testers"]
          
          if (!is.na(p_lines) && p_lines < 0.05) { interpretations <- append(interpretations, list(tags$li(tags$b("Significant Line Effects:"), " Real GCA differences exist among lines."))) }
          else { interpretations <- append(interpretations, list(tags$li(tags$b("Non-Significant Line Effects:"), " No significant GCA differences among lines."))) }
          if (!is.na(p_testers) && p_testers < 0.05) { interpretations <- append(interpretations, list(tags$li(tags$b("Significant Tester Effects:"), " Real GCA differences exist among testers."))) }
          if (!is.na(p_lxt) && p_lxt < 0.05) { interpretations <- append(interpretations, list(tags$li(tags$b("Significant SCA Effects:"), " Significant SCA effects indicate that certain specific crosses differ significantly from expectations based on GCA alone, suggesting the importance of specific parental interactions."))) }
          else { interpretations <- append(interpretations, list(tags$li(tags$b("Non-Significant SCA Effects:"), " Cross performance is predictable by parent GCA; additive action is likely more important."))) }
          
          best_line <- results$gca_lines %>% filter(GCA == max(GCA))
          interpretations <- append(interpretations, list(tags$li(tags$b("Best General Combiner (Line):"), paste0("'", best_line$Line, "' (GCA: ", sprintf("%.2f", best_line$GCA), ") is the best parent for general improvement."))))
          best_cross <- results$sca %>% filter(SCA == max(SCA))
          interpretations <- append(interpretations, list(tags$li(tags$b("Best Specific Combination:"), paste0("'", best_cross$Line, " x ", best_cross$Tester, "' (SCA: ", sprintf("%.2f", best_cross$SCA), ") shows outstanding performance."))))
          
        } else {
          req(results$anova, results$gca, results$sca)
          anova <- results$anova
          # The ANOVA table from diallel_partial_manual already has a "Source" column.
          # We must ensure we don't duplicate it.
          if (!("Source" %in% names(anova))) {
            anova <- anova %>% tibble::rownames_to_column("Source")
          }
          p_gca <- anova$`Pr(>F)`[anova$Source == "GCA"]
          p_sca <- anova$`Pr(>F)`[anova$Source == "SCA"]
          
          if (!is.na(p_gca) && p_gca < 0.05) { interpretations <- append(interpretations, list(tags$li(tags$b("Significant GCA Effects:"), " Indicates significant additive genetic variance. Parent performance is a good predictor of progeny performance."))) }
          else { interpretations <- append(interpretations, list(tags$li(tags$b("Non-Significant GCA Effects:"), " Additive effects are not significant. Parent performance is not a reliable predictor of progeny performance."))) }
          if (!is.na(p_sca) && p_sca < 0.05) { interpretations <- append(interpretations, list(tags$li(tags$b("Significant SCA Effects:"), " Indicates significant non-additive (dominance/epistasis) variance. Certain specific crosses perform unexpectedly well or poorly."))) }
          else { interpretations <- append(interpretations, list(tags$li(tags$b("Non-Significant SCA Effects:"), " Non-additive effects are not significant. The performance of crosses can be well-predicted by the GCA of their parents."))) }
          
          best_gca_parent <- results$gca %>% filter(GCA == max(GCA))
          interpretations <- append(interpretations, list(tags$li(tags$b("Best General Combiner:"), paste0("The parent '", best_gca_parent$Parent, "' (GCA: ", sprintf("%.3f", best_gca_parent$GCA), ") is the best general combiner for this trait."))))
          best_sca_cross <- results$sca %>% filter(SCA == max(SCA))
          interpretations <- append(interpretations, list(tags$li(tags$b("Best Specific Combination:"), paste0("The cross '", best_sca_cross$Female, " x ", best_sca_cross$Male, "' (SCA: ", sprintf("%.3f", best_sca_cross$SCA), ") shows the best specific combining ability."))))
        }
        
        return(tags$ul(style = "font-size: 15px; line-height: 1.6;", interpretations))
      }
      
      div(style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px;",
          generate_interpretations(res, design))
    })
    
    output$anova_output_ui <- renderUI({
      req(md_results(), active_mating_design())
      design <- active_mating_design()
      eq <- mating_design_equations[[design]]
      tagList(
        tags$h4("Model & Formula", style = "color: #142850;"),
        tags$p(tags$b("Model: "), tags$code(eq$model)),
        create_explanation_ui(ns, "anova_model", eq$model_explanation),
        hr(),
        tags$h4("ANOVA Table", style = "color: #142850;"), tableOutput(ns("md_anova")), br(),
        tags$h4("Summary of Results", style = "color: #142850; margin-top: 15px;"), uiOutput(ns("md_interpretations")),
        if (active_mating_design() == "griffing_m2") {
          tagList(br(), tags$h4("Classical ANOVA", style = "color: #142850;"), tableOutput(ns("md_griffing2_anova")))
        }
      )
    })
    
    output$gca_output_ui <- renderUI({
      req(md_results(), active_mating_design())
      design <- active_mating_design()
      eq <- mating_design_equations[[design]]
      tagList(
        tags$h4("GCA Formula", style = "color: #142850;"),
        tags$p(tags$b("Formula: "), tags$code(eq$gca_formula)),
        create_explanation_ui(ns, "gca_formula", eq$gca_explanation),
        hr(),
        if (active_mating_design() == "line_tester") {
          tagList(h4("Line GCA Effects", style = "color: #142850;"), tableOutput(ns("md_gca_lines")),
                  h4("Tester GCA Effects", style = "color: #142850;"), tableOutput(ns("md_gca_testers")))
        } else {
          tagList(h4("GCA Effects", style = "color: #142850;"), tableOutput(ns("md_gca")))
        }
      )
    })
    
    output$sca_output_ui <- renderUI({
      req(md_results(), active_mating_design())
      design <- active_mating_design()
      eq <- mating_design_equations[[design]]
      tagList(
        tags$h4("SCA Formula", style = "color: #142850;"),
        tags$p(tags$b("Formula: "), tags$code(eq$sca_formula)),
        create_explanation_ui(ns, "sca_formula", eq$sca_explanation),
        hr(),
        h4("SCA Effects", style = "color: #142850;"),
        tableOutput(ns("md_sca"))
      )
    })
    
    output$md_download_results <- downloadHandler(
      filename = function() { paste0("MatingDesign_Results_", active_mating_design(), "_", Sys.Date(), ".zip") },
      content = function(file) {
        req(md_results())
        res <- md_results()
        temp_dir <- tempdir()
        files_to_zip <- c()
        
        save_table <- function(table, name) {
          if (!is.null(table)) {
            path <- file.path(temp_dir, paste0(name, ".csv"))
            write.csv(table, path, row.names = (name == "ANOVA_Table"))
            files_to_zip <<- c(files_to_zip, path)
          }
        }
        
        save_table(res$anova, "ANOVA_Table")
        save_table(res$anova_full, "ANOVA_Table_LineTester")
        save_table(res$griffing_anova, "Griffing_Method2_Classical_ANOVA")
        save_table(res$gca, "GCA_Effects")
        save_table(res$gca_lines, "GCA_Lines")
        save_table(res$gca_testers, "GCA_Testers")
        save_table(res$sca, "SCA_Effects")
        save_table(res$var, "Variance_Components")
        save_table(res$variance_components, "Variance_Components_NC3")
        
        utils::zip(zipfile = file, files = files_to_zip, extras = "-j")
      },
      contentType = "application/zip"
    )
  })
}