<!-- badges: start -->
[![R-CMD-check](https://github.com/abhijithkpgen/PBAT/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/abhijithkpgen/PBAT/actions/workflows/R-CMD-check.yaml?query=branch%3Amain)
<!-- badges: end -->

<p align="center">
  <img src="https://raw.githubusercontent.com/abhijithkpgen/PBAT/main/man/figures/LogoNobg.png" alt="PbAT Logo" width="200"/>
</p>

# PbAT: Plant Breeding Analytical Tools

**PbAT** is a Shiny-based R package that provides an **end-to-end pipeline** for statistical analysis in plant breeding experiments. The application is also hosted online and is freely accessible at: **[https://pbat.online](https://pbat.online)**

---

## Key Features

-   **Experimental Design Analysis (EDA)**
    -   Summary statistics, ANOVA, and broad-sense heritability (H²).
    -   Calculates BLUEs and BLUPs (for combined or individual locations).
    -   Includes diagnostic checks and post-hoc tests.
    -   Generates publication-ready visualizations (boxplots, QQ plots, interaction plots).

-   **Multivariate Analysis**
    -   Performs PCA.
    -   Conducts trait correlation and path analysis.
    -   Can be run in standalone mode or linked directly to EDA results.

-   **Mating Design Analysis**
    -   Analyzes common mating designs, including Diallel (Griffing Methods I–IV, Partial Diallel) and Line × Tester.
    -   Calculates GCA/SCA effects, generates ANOVA tables, and estimates variance components.
    -   
-   **Stability Analysis**
    -   Analyzes additive main effects and multiplicative interaction (AMMI) and
    -    genotype plus genotype by environment interaction effect (GGE) Biplots

-   **Downloads**
    -   Export all results as ZIP archives containing CSV tables and high-quality PDF plots.

---

## Application Workflow

The workflow is designed to be intuitive and straightforward.

#### 1. Upload Your Data
Upload your data in CSV format. The file should include columns for genotype, location, replication, and traits. You can refer to the example datasets in the "Help & Guide" section for the required format.

<img src="https://raw.githubusercontent.com/abhijithkpgen/PBAT/main/man/figures/Loading_data.jpg" alt="Loading data" width="700">

#### 2. Select an Analysis
Choose the type of analysis you want to perform based on your experimental design and objectives.

#### 3. Map Data Columns
Assign the appropriate columns from your dataset (e.g., genotype, location, block) when prompted to ensure the analysis runs correctly.


#### 4. Run the Analysis
Execute the analysis to generate tables, summaries, and visualizations.

<img src="https://raw.githubusercontent.com/abhijithkpgen/PBAT/main/man/figures/Descriptive_analysis.jpg" alt="Descriptive analysis" width="700">

#### 5. Review & Download Results
All results—including interactive tables, plots, and summaries—can be downloaded as a single ZIP archive containing publication-ready files.

---

## 📂 Sample Data

PbAT includes sample CSV templates for each analysis type. These are available in the `inst/app/www/` directory of the package and are also downloadable from the **“Help & Guide”** tab within the app itself.

---

## 💻 Installation & Usage

You can install the R package version of PbAT directly from GitHub using `devtools`.

```r
# Install devtools if it's not already installed
if (!require("devtools")) install.packages("devtools")

# Install PbAT from GitHub
devtools::install_github("abhijithkpgen/PbAT")

# Load the library
library(PbAT)

# Run the application
run_app()
