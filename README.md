# Thesis Code Repository

This repository contains the R scripts from my thesis research on smart travel survey analysis, examining how device and user characteristics (such as personal traits, motivation, and ability) influence engagement in editing and improving missing data. The analysis employs a combination of a hurdle model with GLMM and GLMM trees.

## Files and Descriptions

### 1. `variable_prep.R`
This script is responsible for all the variable calculations used in the models. It prepares and processes the necessary data before model fitting.

### 2. `percentage_change.R`
This file calculates the outcome variable, which is essential for the subsequent modeling. The outcome variable is a key part of the analysis for both research questions. It is calculated as the difference in percentage of missing data between the raw and the edited dataset.

### 3. `Hurdle_model.R`
This script contains the hurdle model used to analyze how device and user characteristics influence the level of engagement in editing and improving missing data within the smart travel survey. It builds a binomial GLMM for the zero part of the model and a LMM for the non-zero part of the model, using the variables calculated in `variable_prep.R` and `percentage_change.R`.

### 4. `GLMM_tree_02.R`
This script fits the GLMM trees to identify subgroups based on available features, examining whether these subgroups exhibit similar patterns of engagement in editing and improving missing data.

### 5. `useful_plots.R`
This file contains the code to generate various plots used for vizualizations in the thesis. 

## Datasets
The datasets used in this analysis are securely stored by CBS and are accessible exclusively to CBS employees.

## How to Run
1. Ensure that you have all required packages installed (e.g., `lme4`, `GLMMadaptive`, `partykit`).
2. Run the scripts in the following order:
   - `variable_prep.R`
   - `percentage_change.R`
   - `Hurdle_model.R` (for research question 1)
   - `GLMM_tree_02.R` (for research question 2)
   - `useful_plots.R` (to generate the plots used in the thesis)

