README
================

- [Requirements](#requirements)

\#Airbnb Deal Classification Project

\##Overview

This project aims to classify Airbnb listings in New York City as Good,
Neutral, or Bad Deals using a combination of Exploratory data analysis
(EDA), feature engineering, decision tree modeling, and a rule-based
logic system. Additionally, a Shiny dashboard is provided for real-time
deal prediction and visual exploration.

------------------------------------------------------------------------

\##Project Structure

------------------------------------------------------------------------

\##Exploratory Data Analysis (EDA)

Script: \[`Exploratory_Data_Analysis.R`\]

Key insights were derived through visual and statistical exploration
of: - Numerical distributions (price, footage, reviews, etc.) - Boxplots
stratified by deal category - Correlation matrix of engineered
features - Borough-level summaries - Scatter plots of price vs. features
colored by deal

Tools used: `ggplot2`, `dplyr`, `corrplot`

------------------------------------------------------------------------

\##Model Building and Prediction Pipeline

Script: \[`PredictionModel.R`\]

\###Steps: 1. Feature Engineering - Raw metrics (e.g., `price_per_foot`,
`log_price`) - Relative metrics (vs. borough averages) - Categorical
binning (`price_category`, `review_category`, etc.)

2.  Model Development
    - tree1: Based on numeric and engineered continuous variables
    - tree2: Based on categorized/binned features
    - Rule-based classifier: Custom logic for deal classification
    - combined_tree: Final ensemble decision tree
3.  Cross-Validation
    - Two-fold validation with accuracy reporting
4.  Final Prediction
    - Trained models applied to test data
    - Results saved to `result_airbnb.csv`

------------------------------------------------------------------------

\##Shiny Dashboard

Script: \[`app.R`\]

\###Features: - Enter listing attributes to predict deal type - Display
top 3 most similar listings - Interactive scatter plot (Review
vs. Footage) colored by predicted deal

To run locally:

shiny::runApp(“app.R”)

\##Outputs -result_airbnb.csv: Final classification predictions

-.rds files: Serialized models, borough averages, and quantiles

-price_vs_review.png: Visual output from EDA

## Requirements

Ensure the following R packages are installed:

install.packages(c( “shiny”, \# Shiny app “plotly”, \# Interactive plots
in dashboard “rpart”, \# Decision tree models “ggplot2”, \#
Visualizations for EDA “dplyr”, \# Data manipulation “corrplot”, \#
Correlation matrix visualization “tidyr”, \# (Optional) Data reshaping
“reshape2” \# (Optional) Used for melting data frames ))
