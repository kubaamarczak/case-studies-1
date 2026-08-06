# Fallstudien I — Case Studies
 
Repository of case studies for the module *Fallstudien I* (Faculty of Statistics,
TU Dortmund).
Group project.
 
## About the Module
 
Over the course of the semester, five topics are assigned roughly every three
weeks. For each topic, a real dataset is provided together with an open-ended
list of guiding questions covering data cleaning, exploratory analysis, an
appropriate statistical or machine-learning method, and interpretation of the
results. Each topic is worked through end-to-end, from raw data to a written
report.
 
## Topics
 
| # | Topic | Focus | Data | Deadline |
|---|---|---|---|---|
| 1 | Education and employment in German counties | Descriptive statistics, outlier handling, correlation analysis (East vs. West Germany) | Zensus 2022 (Destatis), 400 German counties | 29 Apr 2026 |
| 2 | [Climate differences in the Ruhr area](project-2) | Distribution comparison, hypothesis testing (Welch's t-test, Kruskal-Wallis), regression, multiple testing | ECA&D station data, 6 weather stations in NRW | 20 May 2026 |
| 3 | Concrete compressive strength | Multiple linear regression, model selection, multicollinearity (VIF), influential observations | UCI Concrete Compressive Strength dataset, n = 1030 | 10 Jun 2026 |
| 4 | Early detection of diabetes risk | Logistic regression, complete separation, classification rule, Hosmer-Lemeshow test | Pima Indians Diabetes dataset (Kaggle), n = 768 | 28 Jun 2026 |
| 5 | [Classifying everyday mobility choices](project-5) | Multinomial logistic regression vs. XGBoost, class imbalance / upsampling | ESS CRONOS-3 Wave 6 panel survey, n = 7330 | 15 Jul 2026 |
 
This repository showcases Topics 2 and 5 in full, with code, plots, and
results in their respective folders. Topics 1, 3, and 4 are summarized here
only for documentation purposes, so the module overview is complete — no code
or report for these is included in this repository.
 
## Repository Structure
 
Each topic lives in its own `project-#/` folder with:
 
- a topic-specific `README.md` (research question, approach, key results, and
  reproducibility instructions),
- numbered analysis scripts (e.g. `01_data_cleaning.R`,
  `02_eda.R`, …), run in order,
- a `plots/` folder with exported figures referenced in the README and report,
- a `data/` folder for raw and cleaned data.

The full written report (PDF) for each topic is not part of this repository.
 
## Tools
 
R (tidyverse, ggplot2, caret) — plus method-specific packages per topic, such
as `nnet`, `car`, `MASS`, `detectseparation`, and `xgboost` for classification,
and `sf`, `geodata`, `osmdata`, `ggrepel` for spatial visualization.