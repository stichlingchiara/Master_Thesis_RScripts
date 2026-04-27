# Soil Organic Carbon Analysis in Mangrove Ecosystems

This repository contains the R scripts used for data processing, analysis, and visualization for the Master's thesis:

**"Soil Organic Carbon in Mangrove Ecosystems: Influence of Site Conditions, Stand Age, and Methodological Choices"**

## Project Description

This study investigates soil organic carbon (SOC) dynamics in planted mangrove forests within the Malizia Mangrove Park in Davao Oriental, Philippines. The analysis focuses on spatial variability, stand age effects, and the sensitivity of SOC estimates to methodological choices such as bulk density calculation and compaction correction.

## Repository Structure

- `scripts/`  
  Contains all R scripts used in the analysis:
  - `01_Import_data.R` – Data cleaning and preparation  
  - `02_DBD.R` – Analysis and comparison of dry bulk density 
  - `03_Comparing_Levels.R` – Statistical and graphic analysis of the three levels
  - `04_Compaction.R` – Statistical and graphic analysis of the influence of compaction on DBD and SOC values
  - `05_.Conversion_Factor.R` – Statistical and graphic analysis of the influence of the conversion factor on SOC values
  - `06_Comparing_Locations.R` – Statistical and graphic analysis of the different locations
  - `07_Comparing_Stations.R` – Statistical and graphic analysis of the different stations, focussing on mangrove age
  - `08_Stock_Calculations.R` – Calculation of carbon stocks and extrapolation to get comparable results

## Methods Overview

The analysis was conducted using R and includes:
- Calculation of SOC density and carbon stocks  
- Comparison of different bulk density approaches  
- Statistical analysis (e.g., mixed models, non-parametric tests)  
- Visualization of results  

## Reproducibility

The scripts are organized to allow step-by-step reproduction of the analysis.  
Users should run the scripts in numerical order.

Required R packages include:
-`tidyverse`
- `dplyr`
- `stats`
- `ggplot2`  
- `lme4`  
- `emmeans`

## Author

Chiara Stichling  
Master’s Thesis, University of Copenhagen
Year: 2026

## Contact

For questions regarding the analysis, please contact:  
nds218@alumni.ku.dk
