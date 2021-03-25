# Computation hardness and human problem-solving across tasks (ICx3)

This repository contains the supplementary data and data analysis related to the open science framework (OSF) project: https://osf.io/tekqa/

## Data

The behavioural data collected in the set of experiments in which we adminsitered one of three tasks: traveling salesperson problem (TSP), Boolean sastisfiability (3SAT) and the knapsack decision problem (KP). 

The data presented here has been collated across participants and cleaned according to the description in the manuscript (eee data preprocessing script). 

A description of the preprocessed data is presented in the data folder in the `metadata` file.

## Data Analysis

This includes the following components:

- The code (R) used to generate all the statistical analyses reported in the manuscript. This is composed of two `results*.Rmd` files: (a) one includes the analysis for the 3SAT task and the TSP while the other (b) contains the analysis done for the knapsack task. Both scripts are presented in R-markdown format and can be run with the data provided in this repository.

- An accompanying report for each data analysis script with the output of the statistical analysis. This report also includes a description of the software (R) and package versions used to run the analyses.

- A data preprocessing script, with an accompanying report (note that the input to this script is not provided here).
