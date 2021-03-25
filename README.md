# Computation hardness and human problem-solving across tasks (ICx3)

This repository contains the supplementary data and data analysis related to the open science framework (OSF) project: https://osf.io/tekqa/

## Data

The behavioural data collected in the set of experiments in which we adminsitered one of three tasks: traveling salesperson problem (TSP), Boolean sastisfiability (3SAT) and the knapsack decision problem (KP). 

The data presented here has been collated across participants and cleaned according to the description in the manuscript (eee data preprocessing script). 

A description of the preprocessed data is presented in the data folder in the `Data/README.md` file.

## Data Analysis

This includes the following components:

- The code (R) used to generate all the statistical analyses reported in the manuscript. This is composed of two `results*.Rmd` files: (a) one includes the analysis for the 3SAT task and the TSP while the other (b) contains the analysis done for the knapsack task. Both scripts are presented in R-markdown format and can be run with the data provided in this repository.

- An accompanying report for each data analysis script with the output of the statistical analysis. This report also includes a description of the software (R) and package versions used to run the analyses.

- A data preprocessing script, with an accompanying report (note that the input to this script is not provided here).

## Experimental design overview

An overview of the tasks invoilved in this study.

### TSP

These files contain the behavioural data collected from the traveling salesperson decision task. This task is based on the traveling salesperson problem (TSP). Given a set of N cities displayed on a rectangular map on the screen and a limit L on path length, the decision problem is to answer the question where there exists}= a path connecting all N cities with a distance at most L.

In the TSP task, each participant completed 72 trials (3 blocks of 24 trials with a rest period of 30 seconds between blocks). Each trial presented a different instance of TSP. Trials were self-paced with a time limit of 40 seconds. Participants could use the mouse to trace routes by clicking on the dots indicating the different cities. The length of the chosen route was indicated at the top of the screen (together with the maximum route length of the instance). When participants were ready to submit their answer, they pressed a button to advance from the screen displaying the cities to the response screen where they responded YES or NO. The time limit to respond was 3 seconds, and the inter-trial interval was 3 seconds as well. The order of instances and the sides of the YES/NO button on the response screen were randomized for each participant.

### 3SAT

These files contain the behavioural data collected from the boolean satisfiability task. This task is based on the 3-satisfiability problem (3SAT). In this problem, the aim is to determine whether a boolean formula is satisfiable. In other words, given a propositional formula, the aim is to determine whether there exists at least one configuration of the variables (which can take values TRUE or FALSE) such that the formula evaluates to TRUE. The propositional formula in 3SAT has a specific structure. Specifically, the formula is composed of a conjunction of clauses that must all evaluate TRUE for the whole formula to evaluate TRUE. Each of these clauses, takes the form of an OR logical operator of three literals (variables and their negations).

In order to represent this in an accessible way to participants we developed a task composed of switches and light bulbs (Fig~\ref{fig_tasks}a). Participants were presented with a set of light bulbs (clauses), each of which had three switches underneath (literals) that were represented by a positive or negative number. The number on each switch represented the variable number, which could be turned on or off (TRUE or FALSE). The aim of the task is to determine whether there exists a way of turning on and off variables such that all the light bulbs are turned on (that is, the formula evaluates TRUE).

In the SAT task, each participant completed 64 trials (4 blocks of 16 trials with a rest period of 60 seconds between blocks). Each trial presented a different instance of SAT. At the beginning of each trial, participants were presented with a different instance of the 3SAT problem. A bar in the top-right corner of the screen indicated the time remaining in the trial. Each participant completed 64 trials (4 blocks of 16 trials with a rest period of 60 seconds between blocks). Trials were self-paced with a time limit of 110 seconds. Participants could use the mouse to click on any of the variables to select their value (blue=TRUE,orange=FALSE). A light bulb above each clause indicated whether a clause evaluated to TRUE (light on) given the selected values of the variables underneath it. The number of clicks in each trial was limited to 20. The purpose of this limit was to discourage participants from using a trial-and-error strategy to solve the instances. When participants were ready to submit their solution, they pressed a button to advance from the screen displaying the instance to the response screen where they responded YES or NO. The time limit to respond was 3 seconds, and the inter-trial interval was 3 seconds as well. The order of instances and the side of the YES/NO button on the response screen were randomized for each participant.

### KS

The knapsack task is based on the 0-1 knapsack problem (KP). An instance of this problem consists of a set of items I with weights {w_1,...,w_N} and values {v_1,...,v_N}, and two positive numbers c and p denoting the capacity and profit constraint (of the knapsack). The problem is to decide whether there exists a set of items such that the added weight of the knapsack is less than or equal to the capacity constraint; and the value of the knapsack is greater than or equal to the profit constraint.

All instances had 6 items and weights, values, capacities and target profits were all integers. In the task each participant completed 72 trials (3 blocks of 24 trials with a rest period of 60s between blocks). Each trial presented a different instance of the KP. Trials had a time limit of 25 seconds and were not self-paced. A green circle at the center of the screen indicated the time remaining in each stage of the trial. During the first 3 seconds participants were presented with a set of items of different values and weights. Then, both capacity constraint and target profit were shown at the center of the screen (22 seconds). No interactivity was incorporated into the task; that is, participants could not click on items. When the time limit was reached, participants were presented with the response screen where they responded YES or NO. The time limit to respond was 2 seconds, and the inter-trial interval was 5 seconds. The order of instances and the sides of the YES/NO button on the response screen were randomized for each participant.

