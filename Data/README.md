# Metadata

An overview of the data included in this repository.

## TSP files

These files contain the behavioural data collected from the traveling salesperson decision task. 

TSP_clean and TSP_clean_time contain the data of each trial by participant (Each row is one instance solved by the participant). Compared to TSP_clean, TSP_clean_time excludes some participants (see manuscript for time-on-task data analysis details).

Each row is one instance solved by the participant.

- id : Instance generation ID.
- case_id : Instance generation sub ID.
- instanceNumber: Instance Number.
- pID: Partcipant number.
- block: Sequential block number.
- trial: Trial number.
- answer: Response by the participant (0=no, 1=yes, 2=No answer)
- correct: Was the answer correct?
- timeSpent: time-on-task before moving to the response stage
- randomYes.1.Left.No.Right.Yes.: Button lateraility (1 = Left.No and Right.Yes.)
- error: Was there a software error when performing this instance?
- ratio: Related to nClauses/NLiterals, but the exact number is wrong. Consider as classes of instances sharing the same ratio.
- phaseT: TCC (1= High TCC, 0=Low TCC).
- region: Constrainedness region.
- sol: Satisfiability (1= Satisfiable, 0= Unsatisfiable).
- no_solutions: Number of solution witnesses.
- solution: sol.
- cx: X coordinates of the cities presented in the instances.
- cy: Y coordinates of the cities presented in the instances.
- distances: Matrix of distances between cities.
- nCities: Number of cities.
- param: Constrainedness (alpha) parameter.
- type: Type of instance (`1`= High TCC , `2`= High TCC , `3`= High TCC, `4`=High TCC, `5`=Overconstrained, `6`=Underconstrained)
- max_distance: Distance constraint (maximum distance that can be travelled).
- finaldistance: Final distance travelled by the participant before submission of response.
- itemsSelected: Path (cities) currently selected.
- runtime: Instance solver output.
- solvetime: Instance solver output.
- variables: Instance solver output.
- propagators: Instance solver output.
- propagations: Instance solver output.
- nodes: Instance solver output.
- failures: Instance solver output.
- restarts: Instance solver output.
- peak_depth: Instance solver output.

TSP_clicks contains the data of all the clicks performed by each participant for each trial.

- itynumber.100.Reset.: City selected in this click (100 is reset is clicking the reset button that removes all current selections)
- In.1..Out.0..Reset.3.: Click type = Adding a city (1), removing a city (2) or reset (3)
- time: Time of click.

## 3SAT files

These files contain the behavioural data collected from the boolean satisfiability task. 

SAT_clean and SAT_clean_time contain the data of each trial by participant (Each row is one instance solved by the participant). Compared to SAT_clean, SAT_clean_time excludes some participants (see manuscript for time-on-task data analysis details).

- id : Instance generation ID.
- instanceNumber: Instance Number.
- pID: Partcipant number.
- block: Sequential block number.
- trial: Trial number.
- answer: Response by the participant (0=no, 1=yes, 2=No answer)
- correct: Was the answer correct?
- timeSpent: time-on-task before moving to the response stage
- randomYes.1.Left.No.Right.Yes.: Button lateraility (1 = Left.No and Right.Yes.)
- error: Was there a software error when performing this instance?
- v: Instance variable number assignment to each literal.
- l: Negation (or not) for each literal.
- sol: Satisfiability (1= Satisfiable, 0= Unsatisfiable).
- type: Type of instance (`1`= High TCC , `2`= High TCC , `3`= High TCC, `4`=High TCC, `5`=Overconstrained, `6`=Underconstrained).
- nVariables: Number of variables in the instance.
- nLiterals: Number of literals in the instance.
- nClauses: Number of clauses in the instance. 
- ratio: Related to nClauses/NLiterals, but the exact number is wrong. Consider as classes of instances sharing the same ratio.
- phaseT: TCC (1= High TCC, 0=Low TCC).
- region: Constrainedness region.
- solution: sol
- solvetime: Instance solver output
- restarts: Instance solver output
- conflicts: Instance solver output
- decisions: Instance solver output
- propagations: Instance solver output
- clauses: nClauses
- literals: nLiterals


Sat_clicks contains the data of all the clicks performed by each participant for each trial.
- clicknumber: Click number per trial.
- Variable: Variable clicked.
- Literal: literal of the variable clicked (negation or not).
- State: Literal set to TRUE after click?
- time: Time of click.

## KS file

These files contain the behavioural data collected from the knapsack decision task (origianlly reported in https://osf.io/t2jv7/). Each row is one instance solved by the participant.

- id : Instance generation ID.
- v: Item values.
- w: Item weigths.
- p: Target profit.
- c: Capacity cosnraint
- pID: Partcipant number.
- block: Sequential block number.
- trial: Trial number.
- answer: Response by the participant (0=no, 1=yes, 2=No answer)
- correct: Was the answer correct?
- timeSpent: NN
- randomYes.1.Left.No.Right.Yes.: Button lateraility (1 = Left.No and Right.Yes.)
- xyCoordinates: Item coordinates.
- error: Was there a software error when performing this instance?
- sol: Satisfiability (1= Satisfiable, 0= Unsatisfiable).
- phaseT: TCC (1= High TCC, 0=Low TCC).
- ExpVersion: Experiment parameters version.
- propagations: Number of minizinc (Gecode) propagations.
- decisions: Number of minizinc (Gecode) decisions.
- totalValues: Sum of values.
- totalWeights: Sum of weights.
- nCapacity: c/totalWeigths.
- nProfit: p/totalValues.
- nSolutions: Number of solution witnesses.
- opt_profit: Maximum value attainable that satisfy the capacity constraints.
- dist_opt_p: Absolute value of the diference between opt_profit and c.
- n_dist_opt_p: dist_opt_p/totalValues (=IC)

## Instance properties files

These files contain the description of each of the instances administered in the TSP and 3SAT tasks.

### TSP

- instanceNumber: Instance Number.
- nSOlutions: Number of solution witnesses.
- opt_distance: Optimal (minimum) path distance.
- opt_tour: Path with optimal (minimum) path distance.
- cx: X coordinates of the cities presented in the instances.
- cy: Y coordinates of the cities presented in the instances.
- distances: Matrix of distances between cities.
- id = Instance generation ID.
- type: Type of instance (`1`= High TCC , `2`= High TCC , `3`= High TCC, `4`=High TCC, `5`=Overconstrained, `6`=Underconstrained)
- sol: Satisfiability (1= Satisfiable, 0= Unsatisfiable)
- max_distance: Distance constraint (maximum distance that can be travelled)
- nCities: Number of cities.
- param: Constrainedness (alpha) parameter.
- dist_from_opt: Absolut value of the difference between max_distance and opt_tour.

### SAT

- instanceNumber: Instance Number.
- v: Instance variable number assignment to each literal.
- l: Negation (or not) for each literal.
- id: Instance generation ID.
- type: Type of instance (`1`= High TCC , `2`= High TCC , `3`= High TCC, `4`=High TCC, `5`=Overconstrained, `6`=Underconstrained).
- sol: Satisfiability (1= Satisfiable, 0= Unsatisfiable).
- nVariables: Number of variables in the instance.
- nLiterals: Number of literals in the instance.
- nClauses: Number of clauses in the instance. 
- ratio: Related to nClauses/NLiterals, but the exact number is wrong. Consider as classes of instances sharing the same ratio.
- min_model: One feasible variable allocation that maximises the clauses evaluating to true (corresponds to mincost).
- mincost: Minimum number of clauses that missing from all clauses evaluating to true.
- models: All possible vairable allocations.
- costs: The corresponding number of clauses that do not evaluate to TRUE.
- nSOlutions: Number of solution witnesses.
