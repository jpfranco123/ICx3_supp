# Data Metadata

An overview of the data included in this repository.

## TSP files

These files contain the behavioural data collected from the traveling salesperson decision task. 


## 3SAT files

These files contain the behavioural data collected from the boolean satisfiability task. 


## KS file

These files contain the behavioural data collected from the knapsack decision task (origianlly reported in https://osf.io/t2jv7/).


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
