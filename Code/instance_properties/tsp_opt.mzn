%
% TSP with circuit predicate in Minizinc.
%
% Model created by Krzysztof Kuchcinski
%

include "circuit.mzn";

% Data

int: n; % number of nodes

array[1..n] of var 1..n: x;             % the circuit
array[1..n, 1..n] of int: distances;  % distance matrix
array[1..n] of var int: d;
var int: distance;
int: path_length;

%solve :: int_search(x, first_fail, indomain_min, complete) minimize distance;
solve :: int_search(d, max_regret, indomain_min, complete) minimize distance;
%solve :: int_search(d, max_regret, indomain_min, complete) satisfy;

constraint

    circuit(x)
    /\ 
    forall(i in 1..n) (
    let {
         array[1..n] of int: l = [ distances[i,j] | j in 1..n ]
	}
	in
	 l[x[i]] = d[i]
    )
    /\ 
   distance = sum(i in 1..n) (d[i])
%    /\
%    distance <= path_length
;


output [
       "x: ", show(x),"\n",
       show(distance)
]