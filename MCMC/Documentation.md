# Code documentation 
The following file contains additional info about what I've been able to figure out so far about the code written by Kim. 

**NOTE**: so far I've tried cleaning up some of the code, starting from the functions used to create the trees, however it could probably still be written in a better way. I've tried mantaining a balance between readability and variable names that are too long (_yes, I'm looking at you_ `x_partition.proposed_splitting_variable`...). 

Also, **BEWARE**: the original code does something **REEEEALLY BAD**: it uses global variables mixed with other stuff...
i.e.  `Xpred.list`, which apparently is never used in the `Sim*` files, magically appears in `GROW.R`, `PRUNE.R` and `CHANGE.R`. HOWEVER, other variables such as `ind`, which also appears in those files, are TOTALLY DIFFERENT things! (It's a big mess) 

## Indexes 
The code uses various vectors of indexes to point to data and predictors(/covariates). Thus, the main vector of data points and predictors _should_ be always the same, indexes are used like pointers.

## Variable names and use
- `ind` in the main simulation files should refer to the selected covariates like we have seen with $\underline{\gamma}$ ($\gamma_i \in \{0, 1\}$) for GLMMs. **Watch out!** This `ind` is different from `ind` used in functions such as GROW, PRUNE and CHANGE. I've not been able to find a use for the `ind` parameter in those cases, it seems like it could be used for some kind of debug, but it's always set to 2, therefore I've removed it. 
- `Obs_list` is a list of vectors of indexes pointing to observations ($y_i$'s) for each one of the trees. At each MCMC iteration, the indexes are ordered based on the partitions induced by the splitting rules for each tree.   
- `dt_list` is a list containing the _current_ (?) decision tree structures.  
- `Tree` is a matrix containing the results of each iteration of the MCMC on the trees.
- `Tree11` ????? It some kind of matrix, but I'm not sure what it is used for, perhaps it has something to do with predictions. 
- `Xcut` should be the partition of the parameter space. 
- `Xpred.list` is a list form of the vector of covariates. 
- `Xpred.mult` ???. 
- `*.prob` or `prob.*` refers to some probaility (_kinda obvious tbf_).
- `prop.*` refers to some sort of proposal/proposed value. (i.e. `prop.prob` is the proposal probability for predictors). 
- `post.*` or `*.post` refers to some kind of posterior. 


### Other relevant infos 
#### Decision trees representation 
Elements in `dt_list` are associated with the following properties: 
- `Terminal`: 1 or 0, 1 if the node is a leaf. I've changed this to TRUE and FALSE for readability reasons. 
- `Split`: index of the predictor used as splitting variable in the splitting rule, `NA` otherwise. 
- `Value`: value used as splitting constant in the splitting rule, `NA` otherwise.
- `MU`: $\mu_{ij}$ value associated with a leaf, `NA` otherwise. 
- `position`: position of the node in the tree structure. Positions are expressed in a linear form: the root has position 1, given a node $N$ with position `pos`, its children can be found at positions $`2 * pos` (left child) and `2  * pos + 1` (right child). 
- `parent`: position of the parent node (`parent.pos = pos %% 2`). 
- `begin`: index pointing to the beginning of the range of observations in the partition defined by the node in `Obs_list`. 
- `end`: index pointing to the end of the range of observations in the partition defined by the node in `Obs_list`.

#### Naming "conventions"
Kim seems to denote with all CAPS some matrices: i.e. `tau2` is a vector of values, `TAU2` is a matrix which, at each iteration, stores the new values of `tau2` (same with `rho` and `RHO`). 
**BEWARE**: this notation is not at all consistent... Sometimes there are parameters like `sigma2` which are scalars, in this case `sigma2` is stored in a vector called `Sigma2`. 



