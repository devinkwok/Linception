Linception
==========

A meta-model for parameter selection on linear models.
Concept and code by Devin Kwok.
In fulfillment of Stat 429 group project, fall 2019, University of Calgary.
Project partner Kevin Wong.

Requirements
------------
* CRAN package `car`
* CRAN package `igraph`

How to use
----------
1. Add datasets into `datasets/`. Each dataset should be a `.csv` file readable
by the R function `read.table()`. Choose one column as the response variable and
prefix the column name with `RESPONSE_VARIABLEx`.
Any column names prefixed with `"NULL_PREDICTORx"` are ignored.
Currently, categorical variables are not supported.
Example:
```
"!response-variable!DriversKilled" "drivers" "front" "rear" "kms" "PetrolPrice" NULL NULL
"1" 107 1687 867 269 9059 0.102971811805368 12 0
"2" 97 1508 825 265 7685 0.102362995884646 6 0

2. Run `generate_data()` in `data.R`. This will output a `.csv` file containing linear models and their statistics to `outputs/`.
```


Overview
--------
The goal of this project is to minimize the test error of a linear model
through parameter selection.

Consider all possible subsets of parameters
as a graph, with edges representing the addition or removal of single
parameters. As parameter selection is an optimization
problem, the minimization surface is not linear. However, adjacent
models in the graph may be locally linearly related to test error.
Therefore I hypothesize that a linear meta-model may be
trained to predict the slope of this change. Such a meta-model can then
be used as part of a greedy optimization algorithm, whereby the graph is
traversed to its minimum by stepwise transitions chosen by the meta-model.

The linear meta-model is trained to predict relative change in test error
of a pair of linear models which are adjacent on the graph of all parameter
subsets. The predictors can be any statistics calculated from the linear model
(such as number of parameters, coefficient of determination, etc.) as well as
pairwise statistics (such as analysis of variance significance,
ratio of AIC, etc.).

Methodology
-----------
* Find datasets with sufficient size and other properties
* Train linear models on many parameter subsets using this data
* Calculate statistics from these linear models
* Train linear meta-model on statistics
* Apply meta-model to choose parameters using a greedy algorithm
* Verify effectiveness of meta-model versus hand-trained linear models
    other datasets, etc.


