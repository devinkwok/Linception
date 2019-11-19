Linception
==========

A meta-model for parameter selection on linear models.
Concept and code by Devin Kwok.
In fulfillment of Stat 429 group project, fall 2019, University of Calgary.
Project partner Kevin Wong.

Requirements
------------
* r-base-dev for `packrat`
* CRAN package `packrat` for managing package dependencies
* CRAN package `car` for VIF and power transform
* CRAN package `igraph` for graph data structure

How to use
----------
1. Open an interactive R prompt in the project root directory. `packrat` should
boostrap install itself. Run `packrat::restore()` to automatically install
the other required libraries.
2. Add datasets into `datasets/`. Each dataset should be a `.csv` file readable
by the R function `read.table()`. Choose one column as the response variable and
prefix the column name with `RESPONSE_VARIABLEx`.
Any column names prefixed with `"NULL_PREDICTORx"` are ignored.
Currently, categorical variables are not supported.
Example:
```
"RESPONSE_VARIABLExDriversKilled" "drivers" "front" "rear" "kms" "PetrolPrice" "NULL_PREDICTORxlaw"
"1" 107 1687 867 269 9059 0.102971811805368 12 0
"2" 97 1508 825 265 7685 0.102362995884646 6 0
```
3. From the project root directory, run `Rscript src/main.R`. This file includes
all necessary code to run the steps below.
4. To generate meta-data from assorted datasets, use the function `generate_meta_data()`
 from `data.R`. By default `main.R` outputs `.csv` files containing linear models and their statistics
to `outputs/`. Every paired statistic is of the form `superset - subset` or `superset/subset`.
 Warning: depending on the size of input datasets, this may take a long time!
5. To use a trained meta-model to do forward step predictor selection, use the function
`fit_model_to_predictors()` from `model.R`. This returns a `lm` object with optimized predictors.
6. To boostrap the meta-model by letting it select its own predictors, use the function
`bootstrap_meta_model()` from `model.R`. Warning: this may take a long time!


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

Furthermore, the linear model is able to choose its own predictors recursively.
By initializing with any linear model which is approximately able to estimate
the slope of the minimization surface, successive application of the meta-model
to its own parameter selection may allow the meta-model to converge on a more
accurate estimation of the minimization slope.

Methodology
-----------
* Find datasets with sufficient size and other properties
* Train linear models on many parameter subsets using this data
* Calculate statistics from these linear models
* Train linear meta-model on statistics
* Apply meta-model to choose parameters using a greedy algorithm
* Verify effectiveness of meta-model versus hand-trained linear models
    other datasets, etc.
* Boostrap meta-model by using it to choose its own predictors.

Abstract and Presentation
-------------------------

Abstract [https://docs.google.com/document/d/1Z5Tzzr__lr5IFiIcDqXRFTFaGCUm-CiBkm4Evh75xks/edit?usp=sharing](https://docs.google.com/document/d/1Z5Tzzr__lr5IFiIcDqXRFTFaGCUm-CiBkm4Evh75xks/edit?usp=sharing)

Presentation slides [https://docs.google.com/presentation/d/1Mqsdy-56XWalV6d4YQwB7B3xC9AzN107M9Qvw39v4Qw/edit?usp=sharing](https://docs.google.com/presentation/d/1Mqsdy-56XWalV6d4YQwB7B3xC9AzN107M9Qvw39v4Qw/edit?usp=sharing)

