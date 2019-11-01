Linception
==========

A meta-model for parameter selection on linear models.
Concept and code by Devin Kwok.
In fulfillment of Stat 429 group project, fall 2019, University of Calgary.
Project partner Kevin Wong.

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


