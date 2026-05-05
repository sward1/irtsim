# irtsim: Monte Carlo Simulation-Based Sample-Size Planning for Item Response Theory

Provides a pipeline application programming interface (API) for Monte
Carlo simulation-based sample-size planning in item response theory
(IRT). Implements the 10-decision framework from Schroeders and Gnambs
(2025)
[doi:10.1177/25152459251314798](https://doi.org/10.1177/25152459251314798)
as a three-step workflow: specify the data-generating model with
irt_design(), add study conditions with irt_study(), and run simulations
with irt_simulate(). Supports one-parameter logistic (1PL),
two-parameter logistic (2PL), and graded response models with
missing-completely-at-random (MCAR), missing-at-random (MAR), booklet,
and linking missingness mechanisms. Results include mean squared error
(MSE), bias, root mean squared error (RMSE), standard error (SE), and
coverage criteria with summary and plot methods.

## See also

Useful links:

- <https://github.com/sward1/irtsim>

- Report bugs at <https://github.com/sward1/irtsim/issues>

## Author

**Maintainer**: Stephen Ward <stephen_ward+irtsim@abhome.co>
