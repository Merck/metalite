# metalite <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/metalite)](https://CRAN.R-project.org/package=metalite)
[![Codecov test coverage](https://codecov.io/gh/Merck/metalite/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Merck/metalite?branch=main)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/metalite)](https://cran.r-project.org/package=metalite)
[![R-CMD-check](https://github.com/Merck/metalite/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Merck/metalite/actions/workflows/R-CMD-check.yaml)
[![status](https://tinyverse.netlify.com/badge/metalite)](https://tinyverse.netlify.app/)
<!-- badges: end -->

## Overview

The purpose of the metalite R package is to define a unified data structure to save metadata information
for analysis & reporting (A&R) based on ADaM datasets in clinical trial development.

The R package is designed to

- standardize function input for analysis and reporting.
- separate logic from data dependency.
- enables pipes (`|>`).
- reduce manual steps to maintain SDLC documentation.
- ensure consistency between analysis specification, mock, and results.

We build the metalite R package to enable principals below:

- Automation: a function call is better than a checklist.
- Single-entry: enter in one place, sync to all deliveries.
  - e.g., Enter data source one time for all AE analysis.
- End-to-end: cover all steps in software development lifecycle from define to delivery.

## Use cases

The metalite R package build a foundation to simplify tool development.
For example, the metalite can be used to:

- standardize input and output for A&R functions.
- create analysis and reporting planning grid
- create mock table
- create and validate A&R results
- trace analysis records

Note: metalite is a low level R package 
that needs to work with other R packages to complete the work.
We illustrate the idea in the diagram below.

<img src="man/figures/design-diagram.png" width="100%" />
