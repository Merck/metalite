# metalite <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/Merck/metalite/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Merck/metalite/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Merck/metalite/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Merck/metalite?branch=main)
[![status](https://tinyverse.netlify.com/badge/metalite)](https://tinyverse.netlify.app/)

<!-- badges: end -->

## Overview

The purpose of the metalite R package is to define a unified data structure to save metadata information
for clinical analysis & reporting (A&R) based on ADaM datasets.

The R package is designed to

- standardize function input for analysis and reporting.
- separate analysis logic from data source.
- enables pipes (`|>`).
- reduce manual steps to develop and maintain documentation in clinical trial development.
- ensure consistency between analysis specification, mock, and results.

We build the metalite R package to enable principals below:

- Automation: a function call is better than a checklist.
- Single-entry: enter in one place, sync to all deliveries.
  - e.g., Enter data source one time for all AE analysis.
- End-to-end: cover all steps in software development lifecycle (SDLC) from define to delivery.

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
