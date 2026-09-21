# Changelog

## metalite 0.1.4

CRAN release: 2024-08-22

- Add SAS-compatible rounding and fixed-decimal formatting helpers, and
  use them for
  [`collect_n_subject()`](https://merck.github.io/metalite/reference/collect_n_subject.md)
  display values.
- Fix bug of
  [`n_subject()`](https://merck.github.io/metalite/reference/n_subject.md)
  for empty factor.
- Add default mapping for subject level analysis.
- Update GitHub Actions workflows.

## metalite 0.1.3

CRAN release: 2023-08-10

- [`n_subject()`](https://merck.github.io/metalite/reference/n_subject.md)
  now has a new argument `na` for labeling missing values.

## metalite 0.1.2

CRAN release: 2023-05-19

- Add styler workflow.
- Fix bug to count unique mock number properly in
  [`plan()`](https://merck.github.io/metalite/reference/plan.md).
- Fix bug to display `NA` values properly in
  [`collect_n_subject()`](https://merck.github.io/metalite/reference/collect_n_subject.md).
- Export
  [`n_subject()`](https://merck.github.io/metalite/reference/n_subject.md).
- Add test cases for functions within `collect_n_subject.R`.

## metalite 0.1.1

CRAN release: 2023-02-08

- Updated `DESCRIPTION` file to add more details to the `Description`
  field.
- Removed the usage of `:::` in documentation.

## metalite 0.1.0

- Initial version submitted to CRAN
- Added a `NEWS.md` file to track changes to the package.
