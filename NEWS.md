# metalite 0.1.4

- Speed up `n_subject()` when a parameter is supplied by deduplicating `(id, group, par)` with an integer key and counting cells with `tabulate()`, instead of the `data.frame()` / `unique.data.frame()` / `table()` pipeline. Results, including factor-level order and the `useNA` trailing column, are unchanged (~7x faster on large observation tables).
- Fix bug of `n_subject()` for empty factor.
- Add default mapping for subject level analysis.
- Update GitHub Actions workflows.

# metalite 0.1.3

- `n_subject()` now has a new argument `na` for labeling missing values.

# metalite 0.1.2

- Add styler workflow.
- Fix bug to count unique mock number properly in `plan()`.
- Fix bug to display `NA` values properly in `collect_n_subject()`.
- Export `n_subject()`.
- Add test cases for functions within `collect_n_subject.R`.

# metalite 0.1.1

- Updated `DESCRIPTION` file to add more details to the `Description` field.
- Removed the usage of `:::` in documentation.

# metalite 0.1.0

- Initial version submitted to CRAN
- Added a `NEWS.md` file to track changes to the package.
