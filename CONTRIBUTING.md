# Contributing

Thank you for helping make small-cohort clinical analyses more reproducible.

## Before opening a change

- Search existing issues and describe the research workflow the change serves.
- Never submit patient-level, confidential, or institution-restricted data.
- Use synthetic or clearly licensed public fixtures in tests and examples.
- Separate statistical-method changes from presentation-only changes when
  practical.

## Development workflow

1. Install package dependencies from DESCRIPTION.
2. Run roxygen2::roxygenise() after changing exported function documentation.
3. Run testthat::test_local().
4. Run R CMD check with no errors, warnings, or unexplained notes.
5. Update NEWS.md for user-visible behavior.

Pull requests should explain the assumption being changed, include tests, and
state whether output schemas are affected. Changes to statistical defaults need
a supporting methodological reference and a migration note.

## Review priorities

Maintainers review privacy and data leakage risk first, statistical correctness
second, backward compatibility third, and convenience last. A smaller,
well-tested contribution is easier to review than a broad rewrite.
