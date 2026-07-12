# Statistical methods and limitations

## Input contract

The outcome is binary and must be coded 0 for non-event and 1 for event. IDs
must be complete and unique. Variables declared continuous must be numeric.
Configured predictors must contain at least two observed values.

## Descriptive statistics

Continuous variables are checked separately within each outcome group using
the Shapiro-Wilk test when its sample-size requirements are met. If both groups
pass, values are reported as mean (SD) and compared with Welch's t-test.
Otherwise, values are reported as median (IQR) and compared with the
Mann-Whitney U test.

Categorical variables are reported as count and percentage within each outcome
group. Missing values are displayed explicitly and included in percentage
denominators, but excluded from hypothesis tests. Fisher's exact test is used
for sparse 2 by 2 tables; otherwise a Pearson chi-square test is used. A label
flags chi-square results with small expected counts.

These unadjusted p-values are descriptive. The package does not apply
multiplicity correction or use them for automatic predictor selection.

## Model fitting

The same user-declared formula is fit with:

1. Maximum-likelihood logistic regression using stats::glm.
2. Firth penalized logistic regression using logistf::logistf.

Categorical predictors are represented by treatment contrasts. Continuous
predictors are modeled linearly on the log-odds scale and are not automatically
standardized or transformed.

Version 0.1.0 uses complete cases across the outcome and configured predictors.
The output reports how many rows were excluded and the minority outcome count
per estimated parameter. These diagnostics do not replace a prospective sample
size calculation.

## Performance

For each successfully fitted model, ClinRiskR reports:

- AUC with a 95 percent DeLong confidence interval.
- Brier score.
- A Youden-index threshold with sensitivity and specificity.
- Calibration intercept and slope estimated on the fitting cohort.

All values are apparent in-sample performance and are expected to be
optimistic. They do not establish transportability, clinical utility, or a
safe decision threshold. External validation is required.

## Known limitations

- No bootstrap optimism correction or cross-validation yet.
- No built-in multiple imputation.
- No nonlinear terms, interactions, competing risks, or time-to-event models.
- No decision-curve analysis.
- No automated model selection.
- No claim of regulatory validation or fitness for clinical care.
