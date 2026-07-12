#' Simulate a clinically plausible HDP cohort
#'
#' Generates a fully synthetic hypertensive-disorders-of-pregnancy cohort with
#' correlated vital signs, laboratory markers, treatments, and a probabilistic
#' binary outcome. The data do not represent real patients.
#'
#' @param n Number of rows to generate.
#' @param outcome_rate Target marginal event rate.
#' @param missing_rate Approximate missingness applied to selected laboratory
#'   variables after outcome generation.
#' @param seed Integer random seed.
#'
#' @return A data frame with one row per synthetic participant.
#' @export
#' @examples
#' data <- simulate_hdp_data(n = 100, seed = 42)
#' mean(data$outcome)
simulate_hdp_data <- function(n = 420L, outcome_rate = 0.38, missing_rate = 0.02, seed = 20260502L) {
  if (length(n) != 1L || !is.numeric(n) || n < 40 || n != as.integer(n)) {
    stop("n must be one integer of at least 40.", call. = FALSE)
  }
  if (
    length(outcome_rate) != 1L || !is.numeric(outcome_rate) ||
      outcome_rate <= 0.05 || outcome_rate >= 0.95
  ) {
    stop("outcome_rate must be between 0.05 and 0.95.", call. = FALSE)
  }
  if (
    length(missing_rate) != 1L || !is.numeric(missing_rate) ||
      missing_rate < 0 || missing_rate > 0.20
  ) {
    stop("missing_rate must be between 0 and 0.20.", call. = FALSE)
  }

  set.seed(as.integer(seed))
  n <- as.integer(n)
  age <- clip_number(stats::rnorm(n, 30.2, 4.6), 20, 43)
  bmi <- clip_number(stats::rnorm(n, 24.8 + 0.10 * (age - 30), 3.5), 18, 40)
  gravida <- sample(1:5, n, replace = TRUE, prob = c(0.30, 0.32, 0.20, 0.12, 0.06))
  para_prob <- stats::plogis(-0.6 + 0.12 * (age - 30))
  para <- pmin(
    stats::rbinom(n, size = pmax(gravida - 1, 0), prob = para_prob),
    gravida - 1
  )

  standardize <- function(x) as.numeric(scale(x))
  severity <- stats::rnorm(n) +
    0.18 * standardize(age) +
    0.22 * standardize(bmi) +
    0.15 * standardize(gravida) -
    0.08 * standardize(para)
  p_severe <- stats::plogis(-1.2 + 1.2 * severity)
  p_pe_or_severe <- stats::plogis(-0.15 + 1.05 * severity)
  random_group <- stats::runif(n)
  hdp_num <- ifelse(
    random_group < p_severe,
    3L,
    ifelse(random_group < p_pe_or_severe, 2L, 1L)
  )

  sbp <- clip_number(
    stats::rnorm(
      n,
      134 + 12 * (hdp_num == 2) + 24 * (hdp_num == 3) + 4 * severity,
      10
    ),
    118,
    210
  )
  dbp <- clip_number(0.58 * sbp + stats::rnorm(n, 11, 7), 70, 130)
  ua <- clip_number(
    stats::rnorm(
      n,
      285 + 38 * severity + 48 * (hdp_num == 2) + 92 * (hdp_num == 3),
      42
    ),
    160,
    650
  )
  plt <- clip_number(
    stats::rnorm(
      n,
      235 - 18 * severity - 24 * (hdp_num == 2) - 58 * (hdp_num == 3),
      36
    ),
    55,
    420
  )
  alt <- clip_number(
    stats::rnorm(
      n,
      24 + 6 * severity + 10 * (hdp_num == 2) + 24 * (hdp_num == 3),
      12
    ),
    6,
    180
  )
  ast <- clip_number(0.76 * alt + 4 * severity + stats::rnorm(n, 6, 7), 8, 160)
  cr <- clip_number(
    stats::rnorm(
      n,
      66 + 4 * severity + 9 * (hdp_num == 2) + 18 * (hdp_num == 3),
      10
    ),
    40,
    180
  )

  proteinuria <- stats::rbinom(
    n,
    1,
    stats::plogis(
      -2 + 0.85 * (hdp_num == 2) + 1.85 * (hdp_num == 3) + 0.25 * severity
    )
  )
  antihyp <- stats::rbinom(
    n,
    1,
    stats::plogis(
      -1.55 + 0.09 * (sbp - 140) + 0.40 * proteinuria + 0.35 * (hdp_num == 3)
    )
  )
  mgso4 <- stats::rbinom(
    n,
    1,
    stats::plogis(
      -3 + 0.85 * (hdp_num == 2) + 2.10 * (hdp_num == 3) + 0.45 * proteinuria
    )
  )

  linear_predictor <- 0.08 * standardize(age) +
    0.18 * standardize(bmi) +
    0.30 * standardize(sbp) +
    0.36 * standardize(ua) -
    0.26 * standardize(plt) +
    0.16 * standardize(cr) +
    0.15 * standardize(alt) +
    0.34 * (hdp_num == 2) +
    0.72 * (hdp_num == 3) +
    0.48 * proteinuria +
    0.12 * antihyp +
    0.10 * mgso4 +
    stats::rnorm(n, 0, 0.55)
  intercept <- stats::uniroot(
    function(a) mean(stats::plogis(a + linear_predictor)) - outcome_rate,
    interval = c(-8, 8)
  )$root
  outcome <- stats::rbinom(
    n,
    1,
    stats::plogis(intercept + linear_predictor)
  )

  result <- data.frame(
    id = seq_len(n),
    outcome = outcome,
    age = round(age, 1),
    bmi = round(bmi, 1),
    gravida = gravida,
    para = para,
    sbp = round(sbp),
    dbp = round(dbp),
    hdp_type = factor(
      hdp_num,
      levels = 1:3,
      labels = c("GH", "PE", "Severe PE")
    ),
    proteinuria = factor(
      proteinuria,
      levels = 0:1,
      labels = c("No", "Yes")
    ),
    ua = round(ua),
    plt = round(plt),
    alt = round(alt, 1),
    ast = round(ast, 1),
    cr = round(cr, 1),
    antihyp = factor(antihyp, levels = 0:1, labels = c("No", "Yes")),
    mgso4 = factor(mgso4, levels = 0:1, labels = c("No", "Yes")),
    check.names = FALSE
  )

  if (missing_rate > 0) {
    missing_candidates <- c("bmi", "ua", "plt", "alt", "ast", "cr")
    for (variable in missing_candidates) {
      count <- as.integer(round(n * missing_rate))
      if (count > 0L) {
        result[sample.int(n, count), variable] <- NA
      }
    }
  }
  result
}
