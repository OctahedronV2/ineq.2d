test_that("The function calculates overall SCV correctly.", {

  data("us16")

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) *
  sum(nr_wgt * (us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV using scv.2d.
  result <- scv.2d(us16, "hitotal", weights = "hpopwgt")
  scv2 <- result[1, 2]

  expect_equal(scv1, scv2)
})

test_that("The function returns NaN if all incomes are equal.", {

  data("us16")

  # Set total incomes of all households to 1.
  # If all members of the studied population earn the same income, SCV must be
  # equal to zero. But, scv.2d calculates "alpha," which is the absolute
  # contribution of the given income source to overall inequality. Its
  # calculation is impossible for identical incomes because the formula involves
  # division by variance of income, which is zero in this case.
  us16$hitotal <- 1

  # SCV using scv.2d.
  result <- scv.2d(us16, "hitotal", weights = "hpopwgt")
  scv2 <- result[1, 2]

  expect_equal(NaN, scv2)
})

test_that("The function calculates overall SCV correctly in case of extreme
          inequality.", {

  data("us16")

  # Set total incomes of all households to 1.
  us16$hitotal <- 1

  # One random household earns huge income.
  us16[, "hitotal"][sample(nrow(us16), 1)] <- 1000000

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) *
    sum(nr_wgt * (us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV using scv.2d.
  result <- scv.2d(us16, "hitotal", weights = "hpopwgt")
  scv2 <- result[1, 2]

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by sex is equal
          to the coefficient calcualted without decomposition.", {

  data("us16")

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) *
  sum(nr_wgt * (us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "sex", "hitotal", "hpopwgt")
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by education is equal
          to the coefficient calcualted without decomposition.", {

  data("us16")

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) *
  sum(nr_wgt * (us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "educ", "hitotal", "hpopwgt")
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by age is equal
          to the coefficient calcualted without decomposition.", {

  data("us16")

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) *
  sum(nr_wgt * (us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- scv.2d(us16, "hitotal", "cohort", "hitotal", "hpopwgt")
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by sex and by income source
          is equal to the coefficient calcualted without decomposition.", {

  data("us16")

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) *
    sum(nr_wgt * (us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "sex", c("hilabour", "hicapital",
                                             "hitransfer"), "hpopwgt")
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by education and by income
          source is equal to the coefficient calcualted without
          decomposition.", {

  data("us16")

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) *
    sum(nr_wgt * (us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "educ", c("hilabour", "hicapital",
                                              "hitransfer"), "hpopwgt")
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by age and by income source
          is equal to the coefficient calcualted without decomposition.", {

  data("us16")

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) *
  sum(nr_wgt * (us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- scv.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital",
                                                "hitransfer"), "hpopwgt")
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("The function calculates overall SCV correctly if it is calculated
          without using weights.", {

  data("us16")

  # SCV without decomposition.
  # Mean of total income.
  wt_mean <- mean(us16$hitotal)

  # variance of total income.
  wt_var <- 1 / nrow(us16) * sum((us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV using scv.2d.
  result <- scv.2d(us16, "hitotal")
  scv2 <- result[1, 2]

  expect_equal(scv1, scv2)
})

test_that("The function returns NaN if all incomes are equal and without
          population weights.", {

  data("us16")

  # Set total incomes of all households to 1.
  # If all members of the studied population earn the same income, SCV must be
  # equal to zero. But, scv.2d calculates "alpha," which is the absolute
  # contribution of the given income source to overall inequality. Its
  # calculation is impossible for identical incomes because the formula
  # involves division by variance of income, which is zero in this case.
  us16$hitotal <- 1

  # SCV using scv.2d.
  result <- scv.2d(us16, "hitotal")
  scv2 <- result[1, 2]

  expect_equal(NaN, scv2)
})

test_that("The function calculates overall SCV correctly in case of extreme
          inequality and without population weights.", {

  data("us16")

  # Set total incomes of all households to 1.
  us16$hitotal <- 1

  # One random household earns huge income.
  us16[, "hitotal"][sample(nrow(us16), 1)] <- 1000000

  # SCV without decomposition.
  # Mean of total income.
  wt_mean <- mean(us16$hitotal)

  # variance of total income.
  wt_var <- 1 / nrow(us16) * sum((us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV using scv.2d.
  result <- scv.2d(us16, "hitotal")
  scv2 <- result[1, 2]

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by sex is equal to the
          coefficient calcualted without decomposition if both cases are
          calculated without using weights.", {

  data("us16")

  # SCV without decomposition.
  # Mean of total income.
  wt_mean <- mean(us16$hitotal)

  # variance of total income.
  wt_var <- 1 / nrow(us16) * sum((us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "sex")
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by education is equal to the
          coefficient calcualted without decomposition if both cases are
          calculated without using weights.", {

  data("us16")

  # SCV without decomposition.
  # Mean of total income.
  wt_mean <- mean(us16$hitotal)

  # variance of total income.
  wt_var <- 1 / nrow(us16) * sum((us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "educ")
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by age is equal to the
          coefficient calcualted without decomposition if both cases are
          calculated without using weights.", {

  data("us16")

  # SCV without decomposition.
  # Mean of total income.
  wt_mean <- mean(us16$hitotal)

  # variance of total income.
  wt_var <- 1 / nrow(us16) * sum((us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- scv.2d(us16, "hitotal", "cohort")
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by sex and by income source
          is equal to the coefficient calcualted without decomposition if both
          cases are calculated without using weights.", {

  data("us16")

  # SCV without decomposition.
  # Mean of total income.
  wt_mean <- mean(us16$hitotal)

  # variance of total income.
  wt_var <- 1 / nrow(us16) * sum((us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "sex", c("hilabour", "hicapital",
                                             "hitransfer"))
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by education and by income
          source is equal to the coefficient calcualted without decomposition
          if both cases are calculated without using weights.", {

  data("us16")

  # SCV without decomposition.
  # Mean of total income.
  wt_mean <- mean(us16$hitotal)

  # variance of total income.
  wt_var <- 1 / nrow(us16) * sum((us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "educ", c("hilabour", "hicapital",
                                              "hitransfer"))
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by age and by income source
          is equal to the coefficient calcualted without decomposition
          if both cases are calculated without using weights.", {

  data("us16")

  # SCV without decomposition.
  # Mean of total income.
  wt_mean <- mean(us16$hitotal)

  # variance of total income.
  wt_var <- 1 / nrow(us16) * sum((us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- scv.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital",
                                                "hitransfer"))
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by sex and by income source
          is not equal to the coefficient calcualted without decomposition if
          the sum of income sources is not equal to total income.", {

  data("us16")

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) * sum(nr_wgt * (us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "sex", c("hilabour", "hicapital"),
                   "hpopwgt")

  scv2 <- sum(result[,-1])

  expect_false(isTRUE(all.equal(scv1, scv2)))
})

test_that("Sum of the components of SCV decomposed by education and by income
          source is not equal to the coefficient calcualted without
          decomposition if the sum of income sources is not equal to total
          income.", {

  data("us16")

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) * sum(nr_wgt * (us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "educ", c("hilabour", "hicapital"),
                   "hpopwgt")

  scv2 <- sum(result[,-1])

  expect_false(isTRUE(all.equal(scv1, scv2)))
})

test_that("Sum of the components of SCV decomposed by age and by income source
          is not equal to the coefficient calcualted without decomposition if
          the sum of income sources is not equal to total income.", {

  data("us16")

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) *
  sum(nr_wgt * (us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- scv.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital"),
                    "hpopwgt")

  scv2 <- sum(result[,-1])

  expect_false(isTRUE(all.equal(scv1, scv2)))
})

test_that("Sum of the components of SCV decomposed by sex and by income source
          is not equal to the coefficient calcualted without decomposition if
          the sum of income sources is not equal to total income (without
          population weights).", {

  data("us16")

  # SCV without decomposition.
  # Mean of total income.
  wt_mean <- mean(us16$hitotal)

  # variance of total income.
  wt_var <- 1 / nrow(us16) * sum((us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "sex", c("hilabour", "hicapital"))

  scv2 <- sum(result[,-1])

  expect_false(isTRUE(all.equal(scv1, scv2)))
})

test_that("Sum of the components of SCV decomposed by education and by income
          source is not equal to the coefficient calcualted without
          decomposition if the sum of income sources is not equal to total
          income (without population weights).", {

  data("us16")

  # SCV without decomposition.
  # Mean of total income.
  wt_mean <- mean(us16$hitotal)

  # variance of total income.
  wt_var <- 1 / nrow(us16) * sum((us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "educ", c("hilabour", "hicapital"))

  scv2 <- sum(result[,-1])

  expect_false(isTRUE(all.equal(scv1, scv2)))
})

test_that("Sum of the components of SCV decomposed by age and by income source
          is not equal to the coefficient calcualted without decomposition if
          the sum of income sources is not equal to total income (without
          population weights).", {

  data("us16")

  # SCV without decomposition.
  # Mean of total income.
  wt_mean <- mean(us16$hitotal)

  # variance of total income.
  wt_var <- 1 / nrow(us16) * sum((us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- scv.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital"))

  scv2 <- sum(result[,-1])

  expect_false(isTRUE(all.equal(scv1, scv2)))
})

test_that("Presence of negative and zero weights results in error.", {

  data("us16")

  us16$hpopwgt[sample(nrow(us16), 5)] <- 0
  us16$hpopwgt[sample(nrow(us16), 5)] <- -1

  expect_error(scv.2d(us16, "hitotal", "sex", "hitotal", "hpopwgt"))
})

test_that("Sum of the components of SCV decomposed by sex and by income source
          is equal to the coefficient calcualted without decomposition
          if NA values are present in the dataset.", {

  data("us16")

  # Randomly place NA values.
  # Only selected columns are used because scv.2d will remove unused features,
  # which are education and age in this case.
  columns <- c("hitotal", "sex", "hilabour", "hicapital", "hitransfer",
               "hpopwgt")

  for (i in columns){
    us16[, i][sample(nrow(us16), 5)] <- NA
  }

  copy <- us16
  copy <- na.omit(us16)

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- copy$hpopwgt / sum(copy$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(copy$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) *
  sum(nr_wgt * (copy$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "sex", c("hilabour", "hicapital",
                                             "hitransfer"), "hpopwgt")
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by education and by income
          source is equal to the coefficient calcualted without decomposition
          if NA values are present in the dataset.", {

  data("us16")

  # Randomly place NA values.
  # Only selected columns are used because scv.2d will remove unused features,
  # which are sex and age in this case.
  columns <- c("hitotal", "educ", "hilabour", "hicapital", "hitransfer",
               "hpopwgt")

  for (i in columns){
    us16[, i][sample(nrow(us16), 5)] <- NA
  }

  copy <- us16
  copy <- na.omit(us16)

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- copy$hpopwgt / sum(copy$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(copy$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) *
  sum(nr_wgt * (copy$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "educ", c("hilabour", "hicapital",
                                              "hitransfer"), "hpopwgt")
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by age and by income
          source is equal to the coefficient calcualted without decomposition
          if NA values are present in the dataset.", {

  data("us16")

  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  # Randomly place NA values.
  # Only selected columns are used because scv.2d will remove unused features,
  # which are sex and education in this case.
  columns <- c("hitotal", "cohort", "hilabour", "hicapital", "hitransfer",
               "hpopwgt")

  for (i in columns){
    us16[, i][sample(nrow(us16), 5)] <- NA
  }

  copy <- us16
  copy <- na.omit(us16)

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- copy$hpopwgt / sum(copy$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(copy$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) *
  sum(nr_wgt * (copy$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital",
                                                "hitransfer"), "hpopwgt")
  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed only by income source
          is equal to the coefficient calcualted without decomposition.", {

  data("us16")

  # SCV without decomposition.
  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)

  # Weighted variance of total income.
  wt_var <- (1 / (sum(nr_wgt))) *
  sum(nr_wgt * (us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal",
                   sources = c("hilabour", "hicapital", "hitransfer"),
                   weights = "hpopwgt")

  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed only by income source
          is equal to the coefficient calcualted without decomposition if both
          cases are calculated without using weights.", {

  data("us16")

  # SCV without decomposition.
  # Mean of total income.
  wt_mean <- mean(us16$hitotal)

  # variance of total income.
  wt_var <- 1 / nrow(us16) * sum((us16$hitotal - wt_mean)^2)

  # Manual SCV.
  scv1 <- wt_var / (2 * wt_mean^2)

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal",
                   sources = c("hilabour", "hicapital", "hitransfer"))

  scv2 <- sum(result[,-1])

  expect_equal(scv1, scv2)
})

test_that("Sum of the components of SCV decomposed by sex expressed as
          percentage share of total income is equal to one hundred.", {

  data("us16")

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "sex", "hitotal", "hpopwgt", TRUE)

  scv <- sum(result[,-1])

  expect_equal(100, scv)
})

test_that("Sum of the components of SCV decomposed by education expressed as
          percentage share of total income is equal to one  hundred.", {

  data("us16")

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "educ", "hitotal", "hpopwgt", TRUE)

  scv <- sum(result[,-1])

  expect_equal(100, scv)
})

test_that("Sum of the components of SCV decomposed by age expressed as
          percentage share of total income is equal to one hundred.", {

  data("us16")

  # SCV after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- scv.2d(us16, "hitotal", "cohort", "hitotal", "hpopwgt", TRUE)

  scv <- sum(result[,-1])

  expect_equal(100, scv)
})

test_that("Sum of the components of SCV decomposed by sex and by income source
          expressed as percentage share of total income is equal to
          one hundred.", {

  data("us16")

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "sex",
                     c("hilabour", "hicapital", "hitransfer"),
                     "hpopwgt", TRUE)

  scv <- sum(result[,-1])

  expect_equal(100, scv)
})

test_that("Sum of the components of SCV decomposed by education and by income
          source expressed as percentage share of total income is equal to one
          hundred.", {

  data("us16")

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "educ",
                     c("hilabour", "hicapital", "hitransfer"),
                     "hpopwgt", TRUE)

  scv <- sum(result[,-1])

  expect_equal(100, scv)
})

test_that("Sum of the components of SCV decomposed by age and by income source
          expressed as percentage share of total income is equal to
          one hundred.", {

  data("us16")

  # SCV after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- scv.2d(us16, "hitotal", "cohort",
                     c("hilabour", "hicapital", "hitransfer"),
                     "hpopwgt", TRUE)

  scv <- sum(result[,-1])

  expect_equal(100, scv)
})

test_that("Output of the function is a data frame with specific dimensions
          when income is decomposed by sex.", {

  data("us16")

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "sex", "hitotal", "hpopwgt")

  # There are no income sources specified - 1 row.
  # There are 2 genders, each having within and between dimension, and
  # the column with names of income sources - 5 columns.
  expect_true(all(is.data.frame(result), ncol(result) == 5,
                  nrow(result) == 1))
})

test_that("Output of the function is a data frame with specific dimensions
          when income is decomposed by education.", {

  data("us16")

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "educ", "hitotal", "hpopwgt")

  # There are no income sources specified - 1 row.
  # There are 3 education levels, each having within and between dimension, and
  # the column with names of income sources - 7 columns.
  expect_true(all(is.data.frame(result), ncol(result) == 7,
                  nrow(result) == 1))
})

test_that("Output of the function is a data frame with specific dimensions
          when income is decomposed by age.", {

  data("us16")

  # SCV after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- scv.2d(us16, "hitotal", "cohort", "hitotal", "hpopwgt")

  # There are no income sources specified - 1 row.
  # There are 4 age cohorts, each having within and between dimension, and
  # the column with names of income sources - 9 columns.
  expect_true(all(is.data.frame(result), ncol(result) == 9,
                  nrow(result) == 1))
})

test_that("Output of the function is a data frame with specific dimensions
          when income is decomposed by sex and income source.", {

  data("us16")

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "sex", c("hilabour", "hicapital",
                                             "hitransfer"), "hpopwgt")
  # There are 3 income sources - 3 rows.
  # There are 2 genders, each having within and between dimension, and
  # the column with names of income sources - 5 columns.
  expect_true(all(is.data.frame(result), ncol(result) == 5,
                  nrow(result) == 3))
})

test_that("Output of the function is a data frame with specific dimensions
          when income is decomposed by education and income source.", {

  data("us16")

  # SCV after decomposition.
  result <- scv.2d(us16, "hitotal", "educ", c("hilabour", "hicapital",
                                              "hitransfer"), "hpopwgt")
  # There are 3 income sources - 3 rows.
  # There are 3 education levels, each having within and between dimension, and
  # the column with names of income sources - 7 columns.
  expect_true(all(is.data.frame(result), ncol(result) == 7,
                  nrow(result) == 3))
})

test_that("Output of the function is a data frame with specific dimensions
          when income is decomposed by age and income source.", {

  data("us16")

  # SCV after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- scv.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital",
                                                "hitransfer"), "hpopwgt")
  # There are 3 income sources - 3 rows.
  # There are 4 age cohorts, each having within and between dimension, and
  # the column with names of income sources - 9 columns.
  expect_true(all(is.data.frame(result), ncol(result) == 9,
                  nrow(result) == 3))
})

test_that("Output of the function is a data frame with specific dimensions
          when neither feature nor income sources are specified.", {

  data("us16")

  # SCV.
  result <- scv.2d(us16, "hitotal", weights = "hpopwgt")

  # There is only total income - 1 row.
  # There is only overall coefficient, and the column with the name of the only
  # income source - 2 columns.
  expect_true(all(is.data.frame(result), ncol(result) == 2,
                  nrow(result) == 1))
})
