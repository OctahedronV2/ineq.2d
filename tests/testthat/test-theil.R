test_that("The function calculates overall Theil index correctly.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index using theil.2d.
  result <- theil.2d(us16, "hitotal", weight = "hpopwgt")
  theil2 <- result[1, 2]

  expect_equal(theil1, theil2)
})

test_that("The function calculates overall Theil index correctly if all
          incomes are equal.", {

  data("us16")

  # Set total incomes of all households to 1.
  # If all members of the studied population earn the same income, the Theil
  # index must be zero!
  us16$hitotal <- 1

  # Theil index using theil.2d.
  result <- theil.2d(us16, "hitotal", weight = "hpopwgt")
  theil2 <- result[1, 2]

  expect_equal(0, theil2)
})

test_that("The function calculates overall Theil index correctly in case of
          extreme inequality.", {

  data("us16")

  # Theil index without decomposition.
  # Set total incomes of all households to 1.
  us16$hitotal <- 1

  # One random household earns huge income.
  us16[, "hitotal"][sample(nrow(us16), 1)] <- 1000000

  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index using theil.2d.
  result <- theil.2d(us16, "hitotal", weight = "hpopwgt")
  theil2 <- result[1, 2]

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by sex is equal
          to the index calcualted without decomposition.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "sex", "hitotal", "hpopwgt")
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by education is
          equal to the index calcualted without decomposition.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "educ", "hitotal", "hpopwgt")
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by age is
          equal to the index calcualted without decomposition.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- theil.2d(us16, "hitotal", "cohort", "hitotal", "hpopwgt")
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by sex and by
          income source is equal to the index calcualted without
          decomposition.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "sex", c("hilabour", "hicapital",
                                               "hitransfer"), "hpopwgt")
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by education and
          by income source is equal to the index calcualted without
          decomposition.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "educ", c("hilabour", "hicapital",
                                                "hitransfer"), "hpopwgt")
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by age and by
          income source is equal to the index calcualted without
          decomposition.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- theil.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital",
                                                  "hitransfer"), "hpopwgt")
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("The function calculates overall Theil index correctly if it is
          calculated without using weights.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Mean of total income.
  wt_mean <- mean(us16$hitotal)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- 1 / nrow(us16) * sum(i_ratio * log(i_ratio))

  # Theil index using theil.2d.
  result <- theil.2d(us16, "hitotal")
  theil2 <- result[1, 2]

  expect_equal(theil1, theil2)
})

test_that("The function calculates overall Theil index correctly if all
          incomes are equal and without population weights.", {

  data("us16")

  # Set total incomes of all households to 1.
  # If all members of the studied population earn the same income, the Theil
  # index must be zero!
  us16$hitotal <- 1

  # Theil index using theil.2d.
  result <- theil.2d(us16, "hitotal")
  theil2 <- result[1, 2]

  expect_equal(0, theil2)
})

test_that("The function calculates overall Theil index correctly in case of
          extreme inequality and without population weights.", {

  data("us16")

  # Theil index without decomposition.
  # Set total incomes of all households to 1.
  us16$hitotal <- 1

  # One random household earns huge income.
  us16[, "hitotal"][sample(nrow(us16), 1)] <- 1000000

  # Mean of total income.
  wt_mean <- mean(us16$hitotal)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- 1 / nrow(us16) * sum(i_ratio * log(i_ratio))

  # Theil index using theil.2d.
  result <- theil.2d(us16, "hitotal")
  theil2 <- result[1, 2]

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by sex is equal
          to the index calcualted without decomposition if both cases are
          calculated without using weights.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Mean of total income.
  wt_mean <- mean(us16$hitotal)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- 1 / nrow(us16) * sum(i_ratio * log(i_ratio))

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "sex")
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by education is
          equal to the index calcualted without decomposition if both cases are
          calculated without using weights.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Mean of total income.
  wt_mean <- mean(us16$hitotal)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- 1 / nrow(us16) * sum(i_ratio * log(i_ratio))

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "educ")
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by age is
          equal to the index calcualted without decomposition if both cases are
          calculated without using weights.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Mean of total income.
  wt_mean <- mean(us16$hitotal)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- 1 / nrow(us16) * sum(i_ratio * log(i_ratio))

  # Theil index after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- theil.2d(us16, "hitotal", "cohort")
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by sex and by
          income source is equal to the index calcualted without decomposition
          if both cases are calculated without using weights.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Mean of total income.
  wt_mean <- mean(us16$hitotal)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- 1 / nrow(us16) * sum(i_ratio * log(i_ratio))

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "sex", c("hilabour", "hicapital",
                                               "hitransfer"))
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by education and
          by income source is equal to the index calcualted without
          decomposition if both cases are calculated without using weights.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Mean of total income.
  wt_mean <- mean(us16$hitotal)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- 1 / nrow(us16) * sum(i_ratio * log(i_ratio))

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "educ", c("hilabour", "hicapital",
                                                "hitransfer"))
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by age and by
          income source is equal to the index calcualted without decomposition
          if both cases are calculated without using weights.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Mean of total income.
  wt_mean <- mean(us16$hitotal)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- 1 / nrow(us16) * sum(i_ratio * log(i_ratio))

  # Theil index after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- theil.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital",
                                                  "hitransfer"))
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by sex and by
          income source is not equal to the index calcualted without
          decomposition if the sum of income sources are not equal to total
          income.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index after decomposition.
  # hitransfer is not specified as income source!
  result <- theil.2d(us16, "hitotal", "sex", c("hilabour", "hicapital"),
                     "hpopwgt")

  theil2 <- sum(result[,-1])

  expect_false(isTRUE(all.equal(theil1, theil2)))
})

test_that("Sum of the components of the Theil index decomposed by educatoin and
          by income source is not equal to the index calcualted without
          decomposition if the sum of income sources are not equal to total
          income.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index after decomposition.
  # hitransfer is not specified as income source!
  result <- theil.2d(us16, "hitotal", "educ", c("hilabour", "hicapital"),
                     "hpopwgt")

  theil2 <- sum(result[,-1])

  expect_false(isTRUE(all.equal(theil1, theil2)))
})

test_that("Sum of the components of the Theil index decomposed by age and by
          income source is not equal to the index calcualted without
          decomposition if the sum of income sources are not equal to total
          income.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  # hitransfer is not specified as income source!
  result <- theil.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital"),
                      "hpopwgt")

  theil2 <- sum(result[,-1])

  expect_false(isTRUE(all.equal(theil1, theil2)))
})

test_that("Sum of the components of the Theil index decomposed by sex and by
          income source is not equal to the index calcualted without
          decomposition if the sum of income sources are not equal to total
          income (without population weights).", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Mean of total income.
  wt_mean <- mean(us16$hitotal)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- 1 / nrow(us16) * sum(i_ratio * log(i_ratio))

  # Theil index after decomposition.
  # hitransfer is not specified as income source!
  result <- theil.2d(us16, "hitotal", "sex", c("hilabour", "hicapital"))

  theil2 <- sum(result[,-1])

  expect_false(isTRUE(all.equal(theil1, theil2)))
})

test_that("Sum of the components of the Theil index decomposed by educatoin and
          by income source is not equal to the index calcualted without
          decomposition if the sum of income sources are not equal to total
          income (without population weights).", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Mean of total income.
  wt_mean <- mean(us16$hitotal)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- 1 / nrow(us16) * sum(i_ratio * log(i_ratio))

  # Theil index after decomposition.
  # hitransfer is not specified as income source!
  result <- theil.2d(us16, "hitotal", "educ", c("hilabour", "hicapital"))

  theil2 <- sum(result[,-1])

  expect_false(isTRUE(all.equal(theil1, theil2)))
})

test_that("Sum of the components of the Theil index decomposed by age and by
          income source is not equal to the index calcualted without
          decomposition if the sum of income sources are not equal to total
          income (without population weights).", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Mean of total income.
  wt_mean <- mean(us16$hitotal)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- 1 / nrow(us16) * sum(i_ratio * log(i_ratio))

  # Theil index after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  # hitransfer is not specified as income source!
  result <- theil.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital"))

  theil2 <- sum(result[,-1])

  expect_false(isTRUE(all.equal(theil1, theil2)))
})

test_that("Presence of negative and zero weights results in error.", {

  data("us16")

  us16$hpopwgt[sample(nrow(us16), 5)] <- 0
  us16$hpopwgt[sample(nrow(us16), 5)] <- -1

  expect_error(theil.2d(us16, "hitotal", "sex", "hitotal", "hpopwgt"))
})

test_that("Sum of the components of the Theil index decomposed by sex and by
          income source is equal to the index calcualted without decomposition
          if NA values are present in the dataset.", {

  data("us16")

  # Randomly place NA values.
  # Only selected columns are used because theil.2d will remove unused features,
  # which are education and age in this case.
  columns <- c("hitotal", "sex", "hilabour", "hicapital", "hitransfer",
               "hpopwgt")

  for (i in columns){
    us16[, i][sample(nrow(us16), 5)] <- NA
  }

  copy <- us16
  copy <- na.omit(us16)

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  copy <- copy[copy$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- copy$hpopwgt / sum(copy$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(copy$hitotal, nr_wgt)
  i_ratio <- copy$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "sex", c("hilabour", "hicapital",
                                               "hitransfer"), "hpopwgt")
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by education and
          by income source is equal to the index calcualted without
          decomposition if NA values are present in the dataset.", {

  data("us16")

  # Randomly place NA values.
  # Only selected columns are used because theil.2d will remove unused features,
  # which are sex and age in this case.
  columns <- c("hitotal", "educ", "hilabour", "hicapital", "hitransfer",
               "hpopwgt")

  for (i in columns){
    us16[, i][sample(nrow(us16), 5)] <- NA
  }

  copy <- us16
  copy <- na.omit(us16)

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  copy <- copy[copy$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- copy$hpopwgt / sum(copy$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(copy$hitotal, nr_wgt)
  i_ratio <- copy$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "educ", c("hilabour", "hicapital",
                                                "hitransfer"), "hpopwgt")
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed by age and by
          income source is equal to the index calcualted without decomposition
          if NA values are present in the dataset.", {

  data("us16")

  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  # Randomly place NA values.
  # Only selected columns are used because theil.2d will remove unused features,
  # which are education and sex in this case.
  columns <- c("hitotal", "cohort", "hilabour", "hicapital", "hitransfer",
               "hpopwgt")

  for (i in columns){
    us16[, i][sample(nrow(us16), 5)] <- NA
  }

  copy <- us16
  copy <- na.omit(us16)

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  copy <- copy[copy$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- copy$hpopwgt / sum(copy$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(copy$hitotal, nr_wgt)
  i_ratio <- copy$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital",
                                                  "hitransfer"), "hpopwgt")
  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed only by
          income source is equal to the index calcualted without
          decomposition.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Normalized weights.
  nr_wgt <- us16$hpopwgt / sum(us16$hpopwgt)

  # Weighted mean of total income.
  wt_mean <- weighted.mean(us16$hitotal, nr_wgt)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- sum(nr_wgt * i_ratio * log(i_ratio))

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal",
                     sources = c("hilabour", "hicapital", "hitransfer"),
                     weights = "hpopwgt")

  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})

test_that("Sum of the components of the Theil index decomposed only by
          income source is equal to the index calcualted without decomposition
          if both cases are calculated without using weights.", {

  data("us16")

  # Theil index without decomposition.
  # Theil is not defined for non-positive values.
  us16 <- us16[us16$hitotal > 0,]

  # Mean of total income.
  wt_mean <- mean(us16$hitotal)
  i_ratio <- us16$hitotal / wt_mean

  theil1 <- 1 / nrow(us16) * sum(i_ratio * log(i_ratio))

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal",
                     sources = c("hilabour", "hicapital", "hitransfer"))

  theil2 <- sum(result[,-1])

  expect_equal(theil1, theil2)
})


test_that("Sum of the components of the Theil index decomposed by sex expressed
          as percentage share of total income is equal to one hundred.", {

  data("us16")

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "sex", "hitotal", "hpopwgt", TRUE)

  theil <- sum(result[,-1])

  expect_equal(100, theil)
})

test_that("Sum of the components of the Theil index decomposed by education
          expressed as percentage share of total income is equal to
          one hundred.", {

  data("us16")

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "educ", "hitotal", "hpopwgt", TRUE)

  theil <- sum(result[,-1])

  expect_equal(100, theil)
})

test_that("Sum of the components of the Theil index decomposed by age expressed
          as percentage share of total income is equal to one hundred.", {

  data("us16")

  # Theil index after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- theil.2d(us16, "hitotal", "cohort", "hitotal", "hpopwgt", TRUE)

  theil <- sum(result[,-1])

  expect_equal(100, theil)
})

test_that("Sum of the components of the Theil index decomposed by sex and by
          income source expressed as percentage share of total income is equal
          to one hundred.", {

  data("us16")

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "sex",
                     c("hilabour", "hicapital", "hitransfer"),
                     "hpopwgt", TRUE)

  theil <- sum(result[,-1])

  expect_equal(100, theil)
})

test_that("Sum of the components of the Theil index decomposed by education and
          by income source expressed as percentage share of total income is
          equal to one hundred.", {

  data("us16")

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "educ",
                     c("hilabour", "hicapital", "hitransfer"),
                     "hpopwgt", TRUE)

  theil <- sum(result[,-1])

  expect_equal(100, theil)
})

test_that("Sum of the components of the Theil index decomposed by age and by
          income source expressed as percentage share of total income is equal
          to one hundred.", {

  data("us16")

  # Theil index after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- theil.2d(us16, "hitotal", "cohort",
                     c("hilabour", "hicapital", "hitransfer"),
                     "hpopwgt", TRUE)

  theil <- sum(result[,-1])

  expect_equal(100, theil)
})

test_that("Output of the function is a data frame with specific dimensions,
          when income is decomposed by sex.", {

  data("us16")

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "sex", "hitotal", "hpopwgt")
  # There are no income sources specified - 1 row.
  # There are 2 genders, each having within and between dimension, and
  # the column with names of income sources - 5 columns.
  expect_true(all(is.data.frame(result), ncol(result) == 5,
                  nrow(result) == 1))
})

test_that("Output of the function is a data frame with specific dimensions,
          when income is decomposed by education.", {

  data("us16")

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "educ", "hitotal", "hpopwgt")
  # There are no income sources specified - 1 row.
  # There are 3 education levels, each having within and between dimension, and
  # the column with names of income sources - 7 columns.
  expect_true(all(is.data.frame(result), ncol(result) == 7,
                  nrow(result) == 1))
})

test_that("Output of the function is a data frame with specific dimensions
          when income is decomposed by age.", {

  data("us16")

  # Theil index after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- theil.2d(us16, "hitotal", "cohort", "hitotal", "hpopwgt")
  # There are no income sources specified - 1 row.
  # There are 4 age cohorts, each having within and between dimension, and
  # the column with names of income sources - 9 columns.
  expect_true(all(is.data.frame(result), ncol(result) == 9,
                  nrow(result) == 1))
})

test_that("Output of the function is a data frame with specific dimensions
          when income is decomposed by sex and income source.", {

  data("us16")

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "sex", c("hilabour", "hicapital",
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

  # Theil index after decomposition.
  result <- theil.2d(us16, "hitotal", "educ", c("hilabour", "hicapital",
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

  # Theil index after decomposition.
  # Assign households to age cohorts.
  us16$cohort <- 0
  us16[us16$age < 25, "cohort"] <- "t24"
  us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
  us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
  us16[us16$age >= 75, "cohort"] <- "f75"

  result <- theil.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital",
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

  # Theil index.
  result <- theil.2d(us16, "hitotal", weights = "hpopwgt")

  # There is only total income - 1 row.
  # There is only overall index, and the column with the name of the only income
  # source - 2 columns.
  expect_true(all(is.data.frame(result), ncol(result) == 2,
                  nrow(result) == 1))
})
