#' Function performing two-dimensional decomposition of the squared
#' coefficient of variation (SCV)
#'
#' @param data Data frame containing income data. Must contain at least one
#' column with numeric values.
#' @param total String specifying the name of the column containing data on
#' total income.
#' @param feature String specifying the name of the column containing
#' information about the feature used for inequality decomposition. If left
#' blank, total income is not decomposed by feature.
#' @param sources Vector containing strings specifying the names of the columns
#' with data on income sources, the sum of which must be equal to total income.
#' If left blank, or the same value as in "total" is specified, then total
#' income is not decomposed by income source.
#' @param weights String specifying the name of the column containing population
#' weights.
#' @param perc If set to TRUE, then the function returns percentage shares of
#' every inequality component in overall inequality. Set to FALSE by default.
#'
#' @description The function performs two-dimensional decomposition of the
#' squared coefficient of variation according to Garcia-Penalosa & Orgiazzi
#' (2013). That is, the coefficient can be decomposed by some feature that
#' members of the studied population possess (e.g., sex, education, age) and
#' their income source at the same time.
#'
#' @return Data frame containing values of components of SCV.
#'
#' Columns of the data frame represent values of the feature used for
#' decomposition. There can be inequality within groups formed by this feature
#' and between them - there are twice as much columns as values of the given
#' feature. Whether a column contains a value of within or between-group
#' inequality is indicated by ".W" and ".B" suffixes respectively.
#'
#' Every row of the data frame represents an income source.
#'
#' Thus, every value in this data frame is the contribution of inequality in
#' income earned from i-th source by members of j-th population cohort to
#' overall income inequality.
#'
#' Remember that overall SCV, which is the sum of all values in the data
#' frame, is always positive. However, some components of the coefficient can
#' have negative contribution to inequality.
#'
#' If all members of the studied population earn the same income, SCV normally
#' must be equal to zero. But, scv.2d calculates "alpha," which is the absolute
#' contribution of the given income source to overall inequality. Its
#' calculation is impossible for identical incomes because the formula involves
#' division by variance of income, which is zero in this case. Thus, the
#' function will return the NaN value.
#'
#' @references
#' Garcia-Penalosa, C., & Orgiazzi, E. (2013). Factor Components of Inequality:
#' A Cross-Country Study. Review of Income and Wealth, 59(4), 689-727.
#'
#' @export
#'
#' @importFrom stats complete.cases weighted.mean cov.wt
#'
#' @examples
#' \dontrun{
#' # Load the test data set.
#' data("us16")
#'
#' # No decomposition, just SCV of total income.
#' result <- scv.2d(us16, "hitotal", weights = "hpopwgt")
#'
#' # Decomposition of income inequality by gender.
#' result <- scv.2d(us16, "hitotal", "sex", "hitotal", "hpopwgt")
#'
#' # Decomposition of income inequality by gender and income source.
#' result <- scv.2d(us16, "hitotal", "sex", c("hilabour", "hicapital",
#' "hitransfer"), "hpopwgt")
#' }
scv.2d <- function(data, total, feature = NULL, sources = NULL,
                   weights = NULL, perc = FALSE){

  # Create population weights if they are not provided.
  if (is.null(weights)) {
    weights <- paste0(total, ".weights")
    data[weights] <- rep(1, nrow(data))
  }

  # Create income source if none is provided.
  if (is.null(sources)){
    sources <- total
  }

  # Create feature if none is provided.
  if (is.null(feature)){
    feature <- paste0(total, ".all")
    data[feature] <- rep("all", nrow(data))
  }

  # Remove unnecessary data.
  data <- data[, c(feature, total, sources, weights)]

  # Remove rows that have missing values.
  data <- data[complete.cases(data),]

  # Stop executing the function if at least one weight is either
  # zero or negative.
  if (!all(data[, weights] > 0)) {
    stop("At least one weight is nonpositive!", call. = FALSE)
  }

  # Identify unique population groups.
  groups <- unique(data[, feature])

  # Create data frame to store the output.
  out <- data.frame(matrix(ncol = 2 * length(groups) + 1,
                           nrow = length(sources)))
  colnames(out) <- c("source", paste0(groups, ".W"), paste0(groups, ".B"))
  out$source <- sources

  # Normalize population weights.
  ovWgt <- data[, weights]
  ovWgt <- ovWgt / sum(ovWgt)

  # Calculation of SCV's components.
  for (i in sources){

    # Share of inequality attributed to a given income source (alpha).
    # Weighted mean and variance of total income.
    tMean <- weighted.mean(data[, total], ovWgt)
    tVar <- (1 / sum(ovWgt)) * sum(ovWgt * (data[, total] - tMean)^2)

    # SCV of total income.
    tSCV <- tVar / (2 * tMean^2)

    # Weighted mean and average of the total population's income
    # from i-th source.
    iMean <- weighted.mean(data[, i], ovWgt)
    iVar <- (1 / sum(ovWgt)) * sum(ovWgt * (data[, i] - iMean)^2)

    # SCV of the given income source.
    iSCV <- iVar / (2 * iMean^2)

    # Correlation between the income source and total income.
    corrMat <- cov.wt(data[, c(i, total)], ovWgt, cor = TRUE)
    corrL <- corrMat$cor[2]

    # Absolute contribution of the given income source to overall inequality.
    alpha <- corrL * (iMean / tMean) * (tSCV * iSCV)^(1 / 2) / iSCV

    for (j in groups){

      # Create a vector of incomes of j-th population group.
      pgr <- data[data[, feature] == j,]

      # Normalize j-th group's population weights.
      gWgt <- pgr[, weights]
      gWgt <- gWgt / sum(gWgt)

      # Weighted average of j-th group's income from i-th source.
      grMean <- weighted.mean(pgr[, i], gWgt)

      # Weighted variance of j-th group's income from i-th source.
      grVar <- (1 / sum(gWgt)) * sum(gWgt * (pgr[, i] - grMean)^2)

      # Within-group component of SCV for i-th income source
      # of j-th population group.
      num <- alpha * (sum(pgr[, weights]) / sum(data[, weights])) *
        (grMean / iMean)^2 * grVar / (2 * grMean^2)
      out[out$source == i, paste0(j, ".W")] <- num

      # Between-group component of SCV for i-th income source
      # of j-th population group.
      num <- alpha * 0.5 * (sum(pgr[, weights]) / sum(data[, weights])) *
        ((grMean / iMean)^2 - 1)
      out[out$source == i, paste0(j, ".B")] <- num
    }
  }

  # If no feature is specified, the function will create two columns: all.W and
  # all.B, making sure that the loop still functions in this case. The all.B
  # column, however, will contain zeroes in this situation,
  # so it can be deleted.
  if (length(unique(data[, feature])) == 1){
    out <- out[, -3]
  }

  # Calculation of percentages.
  if (perc == TRUE){
    out[, -1] <- out[, -1] / sum(out[, -1]) * 100
  }
  return(out)
}
