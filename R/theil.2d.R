#' Function performing two-dimensional decomposition of the Theil index.
#'
#' @param data Data frame containing income data.
#' @param total String specifying the name of the column containing data on
#' total income.
#' @param feature String specifying the name of the column containing
#' information about the feature used for inequality decomposition. If left
#' blank, total income is not decomposed by feature.
#' @param sources Vector containing strings specifying the names of the columns
#' with data on income sources, the sum of which must be equal to total income.
#' The user can specify the same value as in "total." If left blank, total
#' income is not decomposed by income source.
#' @param weights String specifying the name of the column containing population
#' weights.
#'
#' @description
#'
#' @return Data frame containing values of components of the Theil index.
#' Every column represents a feature, while every row represents an income
#' source. Thus, every value in this data frame represents the contribution of
#' income inequality in i-th income source among population members
#' possessing j-th feature to total income inequality. These values are
#' calculated using the formulas suggested in Giammatteo (2007).
#'
#' @references
#' Giammatteo, M. (2007). The Bidimensional Decomposition of Inequality:
#' A nested Theil Approach. LIS Working papers, Article 466, 1-30.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Load the test data set.
#' data("us16")
#'
#' No decomposition, just Theil index of total income.
#' result <- theil.2d(us16, "hitotal", weights = "hpopwgt")
#'
#' Decomposition of income inequality by gender.
#' result <- theil.2d(us16, "hitotal", "sex", "hitotal", "hpopwgt")
#'
#' Decomposition of income inequality by gender and income source.
#' result <- theil.2d(us16, "hitotal", "sex", c("hilabour", "hicapital",
#' "hitransfer"), "hpopwgt")
#' }
theil.2d <- function(data, total, feature = NULL, sources = NULL,
                     weights = NULL){

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

  # Remove all values of total income that are less than or equal to zero.
  data <- data[data[, total] > 0,]

  # Identify unique population groups.
  groups <- unique(data[, feature])

  # Create data frame to store the output.
  out <- data.frame(matrix(ncol = 2 * length(groups) + 1, nrow = length(sources)))
  colnames(out) <- c("source", paste0(groups, ".W"), paste0(groups, ".B"))
  out$source <- sources

  # Normalize population weights.
  ovWgt <- data[, weights]
  ovWgt <- ovWgt / sum(ovWgt)

  # Weighted average income of the entire population.
  ovMean <- weighted.mean(data[, total], ovWgt)

  # Calculation of Theil's index components.
  for (i in sources){
    for (j in groups){

      # Create a vector of incomes of j-th population group.
      pgr <- data[data[, feature] == j,]

      # Normalize j-th group's population weights.
      gWgt <- pgr[, weights]
      gWgt <- gWgt / sum(gWgt)

      # Weighted average income of j-th population group.
      grMean <- weighted.mean(pgr[, total], gWgt)

      # Ratios necessary to calculate the Theil index
      # for i-th income source of j-th population group.
      ratio1 <- pgr[, i] / grMean
      ratio2 <- pgr[, total] / grMean

      # Within-group component of the Theil index for i-th income source
      # of j-th population group.
      num <- (sum(pgr[, weights]) / sum(data[, weights])) *
        (grMean / ovMean) * sum(gWgt * ratio1 * log(ratio2))
      out[out$source == i, paste0(j, ".W")] <- num

      # Weighted average income from i-th source of j-th population group.
      igrMean <- weighted.mean(pgr[, i], gWgt)

      # Between-group component of the Theil index for i-th income source
      # of j-th population group.
      num <- (sum(pgr[, weights]) / sum(data[, weights])) *
        (igrMean / ovMean) * log(grMean / ovMean)
      out[out$source == i, paste0(j, ".B")] <- num
    }
  }
  return(out)
}
