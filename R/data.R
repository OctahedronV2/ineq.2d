#' Test dataset for the ineq.2d package
#'
#' This data set is a combination of household and personal-level data sets
#' available as sample files on the Luxembourg Income Study website. The data
#' sets were combined to assign personal characteristics of household heads to
#' households.
#'
#' Thus, the combined data set contains income data on 1000 households. This
#' includes three income sources and value for total income. Additionally,
#' contains several features of every household head: sex, education, and age.
#'
#' @format A data frame with 1000 rows and 8 variables:
#'
#' \describe{
#'    \item{hitotal}{Total income, sum of hitransfer, hilabour, and hicapital.}
#'    \item{hitransfer}{Transfer income.}
#'    \item{hilabour}{Labor income.}
#'    \item{hicapital}{Capital income.}
#'    \item{hpopwgt}{Population weights.}
#'    \item{age}{Age of the household head.}
#'    \item{sex}{Sex of the household head.}
#'    \item{educ}{Education level of the household head.}
#' }
#'
#' @source {LIS Cross-national Data Center,
#' https://www.lisdatacenter.org/resources/self-teaching/
#' }
#'
#' @examples
#' # To load the dataset to the environment, use the following code:
#' data(us16)
"us16"
