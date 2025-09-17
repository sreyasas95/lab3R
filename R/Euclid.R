#' Euclidean Algorithm for Greatest Common Divisor (GCD)
#'
#' This function implements the Euclidean algorithm to compute the greatest common divisor (GCD)
#' of two integers. The algorithm works by repeatedly dividing the larger number by the smaller number
#' and replacing the larger number with the remainder, until the remainder is zero. The last non-zero remainder
#' is the GCD.
#'
#' @param a A numeric value (integer), representing the first number.
#' @param b A numeric value (integer), representing the second number.
#'
#' @return A numeric value representing the greatest common divisor (GCD) of the two input numbers.
#' The result is an integer.
#'
#' @details
#' The function checks if both `a` and `b` are numeric values and then applies the Euclidean algorithm
#' to compute their greatest common divisor. If either of the inputs is not numeric, the function
#' throws an error.
#'
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(1000, 100)
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export


euclidean <- function(a,b){
  if(!is.numeric(a)||(!is.numeric(b))){
    stop("both the numbers should be a integer")
  }
  a <- as.numeric(a)
  b <- as.numeric(b)
  while (b!=0) {
    temp <- b
       b <- a%%b
       a <- temp
  }
  return(a)
}
euclidean(123612, 13892347912)
euclidean(1000,100)


