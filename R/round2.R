#'Round a number. There is this stupid issue with round() that rounds .5 down..floating point nonsense.
#'
#'@param x Number you'd like to round
#'@param digit Number of digits you'd like to round
#'@examples round2(x,2)
#'
#'@export round2
round2 <- function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}
