my_round <- function(x, n, type) {
  #' Round to nearest number
  #'
  #' Round up or down to nearest user specified number.
  #' @param x Vector of numbers to round.
  #' @param n Nearest number to which the x-value should be rounded.
  #' @param type A string (either 'ceiling' or 'floor') to indicate if the number should be rounded up or down.
  #' @return Returns a vector of rounded numbers.
  #' @keywords internal
  #' @export
  #' @examples
  #' my_round(24.9, 5, 'floor')
  #' my_round(c(24.9, 20.1, 25), 5, 'ceiling')
  #' @author Christoffer Rasmussen

  if (type == 'ceiling') {
    x.rounded <- ceiling(x / n) * n
  } else if (type == 'floor') {
    x.rounded <- floor(x / n) * n
  }

  return(x.rounded)
}
