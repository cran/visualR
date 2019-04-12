#' Visual Auxiliary Function
#'
#' Calculates the net parameter value for a double vertical option spread
#'
#' @param type Character String: "call" or "put"
#' @param parameter Character String: "premium", "delta", "gamma", "vega", "theta", "rho"
#' @param s Spot Price
#' @param si Initial Spot Price
#' @param x1 Option 1 Strike
#' @param x2 Option 2 Strike
#' @param x3 Option 3 Strike
#' @param x4 Option 4 Strike
#' @param v1 Option 1 Volatility
#' @param v2 Option 2 Volatility
#' @param v3 Option 3 Volatility
#' @param v4 Option 4 Volatility
#' @param ti Initial Years to Maturity
#' @param t1 Option 1 years to maturity
#' @param t2 Option 1 years to maturity
#' @param t3 Option 1 years to maturity
#' @param t4 Option 1 years to maturity
#' @param r Annualized continuously compounded risk-free rate
#' @param d Annualized continuously compounded dividend yield
#' @param ls Numerical either 1 or -1
#'
#' @return Returns a Numerical value
#' @export
#'
#' @importFrom optionstrat opteval
#' @importFrom optionstrat callpremium
#'
#' @examples visual(type = "call", parameter = "premium", s = 100, si = 100, x1 = 90,
#' x2 = 95, x3 = 105, x4 = 110, v1 = 0.20, ti = 45/365, t1 = 45/365)
#'
visual <- function(type, parameter, s, si, x1, x2, x3, x4, v1, v2 = v1, v3 = v1, v4 = v1,
                   ti, t1, t2 = t1, t3 = t1, t4 = t1,
                   r = 0.02, d = 0, ls = 1){

  code <- 0
  if(parameter == "premium"){
    code <- 1
  } else{
    code <- 0
  }

  trow <- 1
  tcol <- 1
  if(type == "call"){
    trow <- 1} else{ trow <- 2}

  if(parameter == "premium"){ tcol <- 2
  } else if(parameter == "delta"){ tcol <- 3
  } else if(parameter == "gamma"){ tcol <- 4
  } else if(parameter == "vega") {tcol <- 5
  } else if(parameter == "theta") {tcol <- 6
  } else if(parameter == "rho") {tcol <- 7
  } else{ tcol <- 2}

  initial1 <- opteval(s = si, x = x1, sigma = v1, t = ti, r = r, d = d)
  initial2 <- opteval(s = si, x = x2, sigma = v2, t = ti, r = r, d = d)
  initial3 <- opteval(s = si, x = x3, sigma = v3, t = ti, r = r, d = d)
  initial4 <- opteval(s = si, x = x4, sigma = v4, t = ti, r = r, d = d)

  initial1 <- initial1[trow, tcol] * ls * code
  initial2 <- initial2[trow, tcol] * ls * -1 * code
  initial3 <- initial3[trow, tcol] * ls * -1 * code
  initial4 <- initial4[trow, tcol] * ls * code

  leg1 <- opteval(s = s, x = x1, sigma = v1, t = t1, r = r, d = d)
  leg2 <- opteval(s = s, x = x2, sigma = v2, t = t2, r = r, d = d)
  leg3 <- opteval(s = s, x = x3, sigma = v3, t = t3, r = r, d = d)
  leg4 <- opteval(s = s, x = x4, sigma = v4, t = t4, r = r, d = d)

  (leg1[trow, tcol]  * ls - initial1) +
    (leg2[trow, tcol] * ls * -1 - initial2) +
    (leg3[trow, tcol] * ls * -1 - initial3) +
    (leg4[trow, tcol] * ls - initial4)

}
