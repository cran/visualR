#' Option Parameter Visualization
#'
#' Creates a 3D Plot of an Option Parameter plotted over time
#'
#' @param type Character String: "call" or "put"
#' @param parameter Character String: "premium", "delta", "gamma", "vega", "theta", "rho"
#' @param s Underlying Asset Price
#' @param si Initial Price of the underlying asset
#' @param x1 Option 1 Strike
#' @param x2 Option 2 Strike
#' @param x3 Option 3 Strike
#' @param x4 Option 4 Strike
#' @param v1 Option 1 Volatility
#' @param v2 Option 2 Volatility
#' @param v3 Option 3 Volatility
#' @param v4 Option 4 Volatility
#' @param ti Initial time to maturity in years
#' @param r Annualized continuously compounded risk-free rate
#' @param d Annualized continuously compounded dividend yield
#' @param ls Numerical either 1 or -1
#' @param low Lower Limit for the price range
#' @param high Upper Limit for the price range
#' @param e1 Expiration in years, set to 0
#' @param e2 Expiration in years, set to 5/365
#' @param e3 Expiration in years, set to 10/365
#' @param e4 Expiration in years, set to 15/365
#' @param e5 Expiration in years, set to 20/365
#' @param e6 Expiration in years, set to 25/365
#' @param e7 Expiration in years, set to 30/365
#' @param e8 Expiration in years, set to 35/365
#' @param e9 Expiration in years, set to 40/365
#' @param e10 Expiration in years, set to 45/365
#' @param c1 Option 1, Number of Contracts
#' @param c2 Option 2, Number of Contracts
#' @param c3 Option 3, Number of Contracts
#' @param c4 Option 4, Number of Contracts
#'
#' @return Plots a 3d Graph in the Viewer tab
#' @export
#'
#' @importFrom plotly plot_ly
#' @importFrom plotly add_surface
#' @importFrom plotly layout
#'
#' @examples visualize(type = "put", parameter = "delta", s = 100, si = 100,
#'     x1 = 90, x2 = 95, x3 = 105, x4 = 110, v1 = 0.20,
#'     ti = 45/365, r = 0.02, d = 0, ls = 1,
#'     low = 75, high = 125, e1 =(45/365), e2 = (30/365),
#'     e3 = (15/365), e4 = (1/365), c1 = 1, c2 = 1, c3 = 1, c4 = 1)
#'
#'
visualize <- function(type = "call", parameter = "premium", s = 100, si = 100, x1 = 90, x2 = 95, x3 = 105, x4 = 110, v1 = 0.2, v2 = v1, v3 = v1, v4 = v1,
                   ti = 45/365,
                   r = 0.02, d = 0, ls = 1, low = 75, high = 125,
                   e1 =(0), e2 = (5/365), e3 = (10/365), e4 = (15/365),
                   e5 =(20/365), e6 = (25/365), e7 = (30/365), e8 = (35/365),
                   e9 = (40/365), e10 = (45/365),
                   c1 = 1, c2 = 1, c3 = 1, c4 = 1){



  neo1 <- data.frame(spot = c(low:high))
  neo1$vis <- rep(1, (high - low + 1))
  for(i in 1:nrow(neo1)){

    neo1$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo1$spot[i],
                          x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                          v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                          t1 = e1, ti = ti, r = r, d = 0, ls = ls)
  }

  neo2 <- data.frame(spot = c(low:high))
  neo2$vis <- rep(2, (high - low + 1))
  for(i in 1:nrow(neo2)){

    neo2$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo2$spot[i],
                          x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                          v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                          t1 = e2, ti =ti, r = r, d = 0, ls = ls)
  }

  neo3 <- data.frame(spot = c(low:high))
  neo3$vis <- rep(3, (high - low + 1))
  for(i in 1:nrow(neo3)){

    neo3$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo3$spot[i],
                          x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                          v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                          t1 = e3, ti = ti, r = r, d = 0, ls = ls)
  }

  neo4 <- data.frame(spot = c(low:high))
  neo4$vis <- rep(4, (high - low + 1))
  for(i in 1:nrow(neo4)){

    neo4$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo4$spot[i],
                          x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                          v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                          t1 = e4, ti =ti, r = r, d = 0, ls = ls)
  }

  neo5 <- data.frame(spot = c(low:high))
  neo5$vis <- rep(5, (high - low + 1))
  for(i in 1:nrow(neo5)){

    neo5$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo4$spot[i],
                          x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                          v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                          t1 = e5, ti =ti, r = r, d = 0, ls = ls)
  }

  neo6 <- data.frame(spot = c(low:high))
  neo6$vis <- rep(6, (high - low + 1))
  for(i in 1:nrow(neo6)){

    neo6$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo4$spot[i],
                          x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                          v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                          t1 = e6, ti =ti, r = r, d = 0, ls = ls)
  }

  neo7 <- data.frame(spot = c(low:high))
  neo7$vis <- rep(7, (high - low + 1))
  for(i in 1:nrow(neo7)){

    neo7$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo4$spot[i],
                          x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                          v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                          t1 = e7, ti =ti, r = r, d = 0, ls = ls)
  }

  neo8 <- data.frame(spot = c(low:high))
  neo8$vis <- rep(8, (high - low + 1))
  for(i in 1:nrow(neo8)){

    neo8$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo4$spot[i],
                          x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                          v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                          t1 = e8, ti =ti, r = r, d = 0, ls = ls)
  }

  neo9 <- data.frame(spot = c(low:high))
  neo9$vis <- rep(9, (high - low + 1))
  for(i in 1:nrow(neo9)){

    neo9$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo4$spot[i],
                          x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                          v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                          t1 = e9, ti =ti, r = r, d = 0, ls = ls)
  }

  neo10 <- data.frame(spot = c(low:high))
  neo10$vis <- rep(10, (high - low + 1))
  for(i in 1:nrow(neo10)){

    neo10$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo4$spot[i],
                           x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                           v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                           t1 = e10, ti =ti, r = r, d = 0, ls = ls)
  }

  neo <- cbind(neo1[,2], neo2[,2], neo3[,2], neo4[,2], neo5[,2], neo6[,2], neo7[,2], neo8[,2], neo9[,2], neo10[,2])
  colnames(neo) <- c(e1*365, e2*365, e3*365, e4*365, e5*365, e6*365, e7*365, e8*365, e9 * 365, e10* 365)


  layout(add_surface(plot_ly(showscale = FALSE, z = ~neo)),
         title = paste("Option Parameter:", type, parameter),
         scene = list(
           xaxis = list(title = "Time to Expiration", ticketmode = 'array',
                        ticktext = c(e1*365, e2*365, e3*365, e4*365, e5*365, e6*365, e7*365, e8*365, e9 * 365, e10* 365),
                        tickvals = c(0,1,2,3,4,5,6,7,8,9)),
           yaxis = list(title = "Underlying Price", ticketmode = 'array',
                        ticktext = c(low:high),
                        tickvals = 0:nrow(neo)),
           zaxis = list(title = "Parameter Value")
         ))


}
