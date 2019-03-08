#' Visualize Option Parameter
#'
#' Creates a 3D Plot of an Option Parameter over time
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
#' @param e1 Expiration, in years. Set to 45/365
#' @param e2 Expiration, in years. Set to 30/365
#' @param e3 Expiration, in years. Set to 15/365
#' @param e4 Expiration, in years. Set to 1/365
#' @param c1 Option 1, Number of Contracts
#' @param c2 Option 2, Number of Contracts
#' @param c3 Option 3, Number of Contracts
#' @param c4 Option 4, Number of Contracts
#'
#' @return Returns a 3d Graph in the Viewer tab
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
visualize <- function(type, parameter, s, si, x1, x2, x3, x4, v1, v2 = v1, v3 = v1, v4 = v1,
                      ti,
                      r = 0.02, d = 0, ls = 1, low = 75, high = 125,
                      e1 =(45/365), e2 = (30/365), e3 = (15/365), e4 = (1/365),
                      c1 = 1, c2 = 1, c3 = 1, c4 = 1){



  neo1 <- data.frame(spot = c(1:high))
  neo1$vis <- rep(1, nrow(neo1))
  for(i in low:nrow(neo1)){

    neo1$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo1$spot[i],
                          x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                          v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                          t1 = e1, ti = ti, r = r, d = 0, ls = ls)
  }

  neo2 <- data.frame(spot = c(1:high))
  neo2$vis <- rep(1, nrow(neo2))
  for(i in low:nrow(neo2)){

    neo2$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo2$spot[i],
                          x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                          v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                          t1 = e2, ti =ti, r = r, d = 0, ls = ls)
  }

  neo3 <- data.frame(spot = c(1:high))
  neo3$vis <- rep(1, nrow(neo3))
  for(i in low:nrow(neo3)){

    neo3$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo3$spot[i],
                          x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                          v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                          t1 = e3, ti = ti, r = r, d = 0, ls = ls)
  }

  neo4 <- data.frame(spot = c(1:high))
  neo4$vis <- rep(1, nrow(neo4))
  for(i in low:nrow(neo4)){

    neo4$vis[i] <- visual(type = type, parameter = parameter, si = si, s = neo4$spot[i],
                          x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                          v1 = v1, v2 = v2, v3 = v3, v4 = v4,
                          t1 = e4, ti =ti, r = r, d = 0, ls = ls)
  }


  neo <- cbind(neo1[,2], neo2[,2], neo3[,2], neo4[,2])



   layout(add_surface(plot_ly(z = ~neo)),
    title = paste("Option Parameter:", type, parameter),
    scene = list(
      xaxis = list(title = "Time to Expiration"),
      yaxis = list(title = "Underlying Price", range = c(low,high)),
      zaxis = list(title = "Parameter Value")
    ))

  #print(cbind(neo1, neo2, neo3, neo4))
}
