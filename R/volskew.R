#' Volatility Skew Visualization
#'
#' Creates a 3D Plot of the Volatility Skew
#'
#' @param symbol Ticker symbol for a publicly traded company
#' @param type "call" or "put" option volatility skew
#' @param spot This is the current price of the stock, if blank it defaults to the previous adjusted close
#' @param r risk-free rate, annualized and continuously-compounded
#' @param ll lower limit, set to 0.75
#' @param ul upper limit, set to 1.25
#' @param days_out keep at 50
#' @param d1 dividend yield
#' @param d2 dividend yield
#' @param d3 dividend yield
#' @param d4 dividend yield
#' @param d5 dividend yield
#'
#' @return Plots a 3d Graph in the Viewer tab
#' @export
#'
#' @importFrom plotly plot_ly
#' @importFrom plotly add_surface
#' @importFrom plotly layout
#' @importFrom dplyr inner_join
#' @importFrom dplyr filter
#' @importFrom quantmod getOptionChain
#' @importFrom quantmod getSymbols
#' @importFrom optionstrat iv.calc
#' @importFrom utils tail
#'
#'
#' @examples volskew("^SPX", type = "call", spot = 2900, r = 0.02)
volskew <- function(symbol, type = "call", spot = "current", r, ll = 0.75, ul = 1.25,
                    days_out = 50, d1 = 0, d2 = 0, d3 = 0, d4 = 0, d5 = 0){

  today <- Sys.Date()

  if(spot == "current"){
    if(symbol == "^SPX"){
      symbol2 <- "^GSPC"
    } else {
      symbol2 <- symbol
    }

    current <- getSymbols(symbol2, from = today - 7, auto.assign = FALSE)
    current <- tail(current[,6], 1)
    current <- as.numeric(current)
    spot <- current
  }else{
    spot <- spot
  }


  available <- avoc(symbol)
  aoc_df <- data.frame(aoc = available)


  aoc_df$remain <- as.numeric(aoc_df$aoc) - as.numeric(today)
  aoc_df <- aoc_df[aoc_df[,2]<= days_out, c(1,2)]
  #aoc_df <- filter(aoc_df, remain <= days_out)

  aoc_df$a <- abs(aoc_df$remain + 0.25 - 5)
  aoc_df$b <- abs(aoc_df$remain  + 0.25 - 15)
  aoc_df$c <- abs(aoc_df$remain  + 0.25 - 25)
  aoc_df$d <- abs(aoc_df$remain  + 0.25 - 35)
  aoc_df$e <- abs(aoc_df$remain  + 0.25 - 45)


  aoc_a <- aoc_df[match(min(aoc_df$a),aoc_df$a),]
  aoc_b <- aoc_df[match(min(aoc_df$b),aoc_df$b),]
  aoc_c <- aoc_df[match(min(aoc_df$c),aoc_df$c),]
  aoc_d <- aoc_df[match(min(aoc_df$d),aoc_df$d),]
  aoc_e <- aoc_df[match(min(aoc_df$e),aoc_df$e),]


  aoc_df <- rbind(aoc_a[,c(1,2)],aoc_b[,c(1,2)],aoc_c[,c(1,2)],aoc_d[,c(1,2)],aoc_e[,c(1,2)])

  exp1 <- aoc_df$remain[1]/365
  exp2 <- aoc_df$remain[2]/365
  exp3 <- aoc_df$remain[3]/365
  exp4 <- aoc_df$remain[4]/365
  exp5 <- aoc_df$remain[5]/365

  chain1 <- getOptionChain(symbol, aoc_df$aoc[1])
  chain2 <- getOptionChain(symbol, aoc_df$aoc[2])
  chain3 <- getOptionChain(symbol, aoc_df$aoc[3])
  chain4 <- getOptionChain(symbol, aoc_df$aoc[4])
  chain5 <- getOptionChain(symbol, aoc_df$aoc[5])

  chain1c <- chain1$calls[,c(1,2)]
  chain2c <- chain2$calls[,c(1,2)]
  chain3c <- chain3$calls[,c(1,2)]
  chain4c <- chain4$calls[,c(1,2)]
  chain5c <- chain5$calls[,c(1,2)]

  chain1p <- chain1$puts[,c(1,2)]
  chain2p <- chain2$puts[,c(1,2)]
  chain3p <- chain3$puts[,c(1,2)]
  chain4p <- chain4$puts[,c(1,2)]
  chain5p <- chain5$puts[,c(1,2)]

  cc_12 <- inner_join(chain1c, chain2c, by = "Strike")
  cc_13 <- inner_join(cc_12, chain3c, by = "Strike")
  cc_14 <- inner_join(cc_13, chain4c, by = "Strike")
  cc_15 <- inner_join(cc_14, chain5c, by = "Strike")
  cc_15 <- cc_15[cc_15$Strike>= (ll * spot) & cc_15$Strike <= (ul * spot), ]
  #cc_15 <- filter(cc_15, Strike >= ll*spot & Strike <= ul*spot)

  cp_12 <- inner_join(chain1p, chain2p, by = "Strike")
  cp_13 <- inner_join(cp_12, chain3p, by = "Strike")
  cp_14 <- inner_join(cp_13, chain4p, by = "Strike")
  cp_15 <- inner_join(cp_14, chain5p, by = "Strike")
  cp_15 <- cp_15[cp_15$Strike>= (ll * spot) & cp_15$Strike <= (ul * spot), ]
  #cp_15 <- filter(cp_15, Strike >= ll*spot & Strike <= ul*spot)

  cc_a <- rep(0, nrow(cc_15))
  for(i in 1:nrow(cc_15)){
    cc_a[i] <- iv.calc(type = "call", price = cc_15[i,2], s= spot, x = cc_15[i,1],
                       t= exp1, r = r, d = d1)
  }

  cc_b <- rep(0, nrow(cc_15))
  for(i in 1:nrow(cc_15)){
    cc_b[i] <- iv.calc(type = "call", price = cc_15[i,3], s= spot, x = cc_15[i,1],
                       t= exp2, r = r, d = d2)
  }

  cc_c <- rep(0, nrow(cc_15))
  for(i in 1:nrow(cc_15)){
    cc_c[i] <- iv.calc(type = "call", price = cc_15[i,4], s= spot, x = cc_15[i,1],
                       t= exp3, r = r, d = d3)
  }

  cc_d <- rep(0, nrow(cc_15))
  for(i in 1:nrow(cc_15)){
    cc_d[i] <- iv.calc(type = "call", price = cc_15[i,5], s= spot, x = cc_15[i,1],
                       t= exp4, r = r, d = d4)
  }

  cc_e <- rep(0, nrow(cc_15))
  for(i in 1:nrow(cc_15)){
    cc_e[i] <- iv.calc(type = "call", price = cc_15[i,6], s= spot, x = cc_15[i,1],
                       t= exp5, r = r, d = d5)
  }




  cp_a <- rep(0, nrow(cp_15))
  for(i in 1:nrow(cp_15)){
    cp_a[i] <- iv.calc(type = "put", price = cp_15[i,2], s= spot, x = cp_15[i,1],
                       t= exp1, r = r, d = d1)
  }

  cp_b <- rep(0, nrow(cp_15))
  for(i in 1:nrow(cp_15)){
    cp_b[i] <- iv.calc(type = "put", price = cp_15[i,3], s= spot, x = cp_15[i,1],
                       t= exp2, r = r, d = d2)
  }

  cp_c <- rep(0, nrow(cp_15))
  for(i in 1:nrow(cp_15)){
    cp_c[i] <- iv.calc(type = "put", price = cp_15[i,4], s= spot, x = cp_15[i,1],
                       t= exp3, r = r, d = d3)
  }

  cp_d <- rep(0, nrow(cp_15))
  for(i in 1:nrow(cp_15)){
    cp_d[i] <- iv.calc(type = "put", price = cp_15[i,5], s= spot, x = cp_15[i,1],
                       t= exp4, r = r, d = d4)
  }

  cp_e <- rep(0, nrow(cp_15))
  for(i in 1:nrow(cp_15)){
    cp_e[i] <- iv.calc(type = "put", price = cp_15[i,6], s= spot, x = cp_15[i,1],
                       t= exp5, r = r, d = d5)
  }

  cc <- cbind(cc_a, cc_b, cc_c, cc_d, cc_e)
  cp <- cbind(cp_a, cp_b, cp_c, cp_d, cp_e)



    if(type == "call"){
      layout(add_surface(plot_ly(showscale = FALSE, z = ~cc)),
             title = "Volatility Skew",
             scene = list(
               xaxis = list(title = "Time to Expiration", ticketmode = 'array',
                            ticktext = c(aoc_df$remain[1],aoc_df$remain[2],aoc_df$remain[3],
                                         aoc_df$remain[4],aoc_df$remain[5]),
                            tickvals = c(0,1,2,3,4)),
               yaxis = list(title = "Strike Price",ticketmode = 'array',
                            ticktext = cc_15$Strike,
                            tickvals = c(0:nrow(cc_15)),
                            size = 5),
               zaxis = list(title = "Implied Volatility")
             ))


    }else{
      layout(add_surface(plot_ly(showscale = FALSE, z = ~cp)),
             title = paste("Volatility Skew",type, "option"),
             scene = list(
               xaxis = list(title = "Time to Expiration", ticketmode = 'array',
                            ticktext = c(aoc_df$remain[1],aoc_df$remain[2],aoc_df$remain[3],
                                         aoc_df$remain[4],aoc_df$remain[5]),
                            tickvals = c(0,1,2,3,4)),
               yaxis = list(title = "Strike Price",ticketmode = 'array',
                            ticktext = cp_15$Strike,
                            tickvals = c(0:nrow(cp_15)),
                            size = 5),
               zaxis = list(title = "Implied Volatility")
             ))



    }

}
