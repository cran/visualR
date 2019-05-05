#' Volatility Skew Visualization
#'
#' Creates a 3D Plot of the Volatility Skew
#'
#' @param symbol Ticker symbol for a publicly traded company
#' @param type "call" or "put" option volatility skew
#' @param spot This is the current price of the stock, if blank it defaults to the previous adjusted close
#' @param r risk-free rate, annualized and continuously-compounded
#' @param ll lower limit, set to 0.90
#' @param ul upper limit, set to 1.10
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
#' @importFrom dplyr left_join
#' @importFrom dplyr full_join
#' @importFrom quantmod getOptionChain
#' @importFrom quantmod getSymbols
#' @importFrom optionstrat iv.calc
#' @importFrom utils tail
#' @importFrom zoo na.locf
#'
#'
#' @examples volskew("^SPX", type = "call")
volskew <- function(symbol, type = "call", spot = "current", r = 0.02, ll = 0.90, ul = 1.10,
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

  #Filter by lower and upper Strike limit

  chain1c <- chain1c[chain1c$Strike>= (ll * spot) & chain1c$Strike <= (ul * spot), ]
  chain2c <- chain2c[chain2c$Strike>= (ll * spot) & chain2c$Strike <= (ul * spot), ]
  chain3c <- chain3c[chain3c$Strike>= (ll * spot) & chain3c$Strike <= (ul * spot), ]
  chain4c <- chain4c[chain4c$Strike>= (ll * spot) & chain4c$Strike <= (ul * spot), ]
  chain5c <- chain5c[chain5c$Strike>= (ll * spot) & chain5c$Strike <= (ul * spot), ]




  for(i in 1:nrow(chain1c)){
    chain1c$IV[i] <- iv.calc(type = "call", price = chain1c[i,2], s= spot, x = chain1c[i,1],
                             t= exp1, r = r, d = d1)
  }

  for(i in 1:nrow(chain2c)){
    chain2c$IV[i] <- iv.calc(type = "call", price = chain2c[i,2], s= spot, x = chain2c[i,1],
                             t= exp1, r = r, d = d1)
  }

  for(i in 1:nrow(chain3c)){
    chain3c$IV[i] <- iv.calc(type = "call", price = chain3c[i,2], s= spot, x = chain3c[i,1],
                             t= exp1, r = r, d = d1)
  }

  for(i in 1:nrow(chain4c)){
    chain4c$IV[i] <- iv.calc(type = "call", price = chain4c[i,2], s= spot, x = chain4c[i,1],
                             t= exp1, r = r, d = d1)
  }

  for(i in 1:nrow(chain5c)){
    chain5c$IV[i] <- iv.calc(type = "call", price = chain5c[i,2], s= spot, x = chain5c[i,1],
                             t= exp1, r = r, d = d1)
  }


  #change to rbind for Strikes
  Callmerge <- full_join(chain1c, full_join(chain2c, full_join(chain3c,
                                                               full_join(chain4c, chain5c, by = "Strike"), by = "Strike"), by = "Strike")
                         , by = "Strike")




  Callstrikes <- Callmerge[,1]
  Callstrikes <- data.frame(Strike = sort(unique(Callstrikes)))



  Calljoin <- left_join(Callstrikes, chain1c, by = "Strike")
  Calljoin <- left_join(Calljoin, chain2c, by = "Strike")
  Calljoin <- left_join(Calljoin, chain3c, by = "Strike")
  Calljoin <- left_join(Calljoin, chain4c, by = "Strike")
  Calljoin <- left_join(Calljoin, chain5c, by = "Strike")

  Calljoin[,3] <- na.locf(Calljoin[,3], na.rm = F)
  Calljoin[,3] <- na.locf(Calljoin[,3], na.rm = F, fromLast= TRUE)

  Calljoin[,5] <- na.locf(Calljoin[,5], na.rm = F)
  Calljoin[,5] <- na.locf(Calljoin[,5], na.rm = F, fromLast= TRUE)

  Calljoin[,7] <- na.locf(Calljoin[,7], na.rm = F)
  Calljoin[,7] <- na.locf(Calljoin[,7], na.rm = F, fromLast= TRUE)

  Calljoin[,9] <- na.locf(Calljoin[,9], na.rm = F)
  Calljoin[,9] <- na.locf(Calljoin[,9], na.rm = F, fromLast= TRUE)

  Calljoin[,11] <- na.locf(Calljoin[,11], na.rm = F)
  Calljoin[,11] <- na.locf(Calljoin[,11], na.rm = F, fromLast= TRUE)
  #temp1 <- na.locf(temp1, fromLast = TRUE)

  #print(Calljoin)

  #print(Calljoin)

  #iv.calc(s= 2900, x = 2865, price = 47.95, type = "call", r = 0.02, t = 5/365)

  #Puts

  chain1p <- chain1$puts[,c(1,2)]
  chain2p <- chain2$puts[,c(1,2)]
  chain3p <- chain3$puts[,c(1,2)]
  chain4p <- chain4$puts[,c(1,2)]
  chain5p <- chain5$puts[,c(1,2)]


  chain1p <- chain1p[chain1p$Strike>= (ll * spot) & chain1p$Strike <= (ul * spot), ]
  chain2p <- chain2p[chain2p$Strike>= (ll * spot) & chain2p$Strike <= (ul * spot), ]
  chain3p <- chain3p[chain3p$Strike>= (ll * spot) & chain3p$Strike <= (ul * spot), ]
  chain4p <- chain4p[chain4p$Strike>= (ll * spot) & chain4p$Strike <= (ul * spot), ]
  chain5p <- chain5p[chain5p$Strike>= (ll * spot) & chain5p$Strike <= (ul * spot), ]


  #IV calc



  for(i in 1:nrow(chain1p)){
    chain1p$IV[i] <- iv.calc(type = "put", price = chain1p[i,2], s= spot, x = chain1p[i,1],
                             t= exp1, r = r, d = d1)
  }

  for(i in 1:nrow(chain2p)){
    chain2p$IV[i] <- iv.calc(type = "put", price = chain2p[i,2], s= spot, x = chain2p[i,1],
                             t= exp1, r = r, d = d1)
  }

  for(i in 1:nrow(chain3p)){
    chain3p$IV[i] <- iv.calc(type = "put", price = chain3p[i,2], s= spot, x = chain3p[i,1],
                             t= exp1, r = r, d = d1)
  }

  for(i in 1:nrow(chain4p)){
    chain4p$IV[i] <- iv.calc(type = "put", price = chain4p[i,2], s= spot, x = chain4p[i,1],
                             t= exp1, r = r, d = d1)
  }

  for(i in 1:nrow(chain5p)){
    chain5p$IV[i] <- iv.calc(type = "put", price = chain5p[i,2], s= spot, x = chain5p[i,1],
                             t= exp1, r = r, d = d1)
  }


  #Strike Merge
  Putmerge <- full_join(chain1p, full_join(chain2p, full_join(chain3p,
                                                              full_join(chain4p, chain5p, by = "Strike"),
                                                              by = "Strike"), by = "Strike"), by = "Strike")

  Putstrikes <- Putmerge[,1]
  Putstrikes <- data.frame(Strike = sort(unique(Putstrikes)))


  Putjoin <- left_join(Putstrikes, chain1p, by = "Strike")
  Putjoin <- left_join(Putjoin, chain2p, by = "Strike")
  Putjoin <- left_join(Putjoin, chain3p, by = "Strike")
  Putjoin <- left_join(Putjoin, chain4p, by = "Strike")
  Putjoin <- left_join(Putjoin, chain5p, by = "Strike")

  Putjoin[,3] <- na.locf(Putjoin[,3], na.rm = F)
  Putjoin[,3] <- na.locf(Putjoin[,3], na.rm = F, fromLast= TRUE)

  Putjoin[,5] <- na.locf(Putjoin[,5], na.rm = F)
  Putjoin[,5] <- na.locf(Putjoin[,5], na.rm = F, fromLast= TRUE)

  Putjoin[,7] <- na.locf(Putjoin[,7], na.rm = F)
  Putjoin[,7] <- na.locf(Putjoin[,7], na.rm = F, fromLast= TRUE)

  Putjoin[,9] <- na.locf(Putjoin[,9], na.rm = F)
  Putjoin[,9] <- na.locf(Putjoin[,9], na.rm = F, fromLast= TRUE)

  Putjoin[,11] <- na.locf(Putjoin[,11], na.rm = F)
  Putjoin[,11] <- na.locf(Putjoin[,11], na.rm = F, fromLast= TRUE)




  cc <- cbind(Calljoin[,3],Calljoin[,5],Calljoin[,7],Calljoin[,9],Calljoin[,11])
  cp <- cbind(Putjoin[,3],Putjoin[,5],Putjoin[,7],Putjoin[,9],Putjoin[,11])



  if(type == "call"){
    layout(add_surface(plot_ly(showscale = FALSE, z = ~cc)),
           title = "Volatility Skew",
           scene = list(
             xaxis = list(title = "Time to Expiration", ticketmode = 'array',
                          ticktext = c(aoc_df$remain[1],aoc_df$remain[2],aoc_df$remain[3],
                                       aoc_df$remain[4],aoc_df$remain[5]),
                          tickvals = c(0,1,2,3,4)),
             yaxis = list(title = "Strike Price",ticketmode = 'array',
                          ticktext = Calljoin$Strike,
                          tickvals = c(0:nrow(Calljoin)),
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
                          ticktext = Putjoin$Strike,
                          tickvals = c(0:nrow(Putjoin)),
                          size = 5),
             zaxis = list(title = "Implied Volatility")
           ))



  }

}
