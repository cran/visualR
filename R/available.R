#' Available Option Chain
#'
#' Finds all available option expirations
#'
#' @param symbol Stock Ticker
#'
#' @return Returns a vector of dates representing the expirations
#' @export
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples avoc("AAPL")
#'
avoc <- function(symbol){
  urlExp <- paste0("https://query2.finance.yahoo.com/v7/finance/options/", symbol)
  tbl <- fromJSON(urlExp)
  all.expiries <- tbl$optionChain$result$expirationDates[[1]]
  datecode <- function(date, reverse = FALSE) {

    if(reverse == FALSE){
      days <- as.numeric(as.Date(date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d")) - as.Date("1970-01-01", tryFormats = c("%Y-%m-%d", "%Y/%m/%d")))
      seconds <- days * 24 * 60 * 60
      print(seconds)

    }else{
      days <- date / 24 / 60 / 60


      as.Date(as.numeric(as.Date("1970-01-01")) + days, origin = "1970-01-01")

    }

  }
  datecode(all.expiries, reverse = TRUE)


}

