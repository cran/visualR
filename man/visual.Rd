% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visual.R
\name{visual}
\alias{visual}
\title{Visual Auxiliary Function}
\usage{
visual(type, parameter, s, si, x1, x2, x3, x4, v1, v2 = v1, v3 = v1,
  v4 = v1, ti, t1, t2 = t1, t3 = t1, t4 = t1, r = 0.02, d = 0,
  ls = 1)
}
\arguments{
\item{type}{Character String: "call" or "put"}

\item{parameter}{Character String: "premium", "delta", "gamma", "vega", "theta", "rho"}

\item{s}{Spot Price}

\item{si}{Initial Spot Price}

\item{x1}{Option 1 Strike}

\item{x2}{Option 2 Strike}

\item{x3}{Option 3 Strike}

\item{x4}{Option 4 Strike}

\item{v1}{Option 1 Volatility}

\item{v2}{Option 2 Volatility}

\item{v3}{Option 3 Volatility}

\item{v4}{Option 4 Volatility}

\item{ti}{Initial Years to Maturity}

\item{t1}{Option 1 years to maturity}

\item{t2}{Option 1 years to maturity}

\item{t3}{Option 1 years to maturity}

\item{t4}{Option 1 years to maturity}

\item{r}{Annualized continuously compounded risk-free rate}

\item{d}{Annualized continuously compounded dividend yield}

\item{ls}{Numerical either 1 or -1}
}
\value{
Returns a Numerical value
}
\description{
Calculates the net parameter value for a double vertical option spread
}
\examples{
visual(type = "call", parameter = "premium", s = 100, si = 100, x1 = 90,
x2 = 95, x3 = 105, x4 = 110, v1 = 0.20, ti = 45/365, t1 = 45/365)

}
