% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xplr.R
\name{peek_visdat}
\alias{peek_visdat}
\title{Peek into the data with visdat. If the table is too big you can set how to subset the data frame with \code{peek_mode}}
\usage{
peek_visdat(dat, max_rows = 1000, peek_mode = "head")
}
\arguments{
\item{dat}{Table to peek}

\item{max_rows}{Max rows to peek and visualize}

\item{peek_mode}{Peek mode if number of rows in \code{dat} \code{exceeds max_rows}. Must be one of \code{head}, \code{tail}, \code{middle}, \code{random}}
}
\description{
Peek into the data with visdat. If the table is too big you can set how to subset the data frame with \code{peek_mode}
}
