% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main_script.R
\name{call_api_multiple_times}
\alias{call_api_multiple_times}
\title{Call an API multiple times}
\usage{
call_api_multiple_times(
  iterator,
  URL,
  path,
  query,
  resultVector,
  use_parallel = TRUE
)
}
\arguments{
\item{iterator}{vector to iterate over}

\item{URL}{endpoint url}

\item{path}{the path}

\item{query}{the actual query (with list())}

\item{resultVector}{the vector to prune}

\item{use_parallel}{whether or not to use multiple workers}
}
\description{
Call an API multiple times
}
