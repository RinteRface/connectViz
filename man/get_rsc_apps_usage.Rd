% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers-data.R
\name{get_rsc_apps_usage}
\alias{get_rsc_apps_usage}
\title{Get RSC apps usage}
\usage{
get_rsc_apps_usage(logs)
}
\arguments{
\item{logs}{Given by RSC database.}
}
\value{
A 3 columns tibble with app name, usage and date of usage (nested tibble).
}
\description{
Called on apps_usage data
}
