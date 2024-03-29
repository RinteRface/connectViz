% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers-data.R
\name{sort_users_by_role}
\alias{sort_users_by_role}
\title{Sort RStudio Connect users by role}
\usage{
sort_users_by_role(users, start_date = NULL, end_date = NULL)
}
\arguments{
\item{users}{Get from \link[connectapi]{get_users}.}

\item{start_date}{Date filter.}

\item{end_date}{Date filter.}
}
\value{
A tibble with user grouped by role.
}
\description{
Users are grouped by user_role to check the
server role repartition. Using start_date and end_date allows
to filter data given a specific time range.
}
