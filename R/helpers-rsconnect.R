#' Create a connection to RStudio Connect server
#'
#' Leverages connectapi toolkit. Expect to have \code{Sys.getenv("CONNECT_SERVER")} and
#' \code{Sys.getenv("CONNECT_API_KEY")} properly setup.
#'
#' @export
create_rsc_client <- function() {
  connectapi::connect(
    server = Sys.getenv("CONNECT_SERVER"),
    api_key = Sys.getenv("CONNECT_API_KEY")
  )
}
