#' Create a connection to RStudio Connect server
#'
#' Leverages connectapi toolkit. Expect to have \code{Sys.getenv("CONNECT_SERVER")} and
#' \code{Sys.getenv("CONNECT_API_KEY")} properly setup.
#'
#' @export
create_rsc_client <- function() {

  if (Sys.getenv("CONNECT_SERVER") == "" ||
      Sys.getenv("CONNECT_API_KEY") == "") {
    stop("Make sure to setup CONNECT_SERVER and CONNECT_API_KEY environment variables
         with correct values. See package readme in prerequisites section.")
  }

  connectapi::connect(
    server = Sys.getenv("CONNECT_SERVER"),
    api_key = Sys.getenv("CONNECT_API_KEY")
  )
}
