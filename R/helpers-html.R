# #' Import bs4Dash dependencies
# #'
# #' @export
# #' @import bs4Dash
# useBs4Dash <- function () {
#   deps <- htmltools::findDependencies(
#     dashboardPage(
#       header = dashboardHeader(),
#       sidebar = dashboardSidebar(),
#       body = dashboardBody()
#     )
#   )
#   htmltools::attachDependencies(tags$div(), value = deps)
# }
