#' Generate htmlWidgets table
#'
#' Leverage toastui API.
#'
#' @param logs Obtained with `get_rsc_apps_usage`.
#' @param sparkline Whether to draw a sparkline.
#' @param pagination Control number of lines to display per page.
#' If NULL, all data are displayed.
#' @param height Table height. CSS units.
#'
#' @return An htmlwidget table.
#' @import toastui
#' @export
generate_table <- function(logs, sparkline = FALSE, pagination = 10, height = NULL) {

  # Prepare for additional metrics
  max_logs <- get_max_rsc_apps_usage(logs)
  threshold <- max_logs / 2
  domain <- c(0, max_logs)
  from <- c(0, get_max_rsc_apps_usage(logs))

  table <- datagrid(
    logs,
    filters = TRUE,
    pagination = pagination,
    height = height
  ) %>%
    grid_style_column(
      column = "n",
      background = scales::col_numeric(
        "Blues",
        domain = !!domain
      )(n),
      fontWeight = "bold",
      color = ifelse(n > !!threshold, "white", "black")
    ) %>%
    grid_summary(
      column = "n",
      stat = "avg",
      label = "Mean usage: ",
      position = "top"
    ) %>%
    grid_colorbar(
      column = "n",
      label_outside = TRUE,
      label_width = "30px",
      bar_bg = "#BF616A",
      from = from
    )

  if (sparkline) {
    create_table_sparkline(table)
  } else {
    table
  }

}



#' Create sparkline graph for app usage table
#'
#' This function may be used by \link{generate_table} internally.
#'
#' @param table Obtained after calling \link{generate_table}.
#'
#' @return Adds a sparkline bar chart to the existing table.
#' @keywords internal
#' @import apexcharter toastui dplyr
#' @importFrom rlang .data
create_table_sparkline <- function(table) {
  table %>%
    grid_sparkline(
      column = "calendar_data",
      renderer = function(data) {
        data$Month <- data$Date %>% purrr::map(~ {
          lubridate::month(..1, label = TRUE)
        }) %>%
          unlist() %>%
          as.character()

        data$Month_code <- data$Date %>% purrr::map_dbl(~ {
          lubridate::month(..1)
        })

        # gather by month since it is too laggy
        # for individual dates
        data <- data %>%
          group_by(.data$Month_code) %>%
          count(name = "Freq", wt = .data$Freq) %>%
          bind_cols(Month = data$Month %>% unique())

        freq_max <- max(data$Freq)

        data %>%
          apex(aes(.data$Month, .data$Freq), type = "column") %>%
          ax_chart(sparkline = list(enabled = TRUE)) %>%
          ax_yaxis(min = 0, max = freq_max)
      }
    )
}




#' Create app ranking table
#'
#' Leverages the toastui htmlwidget.
#' See \url{https://dreamrs.github.io/toastui/articles/extras/grid.html}.
#'
#' @param ranking Ranked data from \link{create_app_ranking}.
#' @param pagination Control number of lines to display per page.
#' If NULL, all data are displayed.
#' @param height Table height. CSS units.
#' @return An htmlwidget table containing app ranking, owner sorted by
#' usage.
#' @export
create_app_ranking_table <- function(ranking, pagination = 10, height = NULL) {
  ranking[[2]] %>% generate_table(sparkline = TRUE, pagination, height)
}
