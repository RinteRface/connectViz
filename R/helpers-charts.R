#' Generic calendar chart generator
#'
#' @param calendar_data Calendar chart data.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param start_date Default to minimum calendar_data date. Could also be
#' an input value with Shiny.
#' @param end_date Default to maximum calendar_data date. Could also be
#' an input value with Shiny.
#' @param callback JS function to pass to \link[htmlwidgets]{onRender}. This is
#' useful to access the widget API on the R side at render time
#' and add events on the fly.
#'
#' @return An echarts4r calendar chart
#' @export
#' @import echarts4r
#' @importFrom shiny validate need is.reactive
create_calendar_chart <- function(
  calendar_data,
  title,
  subtitle = NULL,
  start_date = NULL,
  end_date = NULL,
  callback = NULL
) {

  Date <- Freq <- NULL

  renderEcharts4r({
    validate(need(nrow(calendar_data()) > 0, "No calendar data found ..."))
    if (is.reactive(calendar_data)) calendar_data <- calendar_data()
    if (is.null(start_date)) start_date <- min(calendar_data$Date)
    if (is.null(end_date)) end_date <- max(calendar_data$Date)
    if (is.reactive(start_date)) start_date <- start_date()
    if (is.reactive(end_date)) end_date <- end_date()
    if (is.reactive(title)) title <- title()

    range <- c(start_date, end_date)
    max <- max(calendar_data$Freq)

    calendar_chart <- calendar_data %>%
      e_charts(Date) %>%
      e_calendar(range = range) %>%
      e_effect_scatter(Freq, coord_system = "calendar", legend = FALSE) %>%
      e_visual_map(
        max = max,
        inRange = list(
          color = c('#e0f3f8', '#abd9e9', '#74add1', '#4575b4', '#313695')
        ),
        show = FALSE
      ) %>%
      e_title(title, subtitle) %>%
      e_tooltip() %>%
      e_legend(show = FALSE)

    if (!is.null(callback)) {
      calendar_chart %>% htmlwidgets::onRender(callback)
    } else {
      calendar_chart
    }

  })
}



#' Daily app usage chart
#'
#' Leverages echarts4r.
#' See \url{https://echarts4r.john-coene.com/articles/chart_types.html#calendar-1}.
#'.
#' @param app_usage Returned by \link{get_app_daily_usage}.
#' @return A calendar chart displaying daily app usage.
#' @export
create_app_daily_usage_chart <- function(app_usage) {
  create_calendar_chart(app_usage, "Overall app usage")
}




#' Daily app consumption for selected user
#'
#' @param usage Get from \link{get_user_daily_consumption}.
#'
#' @return An echarts4r calendar chart
#' @export
create_user_daily_consumption_chart <- function(usage) {
  create_calendar_chart(usage, "Overall consumption")
}



#' Create cumulated app duration/user
#'
#' Bar chart
#'
#' @param apps_usage First element returned by \link{create_app_ranking}.
#' Can be reactive.
#' @param selected_app Selected app name (string). You'll need a selectInput for instance
#' wrapped by \link[shiny]{reactive}.
#'
#' @return An echarts4r barchart.
#' @export
#' @import echarts4r
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom shiny req is.reactive
create_cumulated_duration_per_user <- function(apps_usage, selected_app) {

  username <- cum_duration <- NULL

  renderEcharts4r({
    if (is.reactive(apps_usage)) apps_usage <- apps_usage()
    req(selected_app())
    apps_usage %>%
      filter(.data$app_name == !!selected_app()) %>%
      mutate(duration = as.numeric(.data$duration)) %>%
      group_by(.data$username) %>%
      summarise(cum_duration = round(sum(.data$duration) / 3600)) %>%
      arrange(.data$cum_duration) %>%
      tidyr::replace_na(list(username = "Unknown")) %>%
      e_charts(username) %>%
      e_bar(cum_duration) %>%
      e_flip_coords() %>%
      e_axis_labels(x = "Duration (hours)", y = "End user") %>%
      e_tooltip()
  })
}


#' Create cumulated app hits/user
#'
#' Bar chart
#'
#' @param apps_usage First element returned by \link{create_app_ranking}.
#' Can be reactive.
#' @param selected_app Selected app name (string). You'll need a selectInput for instance
#' wrapped by \link[shiny]{reactive}.
#'
#' @return An echarts4r barchart.
#' @export
#' @import echarts4r
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom shiny req is.reactive
create_cumulated_hits_per_user <- function(apps_usage, selected_app) {

  username <- NULL

  renderEcharts4r({
    if (is.reactive(apps_usage)) apps_usage <- apps_usage()
    req(selected_app())
    apps_usage %>%
      filter(.data$app_name == !!selected_app()) %>%
      group_by(.data$username) %>%
      summarise(n = n()) %>% # prefer summarize over sort to remove grouping
      arrange(n) %>%
      tidyr::replace_na(list(username = "Unknown")) %>%
      e_charts(username) %>%
      e_bar(n) %>%
      e_flip_coords() %>%
      e_axis_labels(x = "Number of hits (app visit)", y = "End user") %>%
      e_tooltip()
  })
}



#' Create developers ranking bar chart
#'
#' Devs are ranked by number of developed apps.
#' See \link{create_dev_ranking}.
#'
#' @param ranking Obtained after calling \link{create_dev_ranking}.
#' Can be reactive.
#' @param threshold Minimum number of app threshold. You'll need a numericInput
#' wrapped by \link[shiny]{reactive}.
#'
#' @return An echarts4r bar chart.
#' @export
#' @import echarts4r
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom shiny is.reactive
create_dev_ranking_chart <- function(ranking, threshold) {

  username <- n_apps <- NULL

  renderEcharts4r({
    if (is.reactive(ranking)) ranking <- ranking()
    ranking %>%
      filter(.data$n_apps > threshold()) %>%
      arrange(.data$n_apps) %>%
      e_charts(username) %>%
      e_bar(n_apps) %>%
      e_flip_coords() %>%
      e_tooltip()
  })
}


#' Create developer project network overview
#'
#' Leverages visNetwork.
#'
#' @param ranking Developer ranking. See \link{create_dev_ranking}. Useful
#' to get developers sorted by decreasing number of projects in the shiny selectInput.
#' Can be reactive.
#' @param client RSC client. See \link{create_rsc_client}.
#' @param apps_usage First element returned by \link{create_app_ranking}.
#' Can be reactive.
#' @param selected_dev Developer to select. You'll need a selectInput wrapped by \link[shiny]{reactive}.
#' @return A visNetwork htmlwidget with developer projects.
#' @export
#' @import visNetwork
#' @importFrom shiny is.reactive
create_dev_project_overview <- function(ranking, client, apps_usage, selected_dev) {

  renderVisNetwork({
    if (is.reactive(ranking)) ranking <- ranking()
    if (is.reactive(apps_usage)) apps_usage <- apps_usage()
    apps <- get_rsc_developer_apps_list(
      connectapi::user_guid_from_username(client, selected_dev()),
      apps_usage
    )

    groups <- c(
      rep(1, 1),
      rep(2, nrow(apps))
    )

    nodes <- tibble(
      id = seq_len(nrow(apps) + 1),
      group = groups,
      label = c(
        selected_dev(),
        paste0(
          apps$app_name,
          " (n view: ",
          apps$n,
          ")"
        )
      ),
      # Assign size depending on logs views
      value = c(1, apps$n)
    )

    edges <- tibble(
      from = rep(1, nrow(apps)),
      to = seq_len(nrow(apps)) + 1,
      color = rep("lightblue", nrow(apps)),
      dashes = rep(TRUE, nrow(apps))
    )

    # Network output
    visNetwork(nodes, edges, height = "100vh") %>%
      visOptions(
        selectedBy = "group",
        highlightNearest = TRUE,
        nodesIdSelection = TRUE
      ) %>%
      visEdges(smooth = FALSE, arrows = "to") %>%
      visOptions(collapse = TRUE) %>%
      visInteraction(dragView = FALSE, zoomView = TRUE) %>%
      visPhysics(
        solver = "repulsion",
        repulsion = list(
          nodeDistance = 200,
          springLength = 200
        )
      )
  })
}



#' Create apps consumer ranking bar chart
#'
#' @param ranking Data obtained from \link{create_apps_consumer_ranking}.
#'
#' @return A bar chart with consumer sorted by descending number of total views.
#' @export
create_apps_consumer_ranking_chart <- function(ranking) {

  username <- NULL
  renderEcharts4r({
    ranking() %>%
      e_charts(username) %>%
      e_bar(n) %>%
      e_flip_coords() %>%
      e_axis_labels(x = "Number of hits", y = "End user") %>%
      e_tooltip() %>%
      e_legend(FALSE)
  })
}



#' Create standard pie chart
#'
#' Pie chart with percentage data
#'
#' @param data Get from \link{sort_users_by_role}.
#' @param x_axis x variable.
#'
#' @return A pie chart.
#' @export
#' @import echarts4r
create_pie_chart <- function(data, x_axis) {
  Percentage <- NULL
  data %>%
    e_charts_(x_axis) %>%
    e_pie(Percentage, radius = c("50%", "70%")) %>%
    e_title(paste("RSC", names(data)[[1]], "(%)")) %>%
    e_tooltip() %>%
    e_legend(
      type = "scroll",
      orient = "vertical",
      left = "right",
      top = "bottom"
    )
}
