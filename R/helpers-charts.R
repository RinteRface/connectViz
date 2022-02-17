#' Daily app usage chart
#'
#' Leverages echarts4r.
#' See \url{https://echarts4r.john-coene.com/articles/chart_types.html#calendar-1}.
#'.
#' @param apps_usage Second element returned by \link{create_app_ranking}.
#' @return A calendar chart displaying daily app usage.
#' @export
#' @importFrom shiny selectInput reactive
#' @import echarts4r
#' @importFrom rlang .data
create_app_daily_usage <- function(apps_usage) {
  selectInput("selected_app", "Select an application", apps_usage$app_name)

  # Join all data
  targeted_rsc_apps_usage <- reactive({
    apps_usage %>%
      dplyr::filter(.data$app_name == .data$input$selected_app)
  })

  # Calendar chart
  renderEcharts4r({
    tmp_app <- targeted_rsc_apps_usage()

    calendar_data <- tmp_app[ , "calendar_data"] %>%
      tidyr::unnest(cols = c(calendar_data))
    max_usage <- max(calendar_data$Freq)
    year_range <- c(min(calendar_data$Date), max(calendar_data$Date))

    calendar_data %>%
      e_charts(.data$Date, width = "1200px") %>%
      e_calendar(range = .data$year_range) %>%
      e_effect_scatter(.data$Freq, coord_system = "calendar") %>%
      e_visual_map(
        max = .data$max_usage,
        inRange = list(
          color = c('#e0f3f8', '#abd9e9', '#74add1', '#4575b4', '#313695')
        )
      ) %>%
      e_title(tmp_app$app_name) %>%
      e_tooltip()
  })
}


#' Create cumulated app duration/user
#'
#' Bar chart
#'
#' @param apps_usage First element returned by \link{create_app_ranking}.
#'
#' @return An echarts4r barchart.
#' @export
#' @import echarts4r
#' @import dplyr
#' @importFrom rlang .data
create_cumulated_duration_per_user <- function(apps_usage) {
  renderEcharts4r({
    apps_usage %>%
      filter(.data$app_name == .data$input$selected_app) %>%
      mutate(duration = as.numeric(.data$duration)) %>%
      group_by(.data$username) %>%
      summarise(cum_duration = round(sum(.data$duration) / 3600)) %>%
      arrange(.data$cum_duration) %>%
      e_charts(.data$username) %>%
      e_bar(.data$cum_duration) %>%
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
#'
#' @return An echarts4r barchart.
#' @export
#' @import echarts4r
#' @import dplyr
#' @importFrom rlang .data
create_cumulated_hits_per_user <- function(apps_usage) {
  renderEcharts4r({
    apps_usage %>%
      filter(.data$app_name == .data$input$selected_app) %>%
      group_by(.data$username) %>%
      summarise(n = n()) %>% # prefer summarize over sort to remove grouping
      arrange(n) %>%
      e_charts(.data$username) %>%
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
#'
#' @return An echarts4r bar chart.
#' @export
#' @import echarts4r
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom shiny numericInput
create_dev_ranking_chart <- function(ranking) {
  numericInput(
    "apps_threshold",
    "N app threshold",
    5,
    min = 1,
    max = ranking %>% pull(.data$n_apps) %>% max()
  )

  renderEcharts4r({
    ranking %>%
      filter(.data$n_apps > .data$input$apps_threshold) %>%
      arrange(.data$n_apps) %>%
      e_charts(.data$username) %>%
      e_bar(.data$n_apps) %>%
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
#' @param client RSC client. See \link{create_rsc_client}.
#' @param apps_usage First element returned by \link{create_app_ranking}.
#' @return A visNetwork htmlwidget with developer projects.
#' @export
#' @importFrom shiny selectInput
#' @import visNetwork
#' @importFrom rlang .data
create_dev_project_overview <- function(ranking, client, apps_usage) {
  selectInput(
    "app_developer",
    "Select a developer",
    ranking$username
  )


  renderVisNetwork({
    apps <- get_rsc_developer_apps_list(
      connectapi::user_guid_from_username(client, .data$input$app_developer),
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
        .data$input$app_developer,
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
    e_tooltip()
}
