library(shiny)
library(connectapi) # Tested with 0.1.0.9031
library(connectViz)
library(dplyr)
library(shiny)
library(bslib)
library(echarts4r)
library(toastui)
library(visNetwork)

rsc_client <- create_rsc_client()

# TO DO: refresh this in the background
apps_usage <- rsc_client %>% get_usage_shiny(limit = Inf)
rsc_content <- rsc_client %>% get_content()
# Remove locked users?
rsc_users <- rsc_client %>% get_users(limit = Inf) %>%
  filter(locked == FALSE)
publishers <- rsc_users %>% filter(user_role == "publisher")
shiny_apps <- rsc_content %>% filter(app_mode == "shiny")

general_metrics <- list(
  "Onboarded Users" = nrow(rsc_users),
  "Publishers" = nrow(publishers),
  "Deployments" = nrow(rsc_content),
  "Shiny Apps" = nrow(shiny_apps)
)

general_metrics_cards <- purrr::map(
  seq_along(general_metrics),
  function(i) {
    value_box(
      theme_color = "secondary",
      value = general_metrics[[i]],
      title = names(general_metrics)[[i]]
    )
  }
)

developers_apps_ranking <- create_dev_ranking(rsc_users, rsc_content)
max_app <- developers_apps_ranking %>% pull(n_apps) %>% max()

ui <- page_navbar(
  title = "Shiny apps usage",
  sidebar = tagList(
    dateRangeInput(
      "date_range",
      "Select the app usage range:",
      start = Sys.Date() - 10,
      end = Sys.Date()
    )
  ),
  nav_panel(
    "General metrics",
    layout_columns(
      fill = FALSE,
      !!!general_metrics_cards
    ),
    layout_columns(
      col_widths = rep(6, 4),
      card(
        card_header("User roles"),
        full_screen = TRUE,
        min_height = "400px",
        echarts4rOutput("roles_chart"),
      ),
      card(
        card_header("Content access type"),
        full_screen = TRUE,
        min_height = "400px",
        echarts4rOutput("content_access"),
      ),
      card(
        card_header("R versions"),
        full_screen = TRUE,
        min_height = "400px",
        echarts4rOutput("r_versions"),
      ),
      card(
        card_header("Content type"),
        full_screen = TRUE,
        min_height = "400px",
        echarts4rOutput("content_type"),
      )
    )
  ),
  nav_panel(
    "App data",
    h3("Apps ranking"),
    card(
      full_screen = TRUE,
      min_height = "400px",
      datagridOutput("apps_ranking_chart")
    ),
    h3("Single app usage"),
    card(
      full_screen = TRUE,
      min_height = "1200px",
      layout_sidebar(
        sidebar = sidebar(
          width = 150,
          selectInput(
            "selected_app",
            "Select an application",
            choices = NULL
          )
        ),
        echarts4rOutput("daily_usage_chart"),
        echarts4rOutput("daily_sessions_chart"),
        p("User consumption"),
        echarts4rOutput("cumulated_duration_per_user_chart"),
        echarts4rOutput("cumulated_hits_per_user_chart"),

      )
    )
  ),
  nav_panel(
    "Consumer data",
    card(
      full_screen = TRUE,
      min_height = "400px",
      card_header("Consumer ranking"),
      layout_sidebar(
        sidebar = sidebar(
          width = 150,
          numericInput(
            "views_threshold",
            "N view threshold",
            1,
            min = 0
          )
        ),
        echarts4rOutput("consumer_ranking_chart")
      )
    ),
    card(
      full_screen = TRUE,
      min_height = "400px",
      card_header("Daily app consumption"),
      layout_sidebar(
        sidebar = sidebar(
          width = 150,
          selectInput("selected_user", "Select a user", rsc_users$username)
        ),
        echarts4rOutput("daily_consumption_chart")
      )
    )
  ),
  nav_panel(
    "Developer data",
    card(
      full_screen = TRUE,
      min_height = "400px",
      card_header("Developers ranking n deployments (static, shiny, rmd, ...)"),
      layout_sidebar(
        sidebar = sidebar(
          width = 150,
          numericInput(
            "apps_threshold",
            "N app threshold",
            round(max_app / 2),
            min = 1,
            max = max_app
          )
        ),
        echarts4rOutput("dev_ranking_chart")
      )
    ),
    card(
      full_screen = TRUE,
      min_height = "400px",
      card_header("Dev projects overview (Shiny apps only)"),
      layout_sidebar(
        sidebar = sidebar(
          width = 150,
          selectInput(
            "app_developer",
            "Select a developer",
            developers_apps_ranking$username
          )
        ),
        visNetworkOutput("dev_projects_network")
      )
    )
  )
)

server <- function(input, output, session) {

# Apps ranking ------------------------------------------------------------

  apps_ranking <- create_app_ranking(
    rsc_content,
    rsc_users,
    apps_usage,
    start_date = reactive(input$date_range[1]),
    end_date = reactive(input$date_range[2])
  )

  range_text <- reactive({
    sprintf("between %s and %s", input$date_range[1], input$date_range[2])
  })

  output$app_usage_title <- renderText({
    sprintf("Shiny Apps usage %s", range_text())
  })

  output$apps_ranking_chart <- renderDatagrid({
    create_app_ranking_table(apps_ranking)
  })

# App usage ---------------------------------------------------------------

  daily_app_usage <- get_app_daily_usage(reactive(apps_ranking()[[2]]), reactive(input$selected_app))

  observeEvent(apps_ranking, {
    updateSelectInput(
      session,
      "selected_app",
      choices = apps_ranking()[[2]]$app_name
    )
  })

  output$daily_usage_chart <- create_app_daily_usage_chart(daily_app_usage)
  output$daily_sessions_chart <-create_app_daily_session_chart(daily_app_usage)

  output$cumulated_duration_per_user_chart <- create_cumulated_duration_per_user(
    reactive(apps_ranking()[[1]]),
    selected_app = reactive(input$selected_app)
  )
  output$cumulated_hits_per_user_chart <- create_cumulated_hits_per_user(
   reactive(apps_ranking()[[1]]),
    selected_app = reactive(input$selected_app)
  )

# User consumption --------------------------------------------------------

  consumer_ranking <- create_apps_consumer_ranking(apps_usage, rsc_users)
  output$consumer_ranking_chart <- create_apps_consumer_ranking_chart(
    consumer_ranking, reactive(input$views_threshold)
  )

  daily_consumption <- get_user_daily_consumption(
    rsc_content,
    rsc_users,
    apps_usage,
    reactive(input$selected_user)
  )

  output$daily_consumption_chart <- create_user_daily_consumption_chart(daily_consumption[[2]])


# Developers data ---------------------------------------------------------

  output$dev_ranking_chart <- create_dev_ranking_chart(
    developers_apps_ranking,
    reactive(input$apps_threshold)
  )

  output$dev_projects_network <- create_dev_project_overview(
    rsc_client, reactive(apps_ranking()[[1]]),
    reactive(input$app_developer)
  )


# General data ------------------------------------------------------------
  output$roles_chart <- renderEcharts4r({
    tmp <- sort_users_by_role(rsc_users, input$date_range[1], input$date_range[2])
    validate(need(nrow(tmp) > 0, "No data found"))
    tmp %>% create_pie_chart("user_role")
  })

  output$content_access <- renderEcharts4r({
    tmp <- sort_content_by_access(rsc_content, input$date_range[1], input$date_range[2])
    validate(need(nrow(tmp) > 0, "No data found"))
    tmp %>% create_pie_chart("access_type")
  })

  output$r_versions <- renderEcharts4r({
    tmp <- sort_content_by_rversion(rsc_content, input$date_range[1], input$date_range[2])
    validate(need(nrow(tmp) > 0, "No data found"))
    tmp %>% create_pie_chart("r_version")
  })

  output$content_type <- renderEcharts4r({
    tmp <- sort_content_by_appmode(rsc_content, input$date_range[1], input$date_range[2])
    validate(need(nrow(tmp) > 0, "No data found"))
    tmp %>% create_pie_chart("app_mode")
  })
}

shinyApp(ui, server)
