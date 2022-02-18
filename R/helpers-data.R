#' Extract app usage dates
#'
#' @param app app_name to filter.
#' @param logs Given by RSC database.
#'
#' @return A vector containing dates of usage for
#' the given app.
#' @import dplyr
#' @export
#' @importFrom rlang .data
get_rsc_app_dates <- function(app, logs) {
  logs %>%
    filter(.data$app_name == !!app) %>%
    pull(.data$started)
}


#' Get RSC apps usage
#'
#' Called on apps_usage data
#'
#' @param logs Given by RSC database.
#'
#' @return A 3 columns tibble with app name, usage and date of usage (nested tibble).
#' @import dplyr purrr
#' @export
#' @importFrom rlang .data
get_rsc_apps_usage <- function(logs) {

  # Sort by usage an group by project name
  tmp_apps_usage <- logs %>%
    group_by(.data$app_name) %>%
    count(sort = TRUE)

  # handle dates. It's quite heavy but we need for
  # each app the dates of usage and a count for each date.
  # Reason why we use table(). We also need to nest them
  # and bind all results to create a big tibble with nested
  # tibbles. That way, we can add this one column tibble
  # to the other apps data columns. This will be necessary
  # to create the sparklines viz and calendars chart ...
  apps_dates <- map(
    tmp_apps_usage$app_name,
    get_rsc_app_dates,
    logs
  ) %>%
    map(tibble::as_tibble) %>%
    map(
      ~ {
        tbl <- ..1 %>%
          # Be careful, RSC provides datetime format but
          # this is too specific to count at the scale of a day.
          # We must floor to the corresponding day.
          mutate(value = lubridate::floor_date(value, "day")) %>%
          table() %>%
          tibble::as_tibble()

        names(tbl) <- c("Date", "Freq")
        tbl
      }
    ) %>%
    map(nest_by, .key = "calendar_data") %>%
    data.table::rbindlist()

  bind_cols(
    tmp_apps_usage,
    apps_dates = apps_dates
  )
}




#' Get app count for each developer
#'
#' @param developer Developer unique id.
#' @param logs All data logs.
#'
#' @return A number with all apps
#' @import dplyr
#' @export
#' @importFrom rlang .data
get_rsc_developer_apps_count <- function(developer, logs) {
  tmp <- logs %>%
    filter(.data$owner_guid == !!developer)

  n_apps <- if (nrow(tmp) > 0) {
    tmp %>%
      select(.data$name) %>%
      n_distinct()
  } else {
    0
  }

  as.integer(n_apps)

}





#' Get app list for each developer + usage
#'
#' @param developer Unique user id. Typically
#' app_developer_id key.
#' @param logs RSC logs.
#'
#' @return Tibble containing app name + count
#' @import dplyr
#' @export
#' @importFrom rlang .data
get_rsc_developer_apps_list <- function(developer, logs) {
  logs %>%
    filter(.data$owner_guid == !!developer) %>%
    group_by(.data$app_name) %>%
    count(sort = TRUE)
}



#' Get most used app
#'
#' @param logs Obtained after calling `get_rsc_apps_usage`.
#'
#' @return A numeric value.
#' @export
get_max_rsc_apps_usage <- function(logs) {
  logs %>%
    dplyr::pull(n) %>%
    max()
}




#' Process RSC user
#'
#' Select relevant information for RSC users data. See
#' \link{merge_rsc_data}.
#'
#' @param users Get from \link[connectapi]{get_users}.
#'
#' @return A 2 columns tibble with user guid and username.
#' @import dplyr
#' @export
#' @importFrom rlang .data
process_rsc_user <- function(users) {
  users  %>%
    mutate(username = tolower(.data$username)) %>%
    select(user_guid = .data$guid, .data$username)
}


#' Process RSC content
#'
#' Select relevant information for RSC content data. See
#' \link{merge_rsc_data}.
#'
#' @param content Get from \link[connectapi]{get_content}.
#'
#' @return A 3 columns tibble with content guid, app_name and owner guid.
#' Owner guid is the same returned in \link{process_rsc_user}.
#' @import dplyr
#' @export
#' @importFrom rlang .data
process_rsc_content <- function(content) {
  content %>%
    mutate(app_name = tolower(.data$name)) %>%
    select(
      content_guid = .data$guid,
      .data$app_name,
      .data$owner_guid
    )
}


#' Merge RStudio Connect data together
#'
#' See \link{create_app_ranking}.
#'
#' @param content Get from \link[connectapi]{get_content}.
#' @param users Get from \link[connectapi]{get_users}.
#' @param apps Get from \link[connectapi]{get_usage_shiny}.
#'
#' @return A list of 3 tibbles. `[[1]]`: light RSC content data (less columns);
#' `[[2]]`: light RSC users data (less columns); `[[3]]`: merged apps_usage data.
#' @export
#' @import dplyr
#' @importFrom rlang .data
merge_rsc_data <- function(content, users, apps) {
  rsc_content_light <- process_rsc_content(content)
  rsc_users_light <- process_rsc_user(users)

  # This table contain all logs for which we
  # have the app owner, visitor and duration of visit.
  apps_usage_merged <- apps %>%
    left_join(rsc_content_light, by = "content_guid") %>%
    left_join(rsc_users_light, by = "user_guid") %>%
    mutate(duration = .data$ended - .data$started) %>%
    select(-.data$content_guid, -.data$user_guid, -.data$data_version) %>%
    filter(!is.na(.data$app_name)) %>%
    tidyr::replace_na(list(user_guid = "Unknown"))

  list(
    rsc_content_light = rsc_content_light,
    rsc_users_light = rsc_users_light,
    apps_usage_merged = apps_usage_merged
  )
}



#' Daily app usage
#'
#' Used by \link{create_app_daily_usage_chart}
#'.
#' @param apps_usage Second element returned by \link{create_app_ranking}.
#' @param selected_app Selected app name (string). You'll need a selectInput for instance.
#' wrapped by \link[shiny]{reactive}.
#' @return Calendar data for daily app usage.
#' @export
#' @importFrom shiny reactive
#' @import dplyr
#' @importFrom rlang .data
get_app_daily_usage <- function(apps_usage, selected_app) {
  reactive({
    apps_usage %>%
      filter(.data$app_name == !!selected_app) %>%
      select(.data$calendar_data) %>%
      tidyr::unnest(cols = c(.data$calendar_data))
  })
}



#' Get daily shiny app usage for a given user
#'
#' @inheritParams merge_rsc_data
#' @param selected_user User to select.
#' You'll need a selectInput wrapped by \link[shiny]{reactive}.
#'
#' @return 2 columns tibble containing daily app consuption for given user.
#' @export
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom shiny reactive
get_user_daily_consumption <- function(content, users, apps, selected_user) {
  rsc_data_merged <- merge_rsc_data(content, users, apps)

  res <- reactive({
    rsc_data_merged[[3]] %>%
      filter(.data$username == !!selected_user) %>%
      mutate(floored_started = lubridate::floor_date(.data$started, "day")) %>%
      group_by(.data$floored_started) %>%
      summarize(n = n()) %>%
      select(Date = .data$floored_started, Freq = .data$n)
  })

  list(data = res, user = selected_user)
}


#' Process app data for ranking table
#'
#' See \link{create_app_ranking_table}.
#'
#' @inheritParams merge_rsc_data
#'
#' @return A list containing: `[[1]]` merged data between app usage and users data.
#' `[[2]]`: data to be digested by \link{create_app_ranking_table}.
#' @export
#' @import dplyr
#' @importFrom rlang .data
create_app_ranking <- function(content, users, apps) {

  rsc_data_merged <- merge_rsc_data(content, users, apps)

  processed_rsc_apps_usage <- rsc_data_merged[[3]] %>%
    get_rsc_apps_usage() %>%
    left_join(
      rsc_data_merged[[1]] %>%
        mutate(user_guid = .data$owner_guid),
      by = "app_name"
    ) %>%
    left_join(rsc_data_merged[[2]], by = "user_guid") %>%
    select(-contains("_guid"))

  list(rsc_data_merged[[3]], processed_rsc_apps_usage)
}



#' Create Shiny apps consumer ranking
#'
#' Sort consumers by number of views.
#'
#' @param apps Get from \link[connectapi]{get_usage_shiny}.
#' @param users Get from \link[connectapi]{get_users}.
#' @param threshold Minimum number of views threshold. You'll need a numericInput
#' wrapped by \link[shiny]{reactive}.
#'
#' @return A 3 columns tibble with apps consumer sorted by number of view. The role
#' columns allows further analysis.
#' @export
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom shiny reactive
create_apps_consumer_ranking <- function(apps, users, threshold) {
  reactive({
    apps %>%
      group_by(.data$user_guid) %>%
      summarise(n = n()) %>% # prefer summarize over sort to remove grouping
      arrange(n) %>%
      filter(n > !!threshold) %>%
      left_join(users %>% mutate(user_guid = .data$guid), by = "user_guid") %>%
      select(.data$username, .data$n, .data$user_role) %>%
      tidyr::replace_na(list(username = "Unknown", user_role = "External"))
  })
}



#' Create a developer ranking
#'
#' Rank developers by number of published apps
#'
#' @param users Get from \link[connectapi]{get_users}.
#' @param content Get from \link[connectapi]{get_content}.
#'
#' @return A tibble with developer ranked by decreasing number of apps.
#' @export
#' @import dplyr
#' @importFrom rlang .data
create_dev_ranking <- function(users, content) {
  users %>%
    filter(.data$user_role == "publisher") %>%
    mutate(
      n_apps = map_int(
        .data$guid,
        get_rsc_developer_apps_count,
        logs = content
      )
    ) %>%
    filter(.data$n_apps > 0) %>%
    arrange(desc(.data$n_apps))
}



#' Sort RStudio Connect users by role
#'
#' Users are grouped by user_role to check the
#' server role repartition.
#'
#' @param users Get from \link[connectapi]{get_users}.
#'
#' @return A tibble with user grouped by role.
#' @export
#' @importFrom rlang .data
sort_users_by_role <- function(users) {
  users %>%
    group_by(.data$user_role) %>%
    summarize(n = n()) %>%
    mutate(Percentage = round(n / sum(n) * 100))
}


#' Sort RStudio Connect content by access type
#'
#'
#' @param content Get from \link[connectapi]{get_content}.
#'
#' @return A tibble with content grouped by access type.
#' @export
#' @importFrom rlang .data
sort_content_by_access <- function(content) {
  content %>%
    group_by(.data$access_type) %>%
    summarize(n = n()) %>%
    mutate(Percentage = round(n / sum(n) * 100))
}



#' Sort RStudio Connect content by R version
#'
#'
#' @param content Get from \link[connectapi]{get_content}.
#'
#' @return A tibble with content grouped by R version.
#' @export
#' @importFrom rlang .data
sort_content_by_rversion <- function(content) {
  content %>%
    filter(!is.na(.data$r_version)) %>%
    group_by(.data$r_version) %>%
    summarize(n = n()) %>%
    mutate(Percentage = round(n / sum(n) * 100, 1))
}



#' Sort RStudio Connect content by app mode
#'
#'
#' @param content Get from \link[connectapi]{get_content}.
#'
#' @return A tibble with content grouped by app mode.
#' @export
#' @importFrom rlang .data
sort_content_by_appmode <- function(content) {
  content %>%
    group_by(.data$app_mode) %>%
    summarize(n = n()) %>%
    mutate(Percentage = round(n / sum(n) * 100, 1))
}
