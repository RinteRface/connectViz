#' Extract app usage dates
#'
#' @param app app_name to filter.
#' @param logs Given by RSC database.
#'
#' @return A tibble containing dates of usage for
#' the given app as well as the correponding session duration.
#' @import dplyr
#' @export
#' @importFrom rlang .data
get_rsc_app_dates <- function(app, logs) {
  logs %>%
    filter(.data$app_name == !!app) %>%
    select(.data$started, .data$duration)
}


#' Get RSC apps usage
#'
#' Called on apps_usage data
#'
#' @param logs Given by RSC database.
#'
#' @return A 3 columns tibble with app name, usage and date of usage (nested tibble).
#' @import dplyr purrr lubridate
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
    map(
      ~ {
        dat_in <- ..1 %>%
          # Be careful, RSC provides datetime format but
          # this is too specific to count at the scale of a day.
          # We must floor to the corresponding day.
          mutate(started = floor_date(.data$started, "day"))

        tbl <- dat_in %>%
          select(-.data$duration) %>%
          table() %>%
          tibble::as_tibble()

        names(tbl) <- c("Date", "Freq")

        # Create duration table
        cumulated_durations <- do.call(rbind.data.frame, map(tbl$Date, ~ {
          dat_in %>%
            filter(started == as_datetime(..1)) %>%
            summarise(cum_dur = minute(seconds_to_period(sum(.data$duration, na.rm = TRUE))))
        }))

        cbind(tbl, cumulated_durations)
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
#' @param content Get from \link[connectapi]{get_content}. Can be reactive.
#' @param users Get from \link[connectapi]{get_users}. Can be reactive.
#' @param apps Get from \link[connectapi]{get_usage_shiny}. Can be reactive.
#'
#' @return A list of 3 tibbles. `[[1]]`: light RSC content data (less columns);
#' `[[2]]`: light RSC users data (less columns); `[[3]]`: merged apps_usage data.
#' @export
#' @import dplyr
#' @importFrom rlang .data
merge_rsc_data <- function(content, users, apps) {

  if (nrow(apps) == 0) {
    stop("No data usage found. Please contact your RStudio Connect admin.")
  }

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
#' Can also be reactive.
#' @param selected_app Selected app name (string). You'll need a selectInput for instance.
#' wrapped by \link[shiny]{reactive}.
#' @return Calendar data for daily app usage.
#' @export
#' @importFrom shiny reactive is.reactive
#' @import dplyr
#' @importFrom rlang .data
get_app_daily_usage <- function(apps_usage, selected_app) {
  reactive({
    # distinguish between static data vs reactive data
    if (is.reactive(apps_usage)) apps_usage <- apps_usage()
    req(selected_app())
    apps_usage %>%
      filter(.data$app_name == selected_app()) %>%
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
#' @return A list. `[[1]]` contains the row events filtered for the
#' given user. `[[2]]` is 2 columns tibble containing daily app consumption
#' for given user (grouped by dates).
#' @export
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom shiny reactive is.reactive
get_user_daily_consumption <- function(content, users, apps, selected_user) {
  events <- reactive({
    if (is.reactive(content)) content <- content()
    if (is.reactive(users)) users <- users()
    if (is.reactive(apps)) apps <- apps()

    rsc_data_merged <- merge_rsc_data(content, users, apps)
    rsc_data_merged[[3]] %>%
      filter(.data$username == selected_user())
  })

  grouped_by_date <- reactive({
    req(nrow(events()) > 0)
    events() %>%
      mutate(floored_started = floor_date(.data$started, "day")) %>%
      group_by(.data$floored_started) %>%
      count(sort = TRUE) %>%
      select(Date = .data$floored_started, Freq = .data$n)
  })
  list(events, grouped_by_date)
}


#' Process app data for ranking table
#'
#' See \link{create_app_ranking_table}.
#'
#' @inheritParams merge_rsc_data
#' @param start_date Default to minimum calendar_data date. Could also be
#' an input value with Shiny.
#' @param end_date Default to maximum calendar_data date. Could also be
#' an input value with Shiny.
#'
#' @return A list containing: `[[1]]` merged data between app usage and users data.
#' `[[2]]`: data to be digested by \link{create_app_ranking_table}.
#' @export
#' @import dplyr
#' @importFrom rlang .data quo eval_tidy
#' @importFrom shiny reactive is.reactive
create_app_ranking <- function(content, users, apps, start_date = NULL, end_date = NULL) {
  tmp <- quo({
    rsc_data_merged <- merge_rsc_data(content, users, apps)
    if (is.null(start_date)) start_date <- min(rsc_data_merged$apps_usage_merged$started)
    # Note: this might be surprising but some end date are not available in ended so
    # we have to take the max of the start date.
    if (is.null(end_date)) end_date <- max(rsc_data_merged$apps_usage_merged$started)
    if (is.reactive(start_date)) start_date <- start_date()
    if (is.reactive(end_date)) end_date <- end_date()

    rsc_data_merged[[3]] <- rsc_data_merged[[3]] %>%
      filter(
        .data$started >= start_date &
          .data$started <= end_date
      )

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
  })

  if (is.reactive(users) || is.reactive(content) || is.reactive(apps) ||
      is.reactive(start_date) || is.reactive(end_date)) {
    if (is.reactive(users)) user <- users()
    if (is.reactive(content)) content <- content()
    if (is.reactive(apps)) apps <- apps()
    rlang::inject(reactive(!!tmp))
  } else {
    eval_tidy(tmp)
  }
}



#' Create Shiny apps consumer ranking
#'
#' Sort consumers by number of views.
#'
#' @param apps Get from \link[connectapi]{get_usage_shiny}. Can be reactive or not.
#' @param users Get from \link[connectapi]{get_users}. Can be reactive or not.
#'
#' @return A 3 columns tibble with apps consumer sorted by number of view. The role
#' columns allows further analysis.
#' @export
#' @import dplyr
#' @importFrom rlang .data quo eval_tidy
#' @importFrom shiny reactive is.reactive
create_apps_consumer_ranking <- function(apps, users) {

  tmp <- quo(
    apps %>%
      group_by(.data$user_guid) %>%
      summarise(n = n()) %>% # prefer summarize over sort to remove grouping
      arrange(n) %>%
      left_join(users %>% mutate(user_guid = .data$guid), by = "user_guid") %>%
      select(.data$username, .data$n, .data$user_role) %>%
      tidyr::replace_na(list(username = "Unknown", user_role = "External"))
  )

  if (is.reactive(users) || is.reactive(apps)) {
    if (is.reactive(apps)) apps <- apps()
    if (is.reactive(users)) users <- users()
    rlang::inject(reactive(!!tmp))
  } else {
    eval_tidy(tmp)
  }
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
#' @importFrom rlang .data quo eval_tidy
#' @importFrom shiny reactive is.reactive
create_dev_ranking <- function(users, content) {
  tmp <- quo(
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
  )
  if (is.reactive(users) || is.reactive(content)) {
    if (is.reactive(users)) user <- users()
    if (is.reactive(content)) content <- content()
    rlang::inject(reactive(!!tmp))
  } else {
    eval_tidy(tmp)
  }
}



#' Sort RStudio Connect users by role
#'
#' Users are grouped by user_role to check the
#' server role repartition. Using start_date and end_date allows
#' to filter data given a specific time range.
#'
#' @param users Get from \link[connectapi]{get_users}.
#' @param start_date Date filter.
#' @param end_date Date filter.
#'
#' @return A tibble with user grouped by role.
#' @export
#' @importFrom rlang .data
sort_users_by_role <- function(users, start_date = NULL, end_date = NULL) {
  if (is.null(start_date)) start_date <- min(users$created_time)
  if (is.null(end_date)) end_date <- max(users$created_time)

  users %>%
    filter(.data$created_time >= start_date & .data$created_time <= end_date) %>%
    group_by(.data$user_role) %>%
    summarize(n = n()) %>%
    mutate(Percentage = round(n / sum(n) * 100))
}


#' Sort RStudio Connect content by access type
#'
#'
#' @param content Get from \link[connectapi]{get_content}.
#' @inheritParams sort_users_by_role
#'
#' @return A tibble with content grouped by access type.
#' @export
#' @importFrom rlang .data
sort_content_by_access <- function(content, start_date = NULL, end_date = NULL) {
  if (is.null(start_date)) start_date <- min(content$created_time)
  if (is.null(end_date)) end_date <- max(content$created_time)

  content %>%
    filter(.data$created_time >= start_date & .data$created_time <= end_date) %>%
    group_by(.data$access_type) %>%
    summarize(n = n()) %>%
    mutate(Percentage = round(n / sum(n) * 100))
}



#' Sort RStudio Connect content by R version
#'
#'
#' @param content Get from \link[connectapi]{get_content}.
#' @inheritParams sort_users_by_role
#'
#' @return A tibble with content grouped by R version.
#' @export
#' @importFrom rlang .data
sort_content_by_rversion <- function(content, start_date = NULL, end_date = NULL) {
  if (is.null(start_date)) start_date <- min(content$created_time)
  if (is.null(end_date)) end_date <- max(content$created_time)

  content %>%
    filter(
      !is.na(.data$r_version) &
      .data$created_time >= start_date &
      .data$created_time <= end_date
    ) %>%
    group_by(.data$r_version) %>%
    summarize(n = n()) %>%
    mutate(Percentage = round(n / sum(n) * 100, 1))
}

#' Sort RStudio Connect content by python version
#'
#'
#' @param content Get from \link[connectapi]{get_content}.
#'
#' @return A tibble with content grouped by python version.
#' @export
#' @importFrom rlang .data
sort_content_by_pyversion <- function(content) {
  content %>%
    filter(!is.na(.data$py_version)) %>%
    group_by(.data$py_version) %>%
    summarize(n = n()) %>%
    mutate(Percentage = round(n / sum(n) * 100, 1))
}

#' Sort RStudio Connect content by app mode
#'
#'
#' @param content Get from \link[connectapi]{get_content}.
#' @inheritParams sort_users_by_role
#'
#' @return A tibble with content grouped by app mode.
#' @export
#' @importFrom rlang .data
sort_content_by_appmode <- function(content, start_date = NULL, end_date = NULL) {
  if (is.null(start_date)) start_date <- min(content$created_time)
  if (is.null(end_date)) end_date <- max(content$created_time)

  content %>%
    filter(.data$created_time >= start_date & .data$created_time <= end_date) %>%
    group_by(.data$app_mode) %>%
    summarize(n = n()) %>%
    mutate(Percentage = round(n / sum(n) * 100, 1))
}
