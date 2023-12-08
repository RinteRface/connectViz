test_that("create_calendar_chart works with multi years", {
  dates <- seq.Date(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")
  n <- rpois(length(dates), 20)

  calendar_data <- data.frame(app_name = "my_app", Date = dates, Freq = n)

  expect_error(create_calendar_chart(calendar_data), NA)
})
