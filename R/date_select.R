dateSelectUI <- function(id, label, value = lubridate::NA_Date_) {
  dateInput(NS(id, "dateSelect"), label,
            max = Sys.Date(),
            value = value)
}

dateSelectServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
