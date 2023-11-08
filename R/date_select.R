date_select <- function(id, label, value = lubridate::NA_Date_, ...) {
  dateInput(
    id,
    label,
    max = Sys.Date(),
    value = value,
    ...
  )
}
