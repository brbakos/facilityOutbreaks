is_blank <- function(x) {
  x == ""
}

label_mandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
