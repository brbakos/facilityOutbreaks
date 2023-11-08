textInputUI <- function(id, label, choices = NULL, ...) {
  #ns <- NS(id)
  selectizeInput(
    id,
    label,
    choices = choices,
    ...)
}

textInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
