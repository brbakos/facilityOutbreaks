textInputUI <- function(id, label, choices = NULL, ...) {
  #ns <- NS(id)
  selectizeInput(
    id,
    label,
    choices = choices,
    options =
      list(
        create = TRUE,
        onInitialize = I("function() { this.setValue(''); }")),
    ...
  )
}

textInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
