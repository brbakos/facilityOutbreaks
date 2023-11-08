#' selectizeInput with some modifications
#'
#' @details This is a wrapper for selectizeInput which sets some default parameters that are commonly used.
#'  Users need to be able to create new entries, and we want the default start up to always be blank.
selectize_input <- function(id, label, choices = NULL, ...) {
  selectizeInput(
    id,
    label,
    choices = choices,
    options =
      list(
        create = TRUE,
        onInitialize = I("function() { this.setValue(''); }"),
        ...)
  )
}
