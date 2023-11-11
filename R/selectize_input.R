#'  @title Select Input
#'
#'  @description
#'  selectizeInput with some modifications. A simple wrapper for `shinydashboard::selectizeInput()`
#'
#'
#'  @param id The id to refer to the element on the server side
#'  @param label Text that will be displayed with the input box
#'  @param choices What values will be seen by the user in the drop down menu
#'  @param width Size of the input box
#'  @param max_length Set a character limit for the value
#'  @param ... Other optional arguments supplied to `options`
#'
#'  @returns Some HTML that produces a dropdown for users to select options.
#'
#'  @details This is a wrapper for selectizeInput which sets some default parameters that are commonly used.
#'  Users need to be able to create new entries, and we want the default start up to always be blank.
#'
#'  @seealso \code{\link[shinydashboard]{selectizeInput}}
#'
selectize_input <- function(id, label, choices = NULL, width = NULL, max_length = NULL, ...) {

  tag <-
    selectizeInput(
      id,
      label,
      choices = choices,
      width = width,
      options =
        list(
          create = TRUE,
          onInitialize = I("function() { this.setValue(''); }"),
          ...
        )
    )

  if (!is.null(max_length)) {
    htmltools::tagQuery(tag)$
      children("input")$
      addAttrs(maxlength=max_length)$
      allTags()
  } else {
    tag
  }

}
