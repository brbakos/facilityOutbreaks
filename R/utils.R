#' Text input with max length
#'
#' @details This is a wrapper for textInput which allows you to set a max length.
#'
#' @param maxlength The number of characters to limit the entry to.
textInput2 <- function(inputId, label, value = "", width = NULL,
                       placeholder = NULL, maxlength = NULL) {

  tag <- shiny::textInput(
    inputId = inputId,
    label = label,
    value = value,
    width = width,
    placeholder = placeholder
  )

  if (!is.null(maxlength)) {
    htmltools::tagQuery(tag)$
      children("input")$
      addAttrs(maxlength=maxlength)$
      allTags()
  } else {
    tag
  }

}

is_blank <- function(x) {
  x == ""
}

label_mandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

app_css <- ".mandatory_star { color: red; }"


