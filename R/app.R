df <-
  data.frame(
    facility = c("SUPERCALLAFRAGILISTICEXPIALADOCIOUS", "SUPERCALLAFRAGILISTICEXPIALADOCIOUS", "B", "C"),
    facility_address = c("123 Rainbow Road", "401 Not Found St", "111 Memory Lane", "321 Sesame Street"),
    facility_city = c("Vancouver", "The Web", "Paris", "foo"),
    facility_postal_code = c("V1D5H1", "V0Z1T1", "V61111", "ONETWO")
  )
df <- df |> tidyr::unite("united_fac", -tidyr::any_of("facility"), sep = ",", remove = FALSE)

fields_mandatory <- c("facility_name", "facility_address", "outbreak_report_dt")

cities <- c("Vancouver", "Richmond", "Sechelt", "North Vancouver")

## note to self - the attempt to address dupes current blocks all entry after;
## must be a problem with input$ for the facility or something
outbreakApp <- function(...) {
  ui <-
    fluidPage(

      shinyjs::useShinyjs(),
      shinyjs::inlineCSS(".mandatory_star { color: red; }"),

      outbreakFormInput("form")
    )


  server <- function(input, output, session) {

    outbreakFormServer("form")

  }

  shinyApp(ui, server, ...)
}
