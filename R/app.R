df <-
  data.frame(
    facility = c("A", "B", "C"),
    facility_address = c("123 Rainbow Road", "111 Memory Lane", "321 Sesame Street"),
    facility_city = c("Vancouver", "Paris", "foo")
  )

fields_mandatory <- c("facility_name", "facility_address", "outbreak_report_dt")

cities <- c("Vancouver", "Richmond", "Sechelt", "North Vancouver")

label_mandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

app_css <- ".mandatory_star { color: red; }"

outbreakApp <- function(...) {
  ui <-
    fluidPage(

      shinyjs::useShinyjs(),
      shinyjs::inlineCSS(app_css),

      div(
        id = "form",

        selectize_input(
          "facility",
          label_mandatory("Facility Name"),
          choices = df$facility,
          selected = NULL
        ),
        "Facility address: ",
        fluidRow(
          selectize_input(
            "facility_address",
            label_mandatory("Street Address"),
            choices = NULL
          ),
          textInput2("facility_postal_code", "Postal Code", maxlength = 7),
          selectize_input("facility_city", "City", choices = cities),
          ## revisit and consider adding flex-wrap: wrap;
          style = "display: flex; justify-content: space-between; max-width: 1000px;"
        ),
        selectize_input(
          "outbreak_type",
          "Is this an enteric or respiratory outbreak?",
          choices = c("Enteric", "Respiratory")
        ),
        fluidRow(
          dateSelectUI("outbreak_symptom_onset_dt", "When was the first symptom onset?"),
          dateSelectUI("outbreak_report_dt", "What day was the outbreak reported?"),
          style = "display: flex; justify-content: flex-start;"
        ),
        actionButton("submit", "Submit", class = "btn-primary")
      )
    )
  server <- function(input, output, session) {

    observeEvent(input$facility-input, {
      ## only want to fill in the info if the facility is known
      ## otherwise just keep the input as-is
      if (input$facility-input %in% df$facility) {
          facility <- input$facility-input
          address <- df$facility_address[df$facility %in% facility]
          updateSelectizeInput(session, "facility_address", selected = address, choices = address)

          city <- df$facility_city[df$facility %in% facility]
          updateSelectizeInput(session, "facility_city", selected = city, choices = city)
        }
    }, ignoreNULL = FALSE)

  }
  shinyApp(ui, server, ...)
}
