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

        textInputUI(
          "facility",
          label_mandatory("Facility Name"),
          choices = df$facility,
          selected = NULL
        ),
        # selectizeInput(
        #   "facility",
        #   label_mandatory("Facility Name"),
        #   choices = df$facility,
        #   selected = NULL,
        #   options = list(create = TRUE)
        # ),
        "Facility address: ",
        fluidRow(
          textInputUI(
            "facility_address",
            label_mandatory("Street Address"),
            choices = NULL
          ),
          textInput2("facility_postal_code", "Postal Code", maxlength = 7),
          selectizeInput("facility_city", "City", choices = cities),
          ## revisit and consider adding flex-wrap: wrap;
          style = "display: flex; justify-content: space-between; max-width: 1000px;"
        ),
        selectizeInput(
          "outbreak_type",
          "Is this an enteric or respiratory outbreak?",
          choices = c("Enteric", "Respiratory"),
          options = list(onInitialize = I("function() { this.setValue(''); }"))
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

    #facility_address <- textInputServer("facility_address", reactive(input$facility))
    #outbreak_symptom_onset_dt <- dateSelectServer("outbreak_symptom_onset_dt",)
    #outbreak_report_dt <- dateSelectServer("outbreak_report_dt")

    observeEvent(input$facility, {
      ## only want to fill in the info if the facility is known
      if (input$facility %in% df$facility) {
        address <- df$facility_address[df$facility %in% input$facility]
        updateSelectizeInput(session, "facility_address", selected = address, choices = address)

        city <- df$facility_city[df$facility %in% input$facility]
        updateSelectizeInput(session, "facility_city", selected = city, choices = city)
      } #else {
      #   updateSelectizeInput(session, "facility_address", selected = input$facility_address)
      #   updateSelectizeInput(session, "facility_city", selected = input$facility_city)
      # }
    }, ignoreNULL = FALSE)

  }
  shinyApp(ui, server, ...)
}
