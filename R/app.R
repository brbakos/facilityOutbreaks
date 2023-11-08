df <-
  data.frame(
    facility = c("A", "B", "C"),
    facility_address = c("123 Rainbow Road", "111 Memory Lane", "321 Sesame Street")
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
          selected = NULL,
         # value = "",
          options = list(create = TRUE)
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
            choices = NULL,
            options = list(create = TRUE)
          ),
          textInput2("facility_postal_code", "Postal Code", maxlength = 7),
          selectizeInput("facility_city", "City", choices = cities),
          ## revisit and consider adding flex-wrap: wrap;
          style = "display: flex; justify-content: space-between; max-width: 1000px;"
        ),
        selectInput(
          "outbreak_type",
          "Is this an enteric or respiratory outbreak?",
          c("Enteric", "Respiratory")
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

    vals <- reactiveValues(fac = "herpa")

    observeEvent(input$facility, {
      vals$fac <- "A"
    })
    #facility_address <- textInputServer("facility_address", reactive(input$facility))
    #outbreak_symptom_onset_dt <- dateSelectServer("outbreak_symptom_onset_dt",)
    #outbreak_report_dt <- dateSelectServer("outbreak_report_dt")

    updateFacilites <-
      reactive({
        data <- df
        data <- data[data$facility %in% vals$fac , ]
        updateSelectizeInput(session, "facility_address", choices = data$facility_address,
                             #selected = data$facility_address,
                             server = TRUE)
    })

  }
  shinyApp(ui, server, ...)
}
