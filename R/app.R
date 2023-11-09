df <-
  data.frame(
    facility = c("SUPERCALLAFRAGILISTICEXPIALADOCIOUS", "SUPERCALLAFRAGILISTICEXPIALADOCIOUS", "B", "C"),
    facility_address = c("123 Rainbow Road", "401 Not Found St", "111 Memory Lane", "321 Sesame Street"),
    facility_city = c("Vancouver", "The Web", "Paris", "foo")
  )
df <- df |> tidyr::unite("united_fac", tidyr::everything(), sep = ",", remove = FALSE)

fields_mandatory <- c("facility_name", "facility_address", "outbreak_report_dt")

cities <- c("Vancouver", "Richmond", "Sechelt", "North Vancouver")

## note to self - the attempt to address dupes current blocks all entry after;
## must be a problem with input$ for the facility or something
outbreakApp <- function(...) {
  ui <-
    fluidPage(

      shinyjs::useShinyjs(),
      shinyjs::inlineCSS(app_css),


      ## https://stackoverflow.com/questions/45245681/observe-modal-easy-closing-in-shiny
      tags$script(HTML(
        "$(document).on('shown.bs.modal','#shiny-modal', function () {
          Shiny.setInputValue(id = 'modal_visible', value = true);
        });
        $(document).on('hidden.bs.modal','#shiny-modal', function () {
          Shiny.setInputValue(id = 'modal_visible', value = false);
        });"
      )),

      div(
        id = "form",
        style = "display: flex; flex-direction: column;",

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
            ## consider adding address as a choice...
            ## I'm not sure it's necessary
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
          date_select("outbreak_symptom_onset_dt", "When was the first symptom onset?"),
          date_select("outbreak_report_dt", "What day was the outbreak reported?"),
          style = "display: flex; justify-content: flex-start;"
        ),
        actionButton("submit", "Submit", class = "btn-primary")
      )
    )
  server <- function(input, output, session) {

    values <- reactiveValues(modal_closed = T)

    observeEvent(input$facility, {
      ## only want to fill in the info if the facility is known
      ## otherwise just keep the input as-is
      if (input$facility %in% df$facility & values$modal_closed) {
          facility <- input$facility

          city <- df$facility_city[df$facility %in% facility]
          address <- df$facility_address[df$facility %in% facility]
          if (length(address) > 1 | length(city) > 1) {

            values$modal_closed <- F

            dupes <- df[df$facility %in% input$facility , ]
            dupes_display <- dupes |> dplyr::pull(united_fac)

            showModal(
              modalDialog(
                title = "Duplicate records for facility",
                HTML(paste0("There is more than one record for ", facility, ".",
                         " Please select the correct record below. <br><br>",
                         "It's also recommended to clean the database so facility names are unique.<br>")),
                selectize_input("duplicate_facilities", "Facilities: ", multiple = FALSE, choices = dupes_display),
                footer = actionButton("dismiss_modal", label = "Submit Selection")
                )
              )

            observeEvent(input$dismiss_modal, {
              if (input$modal_visible) {
                selected_df <- df[df$united_fac %in% input$duplicate_facilities , ]
                updateSelectizeInput(session, "facility_address", selected = selected_df$facility_address, choices = selected_df$facility_address)
                updateSelectizeInput(session, "facility_city", selected = selected_df$facility_city, choices = selected_df$facility_city)
                tags$script(HTML("Shiny.setInputValue('duplicate_facilities', null);
                               Shiny.setInputValue('dismiss_modal', null);"))
                removeModal()
              }
            })
          } else {
            updateSelectizeInput(session, "facility_address", selected = address, choices = address)
            updateSelectizeInput(session, "facility_city", selected = city, choices = city)
          }
      }
    }, ignoreNULL = FALSE)

  }
  shinyApp(ui, server, ...)
}
