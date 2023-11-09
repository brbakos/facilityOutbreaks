outbreakFormInput <- function(id) {

  ns <- NS(id)

  fluidPage(
    ## https://stackoverflow.com/questions/45245681/observe-modal-easy-closing-in-shiny
    tags$script(HTML(
      paste0(
      "$(document).on('shown.bs.modal','#shiny-modal', function () {
          Shiny.setInputValue(id = ", ns("modal_visible"), "value = true);
        });
        $(document).on('hidden.bs.modal','#shiny-modal', function () {
          Shiny.setInputValue(id = ", ns("modal_visible"), "value = false);
        });")
    )),

    div(
      id = ns("form"),
      style = "display: flex; flex-direction: column; align:",

      selectize_input(
        ns("facility"),
        label_mandatory("Facility Name"),
        choices = df$facility,
        selected = NULL
      ),
      "Facility address: ",
      fluidRow(
        selectize_input(
          ns("facility_address"),
          label_mandatory("Street Address"),
          ## consider adding address as a choice...
          ## I'm not sure it's necessary
          choices = NULL
        ),
        selectize_input(ns("facility_postal_code"), "Postal Code", maxlength = 7),
        selectize_input(ns("facility_city"), "City", choices = cities),
        ## revisit and consider adding flex-wrap: wrap;
        style = "display: flex; justify-content: space-between; max-width: 1000px;"
      ),
      selectize_input(
        ns("outbreak_type"),
        "Is this an enteric or respiratory outbreak?",
        choices = c("Enteric", "Respiratory")
      ),
      fluidRow(
        date_select(ns("outbreak_symptom_onset_dt"), "When was the first symptom onset?"),
        date_select(ns("outbreak_report_dt"), "What day was the outbreak reported?"),
        style = "display: flex; justify-content: flex-start;"
      ),
      actionButton(ns("submit-outbreak"), "Submit", class = "btn-primary", style = "max-width: 200px")
    )
  )
}

outbreakFormServer <- function(id) {
  moduleServer(id, function (input, output, session) {
    observeEvent(input$facility, {
      ## only want to fill in the info if the facility is known
      ## otherwise just keep the input as-is
      if (input$facility %in% df$facility) {
        facility <- input$facility

        city <- df$facility_city[df$facility %in% facility]
        address <- df$facility_address[df$facility %in% facility]
        if (length(address) > 1 | length(city) > 1) {

          dupes <- df[df$facility %in% input$facility , ]
          dupes_display <- dupes |> dplyr::pull(united_fac)

          showModal(
            modalDialog(
              title = "Duplicate records for facility",
              HTML(
                paste0(
                  "There is more than one record for ", facility, ".",
                  " Please select the correct record below. <br><br>",
                  "It's also recommended to clean the database so facility names are unique.<br><br>")),
              selectize_input(
                session$ns("duplicate_facilities"),
                "Facilities: ",
                multiple = FALSE,
                choices = dupes_display
              ),
              footer = actionButton(session$ns("dismiss_modal"), label = "Submit Selection")
            )
          )

          observeEvent(session$input$dismiss_modal, {
            #if (input$modal_visible) {
              selected_df <- df[df$united_fac %in% input$duplicate_facilities , ]
              updateSelectizeInput(
                session,
                "facility_address",
                selected = selected_df$facility_address,
                choices = dupes$facility_address
              )
              updateSelectizeInput(
                session,
                "facility_city",
                selected = selected_df$facility_city,
                choices = dupes$facility_city
              )
              removeModal()
          #}
          })
        } else {
          updateSelectizeInput(session, "facility_address", selected = address, choices = address)
          updateSelectizeInput(session, "facility_city", selected = city, choices = city)
        }
      }
    }, ignoreNULL = FALSE)
  })
}
