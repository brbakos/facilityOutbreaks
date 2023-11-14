outbreakFormInput <- function(id) {

  multiple_inputs_row_style <- "display: inline-flex; flex-wrap: wrap;"
  tab_tag <- span(class = "tab", "")

  ns <- NS(id)

  fluidPage(

    h1("Facility Outbreak Entry Form"),
    ## for some reason when the modal is opened and closed the first time,
    ## it will instantly close if opened a second time
    ## the third time it works, but that's not acceptable for users
    ## this JS will make sure the modal always opens and closes when a duplicate facility is selected
    ## (provided the user changed the facility in between)
    tags$head(tags$script(HTML(
      paste0(
        "$(document).on('shown.bs.modal','.modal-dupes', function () {
          Shiny.setInputValue(id = '", ns("modal_dupe_visible"), "', value = true);
        });
        $(document).on('hidden.bs.modal','.modal-dupes', function () {
          Shiny.setInputValue(id = '", ns("modal_dupe_visible"), "', value = false);
        });")
    ))),

    div(
      id = ns("form"),
      style = "display: inline-flex; flex-direction: column; flex-wrap: wrap;",

      fluidRow(
        div(
        selectize_input(
          ns("facility"),
          label_mandatory("Facility Name"),
          choices = df$facility,
          width = "100%",
          selected = NULL
        ))
      ),

      h3("Facility Information"),
      fluidRow(
        selectize_input(
          ns("facility_address"),
          label_mandatory("Street Address"),
          ## consider adding address as a choice...
          ## I'm not sure it's necessary
          choices = NULL
        ),
        tab_tag,
        selectize_input(
          ns("facility_postal_code"),
          "Postal Code",
          maxlength = 7
        ),
        tab_tag,
        selectize_input(
          ns("facility_city"),
          "City",
          choices = cities
        ),
        style = multiple_inputs_row_style
      ),

      h3("Outbreak Information"),
      fluidRow(
        ## we ask for enteric vs resp so GI or resp illness can be specified
        ## if the organism is unknown
        selectize_input(
          ns("outbreak_type"),
          "Is this an enteric or respiratory outbreak?",
          choices = c("Enteric", "Respiratory")
        )
      ),
      fluidRow(
        selectize_input(
          ns("organism"),
          "Organism",
          ## ----- NOTE ----:
          ## there should be additional lookup sheet for organisms
          ## that way when new ones show up they should be added
          choices = c("Influenza", "COVID-19", "Norovirus")
        ),
        tab_tag,
        div(id = "subtype_placeholder"),
        style = multiple_inputs_row_style
      ),
      fluidRow(
        date_select(
          ns("outbreak_symptom_onset_dt"),
          "When was the first symptom onset?"
        ),
        tab_tag,
        date_select(
          ns("outbreak_report_dt"),
          "What day was the outbreak reported?"
        ),
        style = multiple_inputs_row_style
      ),

      actionButton(
        ns("submit_outbreak"),
        "Submit",
        class = "btn-primary",
        style = "max-width: 200px"
      )
    )
  )
}

outbreakFormServer <- function(id) {
  moduleServer(id, function (input, output, session) {

    ## when a known facility is selected, we want to auto-fill the elements
    ## we also want users to be aware of duplicate facility names
    observeEvent(input$facility, {
      if (input$facility %in% df$facility) {

        our_facility <- df[df$facility %in% input$facility , ]
        our_facility <- as.list(our_facility)

        if (any(sapply(our_facility, function(x) length(x) > 1))) {

          dupes <- do.call(dplyr::bind_cols, our_facility)
          dupes_display <- dupes |> dplyr::pull(united_fac)

          modal_dupes <-
            modalDialog(
              title = "Duplicate records for facility",
              easyClose = TRUE,
              HTML(
                paste0(
                  "There is more than one record for ", input$facility, ".",
                  " Please select the correct record below. <br><br>",
                  "It's also recommended to clean the database so facility names are unique.<br><br>"
                )
              ),
              selectize_input(
                session$ns("duplicate_facilities"),
                "Addresses: ",
                multiple = FALSE,
                choices = dupes_display,
                width = "100%"
              ),
              footer = actionButton(session$ns("dismiss_modal"), label = "Submit Selection")
            )
          modal_dupes <- tagAppendAttributes(modal_dupes, class = "modal-dupes")

          showModal(modal_dupes)

          observeEvent(session$input$dismiss_modal, {
            if (input$modal_dupe_visible) {
              selected_df <- df[df$united_fac %in% input$duplicate_facilities , ]

              updateSelectizeInput(
                session,
                "facility_address",
                selected = selected_df$facility_address,
                choices = dupes$facility_address
              )
              updateSelectizeInput(
                session,
                "facility_postal_code",
                selected = selected_df$facility_postal_code,
                choices = dupes$facility_postal_code
              )
              updateSelectizeInput(
                session,
                "facility_city",
                selected = selected_df$facility_city,
                choices = dupes$facility_city
              )
              removeModal()
           }
        }) } else {
          updateSelectizeInput(
            session,
            "facility_address",
            selected = our_facility$facility_address,
            choices = our_facility$facility_address
          )
          updateSelectizeInput(
            session,
            "facility_postal_code",
            selected = our_facility$facility_postal_code,
            choices = our_facility$facility_postal_code
          )
          updateSelectizeInput(
            session,
            "facility_city",
            selected = our_facility$facility_city,
            choices = our_facility$facility_city
          )
        }
      }
    }, ignoreNULL = FALSE)

    observeEvent(input$submit_outbreak, {

    })
  })
}
