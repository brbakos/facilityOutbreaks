#' Facility Outbreak Form Module
#'
#' @description Entry form for a new facility outbreak
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
outbreakFormInput <- function(id) {

  multiple_inputs_row_style <- "display: inline-flex; flex-wrap: wrap;"
  tab_tag <- span(class = "tab", "")

  ns <- NS(id)

  fluidPage(

    h1("Facility Outbreak Entry Form"),
    ## for some reason when the modal is opened and closed the first time,
    ## it will instantly close if opened a second time
    ## the third time it works, but that's not acceptable for users
    ## I assume this is somehow my fault, but
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

      #' *Facility info*
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
          label_mandatory("City"),
          choices = cities
        ),
        style = multiple_inputs_row_style
      ),

      #' *Outbreak info*
      h3("Outbreak Information"),
      fluidRow(
        ## we ask for enteric vs resp so GI or resp illness can be specified
        ## if the organism is unknown
        selectize_input(
          ns("outbreak_type"),
          label_mandatory("Is this an enteric or respiratory outbreak?"),
          choices = c("Enteric", "Respiratory")
        )
      ),
      fluidRow(
        selectize_input(
          ns("pathogen"),
          "Pathogen",
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

      fluidRow(
        div(id = ns("additional_units")),
        div(
          actionButton(
            inputId = ns("add_floor"),
            style   = "border: 0px",
            label   = NULL,
            icon("circle-plus")
          )
        ),
      ),

      #' *Submit*
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
  ns <- NS(id)
  moduleServer(id, function (input, output, session) {

    ## for adding floors/units affected
    count <- reactiveVal(0)
    my_list <- tagList()
    count(0)
    multiple_inputs_row_style <- "display: inline-flex; flex-wrap: wrap;"
    tab_tag <- span(class = "tab", "")

    ## when a known facility is selected, we want to auto-fill the elements
    ## we also want users to be aware of duplicate facility names
    observeEvent(input$facility, {

      our_facility <- df[df$facility %in% input$facility , ]
      our_facility <- as.list(our_facility)

      if (input$facility %in% df$facility) {

        ## if facility selected is duplicated
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
            ## if facility selected is not a duplicate
        }) } else if (all(sapply(our_facility, function(x) length(x) == 1))) {
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
        ## Facility is not in the known list
      } else {
        updateSelectizeInput(
          session,
          "facility_address",
          selected = "",
          choices = df$facility_address
        )
        updateSelectizeInput(
          session,
          "facility_postal_code",
          selected = "",
          choices = df$facility_postal_code
        )
        updateSelectizeInput(
          session,
          "facility_city",
          selected = "",
          choices = df$facility_city
        )
      }
    }, ignoreNULL = FALSE)

    add_item <- function(count) {
      div(
        id = paste0("div_", count),
        style = "display: inline-flex; flex-direction: column; flex-wrap: wrap;",
        fluidRow(
          textInput(
            inputId     = paste0("floor", count),
            label       = paste("Unit/Floor", count()),
            width       = NULL,
            placeholder = " "
          ),
          tab_tag,
          checkboxInput(
            inputId = paste0("floor_active", count),
            label = paste("Currently Active"),
            value = TRUE
          ),
          style = multiple_inputs_row_style
        )
        # selectize_input(
        #   id = paste0("add_id", count),
        #   label = paste("ID", count())
        # ),
      )
    }

    observeEvent(input$add_floor, {
      count(count() + 1)
      insertUI(selector = paste0("#", ns("additional_units")), ui = add_item(count()))
    })


    observeEvent(input$submit_outbreak, {
      if (!input$facility %in% df$facility) {
        new_facility <-
          data.frame(
            facility = input$facility,
            facility_address = input$facility_address,
            facility_postal_code = input$facility_postal_code,
            facility_city = input$facility_city
          )

        df <- dplyr::bind_rows(df, new_facility)

        print(df)
      }
    })
  })
}
