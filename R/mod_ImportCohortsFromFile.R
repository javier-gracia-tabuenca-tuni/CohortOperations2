#' import_cohort_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS fileInput actionButton
#' @importFrom htmltools tagList hr
#' @importFrom shinyjs useShinyjs
#' @importFrom reactable reactableOutput
mod_importCohortsFromFile_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    mod_appendCohort_ui(),
    shinyjs::useShinyjs(),
    #
    shiny::uiOutput(ns("selectDatabase_pickerInput_uiOutput")),
    shiny::fileInput(ns("uploadedFile"), "Choose a file in cohortData format:",
                     multiple = FALSE,
                     accept = c("text/tsv", "text/tabular-separated-values,text/plain", ".tsv")
    ),
    htmltools::hr(),
    reactable::reactableOutput(ns("cohorts_reactable")), # %>% ui_load_spiner(),
    htmltools::hr(),
    shiny::actionButton(ns("import_actionButton"), "Import Selected")
  )
}

#' import_cohort_file Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer reactiveValues observe req validate need observeEvent
#' @importFrom reactable renderReactable reactable getReactableState updateReactable
#' @importFrom tools file_ext
#' @importFrom readr read_tsv
#' @importFrom utils hasName
#' @importFrom dplyr mutate distinct semi_join slice
#' @importFrom FinnGenTableTypes is_cohortData as_cohortData
#' @importFrom stringr str_c
#' @importFrom shinyjs toggleState reset
mod_importCohortsFromFile_server <- function(id, r_connectionHandlers, r_workbechCohortsSummary) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      uploadedFile = NULL,
      cohortDefinitionSetImported = NULL
    )

    r_toAdd <- shiny::reactiveValues(
      databaseName = NULL,
      cohortDefinitionSet = NULL
    )

    #
    # just pass the info to make it writable
    #
    shiny::observe({
      r$uploadedFile <- input$uploadedFile
    })

    #
    # render selectDatabase_pickerInput
    #
    output$selectDatabase_pickerInput_uiOutput <- shiny::renderUI({
      databaseNames <- names(r_connectionHandlers$databasesHandlers)

      if (length(databaseNames)==0) { databaseNames <- NULL }

      shinyWidgets::pickerInput(
        inputId = ns("selectDatabase_pickerInput"),
        label = "Link patients to database:",
        choices = databaseNames,
        selected = databaseNames[1],
        multiple = FALSE)
    })

    #
    # updates r$cohortDefinitionSetImported with uploaded file, or with error
    #
    shiny::observe({
      shiny::req(r$uploadedFile)
      shiny::req(input$selectDatabase_pickerInput)

      ext <- tools::file_ext(r$uploadedFile$datapath)

      # passing error to shiny::validate
      if(ext != "tsv" & ext != "csv"){
        r$cohortDefinitionSetImported <- "ERROR READING FILE:\nI need to know if the file is in .tsv or .csv format, please set the extension acordingly"
        return()
      }

      if(ext == "tsv"){ cohortData <- HadesExtras::readCohortData(r$uploadedFile$datapath, delim = "\t") }
      if(ext == "csv"){ cohortData <- HadesExtras::readCohortData(r$uploadedFile$datapath, delim = ",") }


      isCohortData <- HadesExtras::checkCohortData(cohortData)

      # passing error to shiny::validate
      if(is.character(isCohortData)){
        r$cohortDefinitionSetImported <- paste(c("ERROR READING COHORTDATA FILE:", isCohortData), sep = "\n")
        return()
      }

      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabase_pickerInput]]$cohortTableHandler
      cohortDefinitionSet <- HadesExtras::cohortDataToCohortDefinitionSet(
        connection = cohortTableHandler$connectionHandler$getConnection(),
        cdmDatabaseSchema = cohortTableHandler$cdmDatabaseSchema,
        cohortData = cohortData
      )

      r$cohortDefinitionSetImported <- cohortDefinitionSet
    })

    #
    # updates output$cohorts_reactable with r$cohortDefinitionSetImported
    #
    output$cohorts_reactable <- reactable::renderReactable({
      shiny::req(r$cohortDefinitionSetImported)
      shiny::req(input$selectDatabase_pickerInput)

      shiny::validate(
        shiny::need(!is.character(r$cohortDefinitionSetImported), r$cohortDefinitionSetImported)
      )

      table_importCohortsFromFile_reactable(r$cohortDefinitionSetImported, input$selectDatabase_pickerInput)

    })

    # reactive function to get selected values
    r_selectedIndex <- reactive(reactable::getReactableState("cohorts_reactable", "selected", session))

    #
    # button import selected: checks selected cohorts
    #
    shiny::observe({
      shinyjs::toggleState("import_actionButton", condition = !is.null(r_selectedIndex()) )
    })

    shiny::observeEvent(input$import_actionButton, {
      shiny::req(r_selectedIndex())
      shiny::req(r$cohortDefinitionSetImported)
      shiny::req(input$selectDatabase_pickerInput)

      ## copy selected to
      r_toAdd$databaseName <- input$selectDatabase_pickerInput
      r_toAdd$cohortDefinitionSet <- r$cohortDefinitionSetImported |>
        dplyr::slice(r_selectedIndex())
    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    r_append_accepted_counter <- mod_appendCohort_server("impor_file", r_connectionHandlers, r_workbechCohortsSummary, r_toAdd )

    # close and reset
    shiny::observeEvent(r_append_accepted_counter(), {
      shinyjs::reset("uploadedFile")
      r$uploadedFile <- NULL
      r$cohortDefinitionSetImported <- NULL
      r_toAdd$cohortDefinitionSet <- NULL
      reactable::updateReactable("cohorts_reactable", selected = NA, session = session )
      r$cohortDefinitionSetImported <- NULL
    })

  })
}
