mod_selectDatabases_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h2("Databases connection"),
    shiny::br(),
    shiny::p("CohortOperations can connect to one or more databases. By default, it will connect only to the top database."),
    shiny::br(),
    shiny::uiOutput(ns("selectDatabases_pickerInput_uiOutput")),
    shiny::br(),
    shiny::h4("Connection status"),
    shiny::p("This table shows the connected databases."),
    reactable::reactableOutput(ns("connectionStatusLogs_reactable"))
  )
}

#' select_configuration Server Functions
#'
#' @noRd
#' @importFrom reactable renderReactable reactable colDef
#' @importFrom shiny observeEvent
#' @importFrom dplyr bind_rows
#'
mod_selectDatabases_server <- function(id, cohortOperationsSettings, r_connectionHandlers) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r <- shiny::reactiveValues(
      connectionStatusLogs = HadesExtras::LogTibble$new()$logTibble,
    )

    output$selectDatabases_pickerInput_uiOutput <- shiny::renderUI({
      settingsNames <- names(cohortOperationsSettings)

      if (length(settingsNames)==0) {
        shiny::tagList(
        shiny::h6("Error reading the settings file."),
        shinyWidgets::pickerInput(
          inputId = ns("selectDatabases_pickerInput"),
          label = "Connect to databases:",
          choices = NULL,
          multiple = TRUE)
        )
      } else {
        shinyWidgets::pickerInput(
          inputId = ns("selectDatabases_pickerInput"),
          label = "Select databases to connect:",
          choices = settingsNames,
          selected = settingsNames[1],
          multiple = TRUE)
      }
    })



    shiny::observeEvent(input$selectDatabases_pickerInput, {

      databasesHandlers <- list()
      for(selectedDatabase in input$selectDatabases_pickerInput){

        config <- cohortOperationsSettings[[selectedDatabase]]$cohortTableHandler

        connectionHandler <- HadesExtras::ResultModelManager_createConnectionHandler(
          connectionDetailsSettings = config$connection$connectionDetailsSettings,
          tempEmulationSchema = config$connection$tempEmulationSchema
        )
        cohortTableHandler <- HadesExtras::CohortTableHandler$new(
          connectionHandler = connectionHandler,
          cdmDatabaseSchema = config$cdm$cdmDatabaseSchema,
          vocabularyDatabaseSchema = config$cdm$vocabularyDatabaseSchema,
          cohortDatabaseSchema = config$cohortTable$cohortDatabaseSchema,
          cohortTableName = config$cohortTable$cohortTableName
        )

        databasesHandlers[[selectedDatabase]] <- list(cohortTableHandler = cohortTableHandler)
      }

      r_connectionHandlers$databasesHandlers <- databasesHandlers

    })



    shiny::observeEvent(r_connectionHandlers$databasesHandlers, {

      connectionStatusLogs <- HadesExtras::LogTibble$new()$logTibble
      for(selectedDatabase in names(r_connectionHandlers$databasesHandlers)){

        connectionStatusLog <- r_connectionHandlers$databasesHandlers[[selectedDatabase]]$cohortTableHandler$connectionStatusLog$logTibble
        connectionStatusLogs <- dplyr::bind_rows(
          connectionStatusLogs,
          connectionStatusLog |> dplyr::mutate(database = selectedDatabase)
          )
      }

      r$connectionStatusLogs <- connectionStatusLogs

    })

    output$connectionStatusLogs_reactable <- reactable::renderReactable({
          r$connectionStatusLogs |>
        table_connectionStatus_reactable()
      })


  })
}


