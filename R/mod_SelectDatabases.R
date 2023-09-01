mod_selectDatabases_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # imports
    shinyWidgets::useSweetAlert(),
    #
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


mod_selectDatabases_server <- function(id, configurationList, r_connectionHandlers) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r <- shiny::reactiveValues(
      connectionStatusLogs = HadesExtras::LogTibble$new()$logTibble |>
        dplyr::mutate(databaseName = "") |>
        dplyr::relocate(databaseName, .before = 1)
    )


    output$selectDatabases_pickerInput_uiOutput <- shiny::renderUI({

       configurationListChecks <- fct_checkConfigurationList(configurationList)

      if (isTRUE(configurationListChecks)) {
        databaseIdNamesList <- fct_getDatabaseIdNamesListFromConfigurationList(configurationList)

        shinyWidgets::pickerInput(
          inputId = ns("selectDatabases_pickerInput"),
          label = "Select databases to connect:",
          choices = databaseIdNamesList,
          selected = databaseIdNamesList[1],
          multiple = TRUE)
      } else {
        shiny::tagList(
          shiny::h6("Error reading the settings file.", configurationListChecks),
          shiny::h6(configurationListChecks),
          shinyWidgets::pickerInput(
            inputId = ns("selectDatabases_pickerInput"),
            label = "Connect to databases:",
            choices = NULL,
            multiple = TRUE)
        )
      }
    })


    shiny::observeEvent(input$selectDatabases_pickerInput, {

      sweetAlert_spinner("Connecting to databases")

      selectedConfigurationList <- configurationList[input$selectDatabases_pickerInput]

      databasesHandlers <- fct_configurationListToDatabasesHandlers(selectedConfigurationList)
      r_connectionHandlers$databasesHandlers <- databasesHandlers

      remove_sweetAlert_spinner()

    })

    shiny::observeEvent(r_connectionHandlers$databasesHandlers, {

      connectionStatusLogs <- HadesExtras::LogTibble$new()$logTibble |>
        dplyr::mutate(databaseName = "") |>
        dplyr::relocate(databaseName, .before = 1)

      for(databaseId in names(r_connectionHandlers$databasesHandlers)){
        connectionStatusLogs <- dplyr::bind_rows(
          connectionStatusLogs,
          r_connectionHandlers$databasesHandlers[[databaseId]]$cohortTableHandler$connectionStatusLog
        )
      }

      r$connectionStatusLogs <- connectionStatusLogs

    })

    output$connectionStatusLogs_reactable <- reactable::renderReactable({

      r$connectionStatusLogs |>
        HadesExtras::reactable_connectionStatus()
    })


  })
}


