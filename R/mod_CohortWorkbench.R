#' cohorts_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS actionButton downloadButton
#' @importFrom htmltools tagList hr
#' @importFrom shinyjs useShinyjs
#' @importFrom reactable reactableOutput
mod_cohortWorkbench_ui <- function(id){
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyWidgets::useSweetAlert(),
    #
    reactable::reactableOutput(ns("cohortsSummaryDatabases_reactable"))
  )
}

#' cohorts_table Server Functions
#'
#' @noRd
#' @importFrom reactable renderReactable getReactableState
#' @importFrom FinnGenTableTypes table_summarycohortData
#' @importFrom shinyjs toggleState
#' @importFrom shiny observeEvent req downloadHandler
#' @importFrom dplyr slice pull filter
#' @importFrom shinyWidgets confirmSweetAlert
#' @importFrom htmltools HTML
#' @importFrom stringr str_c
#' @importFrom readr write_tsv
mod_cohortWorkbench_server <- function(id, r_connectionHandlers, r_workbench,  table_editing=TRUE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #
    # Renders cohortsSummaryDatabases_reactable
    #
    output$cohortsSummaryDatabases_reactable <- reactable::renderReactable({
      r_workbench$cohortsSummaryDatabases |>
        HadesExtras::rectable_cohortsSummary(deleteButtonsShinyId = ns("cohortsWorkbenchDeleteButtons"))
    })

    #
    # Ask for confirmation when delete button is clicked
    #
    shiny::observeEvent(input$cohortsWorkbenchDeleteButtons, {

      rowNumber <- input$cohortsWorkbenchDeleteButtons$index
      databaseName <- r_workbench$cohortsSummaryDatabases |> purrr::pluck("databaseName", rowNumber)
      cohortName <- r_workbench$cohortsSummaryDatabases |> purrr::pluck("cohortName", rowNumber)
      shortName <- r_workbench$cohortsSummaryDatabases |> purrr::pluck("shortName", rowNumber)

      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = ns("confirmSweetAlert_CohortsWorkbenchDeleteButtons"),
        type = "question",
        title = "Delete cohort ?",
        text = htmltools::HTML(paste0(
          "Are you sure you want to delete cohort<br>", shortName,": '", cohortName, "'<br>from database<br>'", databaseName, "' ?"
        )),
        btn_labels = c("Cancel", "Delete"),
        html = TRUE
      )

    })

    #
    # If delete confirmation accepted, deletes cohort and updates r_workbench
    #
    shiny::observeEvent(input$confirmSweetAlert_CohortsWorkbenchDeleteButtons, {
      if (input$confirmSweetAlert_CohortsWorkbenchDeleteButtons == TRUE) {
        rowNumber <- input$cohortsWorkbenchDeleteButtons$index
        databaseName <- r_workbench$cohortsSummaryDatabases |> purrr::pluck("databaseName", rowNumber)
        cohortId <- r_workbench$cohortsSummaryDatabases |> purrr::pluck("cohortId", rowNumber)
        databaseId <- fct_getDatabaseIdNamesListFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)[[databaseName]]

        cohortTableHandler <- r_connectionHandlers$databasesHandlers[[databaseId]]$cohortTableHandler
        cohortTableHandler$deleteCohorts(as.integer(cohortId))

        r_workbench$cohortsSummaryDatabases <- fct_getCohortsSummariesFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      }
    })







  })
}
