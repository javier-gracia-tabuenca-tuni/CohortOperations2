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
    reactable::reactableOutput(ns("workbechCohortsSummary_reactable"))
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
mod_cohortWorkbench_server <- function(id, r_connectionHandlers, r_workbechCohortsSummary,  table_editing=TRUE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #
    # Renders workbechCohortsSummary_reactable
    #
    output$workbechCohortsSummary_reactable <- reactable::renderReactable({

      r_workbechCohortsSummary$workbechCohortsSummary |>
        table_cohortsWorkbench_reactable(deleteButtonsShinyId = ns("cohortsWorkbenchDeleteButtons"))

    })

    #
    # Ask for confirmation when delete button is clicked
    #
    shiny::observeEvent(input$cohortsWorkbenchDeleteButtons, {

      rowNumber <- input$cohortsWorkbenchDeleteButtons$index
      database_name <- r_workbechCohortsSummary$workbechCohortsSummary |> purrr::pluck("database_name", rowNumber)
      cohortName <- r_workbechCohortsSummary$workbechCohortsSummary |> purrr::pluck("cohortName", rowNumber)

      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = ns("confirmSweetAlert_CohortsWorkbenchDeleteButtons"),
        type = "question",
        title = "Delete cohort ?",
        text = htmltools::HTML(paste0(
          "Are you sure you want to delete cohort '", cohortName, "' from database '", database_name, "' ?"
        )),
        btn_labels = c("Cancel", "Delete"),
        html = TRUE
      )

    })

    #
    # If delete confirmation accepted, deletes cohort and updates r_workbechCohortsSummary
    #
    shiny::observeEvent(input$confirmSweetAlert_CohortsWorkbenchDeleteButtons, {
      if (input$confirmSweetAlert_CohortsWorkbenchDeleteButtons == TRUE) {
        rowNumber <- input$cohortsWorkbenchDeleteButtons$index
        database_name <- r_workbechCohortsSummary$workbechCohortsSummary |> purrr::pluck("database_name", rowNumber)
        cohortId <- r_workbechCohortsSummary$workbechCohortsSummary |> purrr::pluck("cohortId", rowNumber)

        cohortTableHandler <- r_connectionHandlers$databasesHandlers[[database_name]]$cohortTableHandler
        cohortTableHandler$deleteCohorts(as.integer(cohortId))

        r_workbechCohortsSummary$workbechCohortsSummary <- fct_createCohortWorkbenchTableFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      }
    })







  })
}
