#' mod_appendCohort_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets useSweetAlert
mod_appendCohort_ui <- function() {
  shinyWidgets::useSweetAlert()
}


#' server_appendCohort Server Functions
#'
#' @noRd
#' @importFrom shiny reactiveValues observe observeEvent
#' @importFrom dplyr inner_join distinct mutate pull anti_join bind_rows
#' @importFrom stringr str_c
#' @importFrom shinyWidgets confirmSweetAlert
#' @importFrom htmltools HTML
#' @importFrom FinnGenTableTypes summarise_cohortData
mod_appendCohort_server <- function(id, r_connectionHandlers, r_workbench, r_toAdd ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      replaceQuestion = NULL,
      appendAcceptedCounter = 0
    )

    #
    # if r_appendCohort is modified
    #
    observeEvent(r_toAdd$cohortDefinitionSet, {
      req(r_toAdd$databaseName)
      req(r_toAdd$cohortDefinitionSet)

      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[r_toAdd$databaseName]]$cohortTableHandler

      # ask if existing cohorts should be replaced
      namesExistInWorkbech <- intersect(
        cohortTableHandler$getCohortIdAndNames() |> dplyr::pull(cohortName),
        r_toAdd$cohortDefinitionSet |> dplyr::pull(cohortName)
      )

      if (length(namesExistInWorkbech) > 0) {
        shinyWidgets::confirmSweetAlert(
          session = session,
          inputId = "replaceQuestion_alert",
          type = "question",
          title = "Some selected cohorts had been alredy imported:",
          text = htmltools::HTML(
            "The following cohorts had been alredy imported for database: ", r_toAdd$databaseName, " <ul>",
            paste0(paste0("<li> ", namesExistInWorkbech, "</li>"), collapse = "\n"),
            "</ul> Should these be replaced or ignored."
          ),
          btn_labels = c("Not-import", "Replace"),
          html = TRUE
        )
      }else{
        r$replaceQuestion <- TRUE
      }
    })
    ## just pass the info to make it writable
    shiny::observe({
      r$replaceQuestion <- input$replaceQuestion_alert
    })

    #
    # confirmSweetAlert replaceQuestion_alert
    #
    shiny::observeEvent(r$replaceQuestion, {
      req(r_toAdd$databaseName)
      req(r_toAdd$cohortDefinitionSet)
      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[r_toAdd$databaseName]]$cohortTableHandler

      if(r$replaceQuestion){
        cohortTableHandler$insertOrUpdateCohorts(r_toAdd$cohortDefinitionSet)
      }

      # update r_workbench
      r_workbench$cohortsSummaryDatabases <- fct_getCohortsSummariesFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      # reset module
      r$replaceQuestion <- NULL

      # pass action
      r$appendAcceptedCounter <- r$appendAcceptedCounter+1
    })


    return(reactive(r$appendAcceptedCounter))
  })
}
