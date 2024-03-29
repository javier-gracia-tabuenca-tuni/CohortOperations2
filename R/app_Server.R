#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # get settings loaded from file
  configurationList <- shiny::getShinyOption("configurationList")

  # list of connection handlers that are passed to modules,
  # not all modules used all, they modules check the list has at least the ones they need
  # they are produced only by mod_select_configuration and consumed by the modules
  r_connectionHandlers <- shiny::reactiveValues(
    cohortTableHandler = NULL
  )

  # produced by modules related to cohort editing
  # consumed by cohort table viewer
  r_workbench <- shiny::reactiveValues(
    cohortsSummaryDatabases = HadesExtras::createEmptyCohortsSummary()
  )


  mod_selectDatabases_server("selectDatabases", configurationList, r_connectionHandlers)

  mod_cohortWorkbench_server("cohortWorkbench_importCohorts", r_connectionHandlers, r_workbench)
  mod_importCohortsFromFile_server("importCohortsFromFile", r_connectionHandlers, r_workbench)

  mod_cohortWorkbench_server("cohortWorkbench_matchCohorts", r_connectionHandlers, r_workbench)
  mod_matchCohorts_server("matchCohorts", r_connectionHandlers, r_workbench)


  mod_cohortWorkbench_server("cohortWorkbench_timeCodeWAS", r_connectionHandlers, r_workbench)
  mod_timeCodeWAS_server("timeCodeWAS", r_connectionHandlers)

  mod_cohortWorkbench_server("cohortWorkbench_cohortsIncidence", r_connectionHandlers, r_workbench)
  mod_cohortsIncidence_server("cohortsIncidence", r_connectionHandlers)

}


