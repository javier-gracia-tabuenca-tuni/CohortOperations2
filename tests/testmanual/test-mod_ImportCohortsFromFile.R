

# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("helper.R"))


databasesHandlers <- helper_createTestDatabasesHandlers(withEunomiaCohorts = TRUE)

workbechCohortsSummary <- fct_databasesHandlersToWorkbechCohortsSummary(databasesHandlers)

r_connectionHandlers <- shiny::reactiveValues(
  databasesHandlers = databasesHandlers
)

r_workbechCohortsSummary <- shiny::reactiveValues(
  workbechCohortsSummary = workbechCohortsSummary
)

# run module --------------------------------------------------------------
devtools::load_all(".")

shiny::shinyApp(
  shiny::fluidPage(
    mod_importCohortsFromFile_ui("test")
  ),
  function(input,output,session){
    mod_importCohortsFromFile_server("test", r_connectionHandlers, r_workbechCohortsSummary)
  }
)



# connectionStatus_reactable ----------------------------------------------
# devtools::load_all(".")
#
# log <- HadesExtras::LogTibble$new()
# log$INFO("step 1", "example info")
# log$WARNING("step 2", "example warning")
# log$ERROR("step 3", "example error")
#
# connectionStatusLogs <- log$logTibble |> dplyr::mutate(database="Database name")
#
# connectionStatus_reactable(connectionStatusLogs)
