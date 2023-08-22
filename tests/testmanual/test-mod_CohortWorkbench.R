

# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))


databasesHandlers <- helper_createTestDatabasesHandlers(withEunomiaCohorts = TRUE)

workbechCohortsSummary <- fct_createCohortWorkbenchTableFromDatabasesHandlers(databasesHandlers)

# make one empty
workbechCohortsSummary <- workbechCohortsSummary |>
  dplyr::mutate(
    cohortEntries = dplyr::if_else(cohortId == 1, NA, cohortEntries),
    cohortSubjects = dplyr::if_else(cohortId == 1, NA, cohortSubjects),
    histogram_cohort_start_year = dplyr::if_else(cohortId == 1, NA, histogram_cohort_start_year),
    histogram_cohort_end_year = dplyr::if_else(cohortId == 1, NA, histogram_cohort_end_year),
    count_sex = dplyr::if_else(cohortId == 1, NA, count_sex)
  ) |>
  HadesExtras::correctEmptyCohortsInCohortsSummary()


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
    mod_cohortWorkbench_ui("test")
  ),
  function(input,output,session){
    mod_cohortWorkbench_server("test", r_connectionHandlers, r_workbechCohortsSummary)
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
