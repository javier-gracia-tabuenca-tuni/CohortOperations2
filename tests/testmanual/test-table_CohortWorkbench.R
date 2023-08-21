
# setup -------------------------------------------------------------------

source(testthat::test_path("helper.R"))

cohortTableHandler <- helper_createTest_cohortTableHandler(withEunomiaCohorts = TRUE)

cohortSummary <- cohortTableHandler$getCohortsSummary()

workbechCohortsSummary <- cohortSummary |>
  dplyr::mutate( database_name = "Eunomia 1") |>
  dplyr::select(database_name, dplyr::everything())

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

# table -------------------------------------------------------------------

devtools::load_all(".")
table_cohortsWorkbench_reactable(workbechCohortsSummary,  deleteButtonsShinyId = "test")


