# setup ---------------------------------------------------------------------

source(testthat::test_path("helper.R"))

cohortDataTestFile <- "cohortData_eunomia.csv"
cohortDataTestFile <- "cohortData_eunomia_nostartdate.csv"
#cohortDataTestFile <- "cohortData_eunomia_missingids.csv"

cohortTableHandler <- helper_createTest_cohortTableHandler(withEunomiaCohorts = TRUE)
cohortData <- HadesExtras::readCohortData(testthat::test_path("testdata", cohortDataTestFile))


cohortDefinitionSet <- HadesExtras::cohortDataToCohortDefinitionSet(
  connection = cohortTableHandler$connectionHandler$getConnection(),
  cdmDatabaseSchema = cohortTableHandler$cdmDatabaseSchema,
  cohortData = cohortData
)


# run ---------------------------------------------------------------------

devtools::load_all(".")

table_importCohortsFromFile_reactable(cohortDefinitionSet, "Database X")
