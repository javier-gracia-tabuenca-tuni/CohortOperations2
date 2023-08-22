


helper_createNewDatabaseHandlers <- function(withEunomiaCohorts = FALSE) {

  databasesHandlers <- list()
  for(selectedDatabase in names(cohortOperationsSettings)){

    configCohortTableHandler <- cohortOperationsSettings[[selectedDatabase]]$cohortTableHandler

    cohortTableHandler <- helper_createNewCohortTableHandler(
      configCohortTableHandler = configCohortTableHandler,
      withEunomiaCohorts = withEunomiaCohorts
    )

    databasesHandlers[[selectedDatabase]] <- list(cohortTableHandler = cohortTableHandler)
  }

  return(databasesHandlers)

}



helper_createNewCohortTableHandler <- function(configCohortTableHandler, withEunomiaCohorts = FALSE){
  connectionHandler <- HadesExtras::ResultModelManager_createConnectionHandler(
    connectionDetailsSettings = configCohortTableHandler$connection$connectionDetailsSettings,
    tempEmulationSchema = configCohortTableHandler$connection$tempEmulationSchema
  )
  cohortTableHandler <- HadesExtras::CohortTableHandler$new(
    connectionHandler = connectionHandler,
    databaseName = configCohortTableHandler$databaseName,
    cdmDatabaseSchema = configCohortTableHandler$cdm$cdmDatabaseSchema,
    vocabularyDatabaseSchema = configCohortTableHandler$cdm$vocabularyDatabaseSchema,
    cohortDatabaseSchema = configCohortTableHandler$cohortTable$cohortDatabaseSchema,
    cohortTableName = configCohortTableHandler$cohortTable$cohortTableName
  )

  if(withEunomiaCohorts==TRUE){

    cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
      settingsFileName = "testdata/name/Cohorts.csv",
      jsonFolder = "testdata/name/cohorts",
      sqlFolder = "testdata/name/sql/sql_server",
      cohortFileNameFormat = "%s",
      cohortFileNameValue = c("cohortName"),
      packageName = "CohortGenerator",
      verbose = FALSE
    )

    cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSet)
  }

  return(cohortTableHandler)

}

