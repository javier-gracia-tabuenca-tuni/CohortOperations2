

helper_createTestDatabasesHandlers <- function(withEunomiaCohorts = FALSE) {

  cohortOperationsSettings <- yaml::read_yaml(testthat::test_path("config", "test_config.yml"))


  databasesHandlers <- list()
  for(selectedDatabase in names(cohortOperationsSettings)){

    configCohortTableHandler <- cohortOperationsSettings[[selectedDatabase]]$cohortTableHandler

    cohortTableHandler <- helper_createTest_cohortTableHandler(
      configCohortTableHandler = configCohortTableHandler,
      withEunomiaCohorts = withEunomiaCohorts
    )

    databasesHandlers[[selectedDatabase]] <- list(cohortTableHandler = cohortTableHandler)
  }

  return(databasesHandlers)

}




helper_createTest_cohortTableHandler <- function(configCohortTableHandler = NULL, withEunomiaCohorts = FALSE) {

  if(is.null(configCohortTableHandler)){

    config_yaml <- "
    connection:
      connectionDetailsSettings:
          dbms: eunomia
    cdm:
        cdmDatabaseSchema: main
        vocabularyDatabaseSchema: main
    cohortTable:
        cohortDatabaseSchema: main
        cohortTableName: test_cohort_table
  "

    configCohortTableHandler <- yaml::yaml.load(config_yaml)

  }

  connectionHandler <- HadesExtras::ResultModelManager_createConnectionHandler(
    connectionDetailsSettings = configCohortTableHandler$connection$connectionDetailsSettings
  )

  cohortTableHandler <- HadesExtras::CohortTableHandler$new(
    connectionHandler = connectionHandler,
    cdmDatabaseSchema = configCohortTableHandler$cdm$cdmDatabaseSchema,
    vocabularyDatabaseSchema = configCohortTableHandler$cdm$vocabularyDatabaseSchema,
    cohortDatabaseSchema = configCohortTableHandler$cohortTable$cohortDatabaseSchema,
    cohortTableName = configCohortTableHandler$cohortTable$cohortTableName
  )

  if(withEunomiaCohorts==TRUE){
    eunomiaCohorts <- Eunomia::createCohorts(
      connectionDetails = cohortTableHandler$connectionHandler$connectionDetails,
      cohortTable = "cohort"
    )

    sql <- "-- This code inserts records from an external cohort table into the cohort table of the cohort database schema.
            DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;
            INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
            SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
            FROM main.cohort
            WHERE cohort_definition_id = "

    cohortDefinitionSet <- eunomiaCohorts |>
      dplyr::transmute(
        cohortId = as.double(cohortId),
        cohortName = name,
        sql = paste(sql, cohortId),
        json = " "
      )

    cohortTableHandler$insertOrUpdateCohorts(cohortDefinitionSet)
  }

  return(cohortTableHandler)

}
