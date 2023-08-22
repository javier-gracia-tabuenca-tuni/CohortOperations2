

fct_getCohortsSummariesFromDatabasesHandlers <- function(databasesHandlers){

  databasesHandlers |> checkmate::assertList(null.ok = TRUE)

  if(is.null(databasesHandlers)){
    return(HadesExtras::createEmptyCohortsSummary())
  }

  CohortsSummaryDatabases <- HadesExtras::createEmptyCohortsSummary()
  for(name in names(databasesHandlers)) {

    CohortsSummaryDatabases <- dplyr::bind_rows(
      CohortsSummaryDatabases,
      databasesHandlers[[name]]$cohortTableHandler$getCohortsSummary()
    )

  }
  return(CohortsSummaryDatabases)
}


fct_getDatabaseIdNamesListFromDatabasesHandlers <- function(databasesHandlers){

  databaseIdNamesList <- list()
  for(databaseId in names(databasesHandlers)){
    databaseIdNamesList[[databasesHandlers[[databaseId]]$cohortTableHandler$databaseName]] <- databaseId
  }

  if (length(databaseIdNamesList)==0) { databaseIdNamesList <- NULL }

  return(databaseIdNamesList)
}
