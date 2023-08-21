

fct_createEmptyCohortWorkbenchTable <- function() {

  cohortWorkbenchTable <- HadesExtras::createEmptyCohortsSummary() |>
    dplyr::mutate(database_name="") |>
    dplyr::relocate(database_name, .before = 1)

  return(cohortWorkbenchTable)

}


fct_createCohortWorkbenchTableFromDatabasesHandlers <- function(databasesHandlers){

  databasesHandlers |> checkmate::assertList(null.ok = TRUE)

  if(is.null(databasesHandlers)){
    return(fct_createEmptyCohortWorkbenchTable())
  }

  cohortWorkbenchTable <- fct_createEmptyCohortWorkbenchTable()
  for(name in names(databasesHandlers)) {

    cohortTableHandler <- databasesHandlers[[name]]$cohortTableHandler

    cohortWorkbenchTable <- dplyr::bind_rows(
      cohortWorkbenchTable,
      cohortTableHandler$getCohortsSummary() |>
        dplyr::mutate(database_name=name)
    )

  }

  return(cohortWorkbenchTable)
}

