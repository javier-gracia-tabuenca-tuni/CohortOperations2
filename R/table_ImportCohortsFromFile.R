


# function separated for easier development
table_importCohortsFromFile_reactable <- function(cohortDefinitionSet, databaseName) {

  # if(nrow(connectionStatusLogs)==0){
  #   return(connectionStatusLogs |>
  #            reactable::reactable())
  # }

  table <- cohortDefinitionSet |>
    dplyr::select(cohortName, extra_info) |>
    tidyr::unnest(extra_info) |>
  dplyr::transmute(
    database_name = databaseName,
    cohortName = cohortName,
    cohort_counts_str = paste0(n_source_person, " (", n_source_entries, ")"),
    message_detailed = paste0(
      dplyr::case_match(n_missing_source_person,
        0 ~ paste0(uiCommons$emojis$check, " All person_source_values were found in ", database_name),
        n_source_person ~ paste0(uiCommons$emojis$error, " None of the person_source_values were found in ", database_name),
        .default = paste0(uiCommons$emojis$warning, " ", n_missing_source_person, " person_source_values were not found in ", database_name)
      ),
      dplyr::if_else(n_missing_cohort_start==0, "",
                     paste0("<br>", uiCommons$emojis$warning, " ", n_missing_cohort_start, " cohort_start_dates were missing and set to the first observation date")
      ),
      dplyr::if_else(n_missing_cohort_end==0, "",
                     paste0("<br>", uiCommons$emojis$warning, " ", n_missing_cohort_end, " cohort_end_dates were missing and set to the first observation date")
      )
    )
  ) |>
    reactable::reactable(
      columns = list(
        database_name = reactable::colDef(
          name = "Database"
        ),
        cohortName = reactable::colDef(
          name = "Cohort Name"
        ),
        cohort_counts_str = reactable::colDef(
          name = "N Subjects (N Entries)"
        ),
        message_detailed = reactable::colDef(
          name = "Checks", html = TRUE
        )
      ),
      #
      selection = "multiple",
      onClick = "select"
    )

  return(table)

}



