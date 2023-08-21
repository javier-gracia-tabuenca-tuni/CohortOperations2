


# function separated for easier development
table_connectionStatus_reactable <- function(connectionStatusLogs) {

  if(nrow(connectionStatusLogs)==0){
    return(connectionStatusLogs |>
             reactable::reactable())
  }

  connectionStatusLogs |>
    dplyr::mutate(
      type = dplyr::case_when(
        type == "INFO" ~ uiCommons$emojis$check,
        type == "WARNING" ~ uiCommons$emojis$warning,
        type == "ERROR" ~ uiCommons$emojis$error
      )
    ) |>
    reactable::reactable(
      groupBy = "database",
      columns = list(
        database = reactable::colDef(
          name = "Database"
        ),
        type = reactable::colDef(
          name = "Status",
          aggregate = "frequency"
        ),
        step = reactable::colDef(
          name = "Check"
        ),
        message = reactable::colDef(
          name = "Message"
        )
      )
    )
}



