

table_cohortsWorkbench_reactable <- function(cohortsSummary, deleteButtonsShinyId = NULL) {

  cohortsSummary |>  HadesExtras::assertCohortsSummary()
  deleteButtonsShinyId |> checkmate::assertString(null.ok = TRUE)


  table <- cohortsSummary  |>
    dplyr::transmute(
      database_name = database_name,
      cohortName = cohortName,
      cohort_counts_str = paste0(cohortSubjects, " (", cohortEntries, ")"),
      histogram_cohort_start_year,
      histogram_cohort_end_year,
      count_sex_str = purrr::map_chr(count_sex, .sex_tibble_to_str),
      delete_button = NA
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
        histogram_cohort_start_year = reactable::colDef(
          name = "Cohort Start Date",
          cell = .render_apex_plot
        ),
        histogram_cohort_end_year = reactable::colDef(
          name = "Cohort End Date",
          cell = .render_apex_plot
        ),
        count_sex_str = reactable::colDef(
          name = "Sex",
          style = function(value) {
            .bar_style(per_sex_str = value)
          },
          align = "left"
        ),
        delete_button = reactable::colDef(
          name = "",
          show = !is.null(deleteButtonsShinyId),
          sortable = FALSE,
          cell = function() htmltools::tags$button(shiny::icon("trash"))
        )
      ),
      onClick = reactable::JS(ifelse(is.null(deleteButtonsShinyId), "", paste0("
        function(rowInfo, column) {
          // Only handle click events on the 'details' column
          if (column.id !== 'delete_button') {
            return
          }

          // Display an alert dialog with details for the row
          //window.alert('Details for row ' + rowInfo.index)

          // Send the click event to Shiny, which will be available in input$show_details
          // Note that the row index starts at 0 in JavaScript, so we add 1
          if (window.Shiny) {
            Shiny.setInputValue('", deleteButtonsShinyId, "', { index: rowInfo.index + 1 }, { priority: 'event' })
          }
        }
      ")))
    )

  return(table)
}

.render_apex_plot <- function(data) {
  data |>
    apexcharter::apex(apexcharter::aes(year, n ), type = "column", height = 50) |>
    apexcharter::ax_chart(sparkline = list(enabled = TRUE)) |>
    apexcharter::ax_colors(uiCommons$colors$timeHist) |>
    apexcharter::ax_yaxis(min = 0, max = max(data$n))
}

# Render a bar chart in the background of the cell
.bar_style <- function(per_sex_str){
  height <-  "75%"
  fill_color <- uiCommons$colors$sexMale
  na_color <- uiCommons$colors$sexNa
  background_color <- uiCommons$colors$sexFemale
  text_color <-  "#FFFFFF"

  ss<-  stringr::str_split(per_sex_str, "[:blank:]")
  p_male <- ss[[1]][1] |> stringr::str_remove("%") |> as.double()
  p_na <- ss[[1]][2] |> stringr::str_remove("%") |> as.double()
  p_female <- ss[[1]][3] |> stringr::str_remove("%") |> as.double()

  list(
    backgroundImage = paste0("linear-gradient(to right, ",
                             fill_color, " ", p_male,"%, ",
                             na_color, " ", p_male,"%, ", na_color, " ", p_male+p_na,"%, ",
                             background_color ," ", p_male+p_na,"%, ",
                             background_color,
                             ")"),
    backgroundSize = paste("100%", height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    color = text_color
  )
}


.sex_tibble_to_str <- function(data){
  n_male <- data |> dplyr::filter(sex=="MALE") |> purrr::pluck("n",1, .default = 0)
  n_female <- data |> dplyr::filter(sex=="FEMALE") |> purrr::pluck("n",1, .default = 0)
  n_na <- data |> dplyr::filter( sex!="MALE" & sex!="FEMALE" ) |> purrr::pluck("n",1, .default = 0)
  n_total <- n_male + n_female + n_na

  p_male <- round(n_male/n_total*10000)/100
  p_female <- round(n_female/n_total*10000)/100
  p_na <- round(n_na/n_total*10000)/100

  return(paste0(p_male, "% ", p_na, "% ", p_female, "%" ))
}



