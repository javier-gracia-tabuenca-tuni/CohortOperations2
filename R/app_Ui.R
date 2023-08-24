#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(

    # Your application UI logic
    shinydashboard::dashboardPage(

      # TITLE
      shinydashboard::dashboardHeader(title = "Cohort Operations"),

      ## SIDEBAR
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shiny::h5(" Databases"),
          shinydashboard::menuItem("Databases connection", tabName = "selectDatabases", icon = shiny::icon("database")),
          shiny::h5(" Cohorts"),
          shinydashboard::menuItem("Import Cohorts", tabName = "importCohorts", icon = shiny::icon("address-card")),
          shinydashboard::menuItem("Match Cohorts", tabName = "matchCohorts", icon = shiny::icon("connectdevelop"))
        )
      ),

      ## BODY
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          ## Connect to databases
          shinydashboard::tabItem(
            tabName = "selectDatabases",
            mod_selectDatabases_ui("selectDatabases")
          ),
          ## TAB Import Cohorts
          shinydashboard::tabItem(
            tabName = "importCohorts",
            ### Cohorts workbench
            shinydashboard::box(
              title = "Cohorts workbench ",
              status = "primary", solidHeader = TRUE, width = 12,
              mod_cohortWorkbench_ui("cohortWorkbench_importCohorts")
            ),
            ### Import Cohorts
            shinydashboard::tabBox(
              title = tagList(shiny::icon("upload"), "Import Cohorts:"),
              id = "import_files", width = 12, side="right",
              selected = "from File",
              #### panel FILE
              shiny::tabPanel(
                "from File",
                mod_importCohortsFromFile_ui("importCohortsFromFile")
              )
            )
          ),
          ## TAB Matching Cohorts
          shinydashboard::tabItem(
            tabName = "matchCohorts",
            ### Cohorts workbench
            shinydashboard::box(
              title = "Cohorts workbench ",
              status = "primary", solidHeader = TRUE, width = 12,
              mod_cohortWorkbench_ui("cohortWorkbench_matchCohorts")
            ),
            ### Import Cohorts
            shinydashboard::box(
              title = tagList(shiny::icon("connectdevelop"), "Match Cohorts:"),
              solidHeader = TRUE, width = 12,
              mod_matchCohorts_ui("matchCohorts")
            )
          )
        )
      )
    )
  )
}
