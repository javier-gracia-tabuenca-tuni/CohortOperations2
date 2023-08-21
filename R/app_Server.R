#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # get settings loaded from file
  cohortOperationsSettings <- shiny::getShinyOption("cohortOperationsSettings")

  # list of connection handlers that are passed to modules,
  # not all modules used all, they modules check the list has at least the ones they need
  # they are produced only by mod_select_configuration and consumed by the modules
  r_connectionHandlers <- shiny::reactiveValues(
    cohortTableHandler = NULL
  )

  # produced by modules related to cohort editing
  # consumed by cohort table viewer
  r_workbechCohortsSummary <- shiny::reactiveValues(
    workbechCohortsSummary = fct_createEmptyCohortWorkbenchTable()
  )


  mod_selectDatabases_server("selectDatabases", cohortOperationsSettings, r_connectionHandlers)

  mod_cohortWorkbench_server("cohortWorkbench_importCohorts", r_connectionHandlers, r_workbechCohortsSummary)

  mod_importCohortsFromFile_server("importCohortsFromFile", r_connectionHandlers, r_workbechCohortsSummary)

  #   r_cdmConfig <- shiny::reactiveValues(
  #     selectedConfig = NULL
  #   )

  #   r_cohorts <- reactiveValues(
  #     cohortData = NULL, #R6 object, only changed by config change
  #     summaryCohortData = #summary made from R6::summary
  #   )

  #   r_config_atlas <- reactiveValues(
  #     #only changed by config
  #     )


  # # base modules ---------------------------------------------
  # # connection
  # # input, path to config file ; returns list of connections and configurations to pass the server modules
  # # import

  # output$txt <-shiny::renderText({
  #   paste("dd", input$cb)
  # })


  # aa <- shiny::reactive({
  #   input$cb
  # })

  # mod_test_server("mod_test", aa)

  # mod_test_server("mod_test2", aa)

  # optional modules ---------------------------------------------
  # if sering wrong then show error mesage


  # INFO connection tab ---------------------------------------------
  # r_connection <- reactiveValues(
  #   cdm_webapi_conn = configCDMTools(),
  #   phewas_conn = configFGpheWAS(),
  #   connection_sandboxAPI = configGWAS())
  # r_cohorts <- reactiveValues(
  #   cohortData = FinnGenTableTypes::empty_cohortData(),
  #   summaryCohortData = FinnGenTableTypes::empty_cohortData() %>% FinnGenTableTypes::summarise_cohortData()
  # )
  #
  #
  #
  #
  #
  #
  # # modules ---------------------------------------------
  # mod_connection_to_db_server("mod_connection_to_db", r_connection)
  #
  # mod_cohorts_table_server("mod_cohorts_table_import", r_cohorts)
  # mod_import_cohort_file_server("mod_import_cohort_file", r_cohorts)
  # mod_import_cohort_atlas_server("mod_import_cohort_atlas", r_connection, r_cohorts)
  # mod_import_cohort_endpoints_server("mod_import_cohort_endpoint", r_connection, r_cohorts)
  #
  # mod_cohorts_table_server("mod_cohorts_table_operate", r_cohorts)
  # mod_operate_cohorts_server("mod_operate_cohorts", r_cohorts)
  #
  # mod_cohorts_table_server("mod_cohorts_table_compare", r_cohorts, table_editing = FALSE)
  # mod_compare_cohorts_server("mod_compare_cohorts", r_cohorts)
  #
  # mod_cohorts_table_server("mod_cohorts_table_phewas", r_cohorts, table_editing = FALSE)
  # mod_phewas_server("mod_phewas", r_connection, r_cohorts)
  # mod_cohorts_table_server("mod_cohorts_table_gwas", r_cohorts, table_editing = FALSE)
  # mod_gwas_server("mod_gwas", r_connection, r_cohorts)

  #
  # info bubbles ---------------------------------------------
  # mod_info_box_server("info_importcohorts", "Import Cohorts", "info_importcohorts.md")
  # mod_info_box_server("info_operatecohorts", "Operate Cohorts", "info_operatecohorts.md")
  # mod_info_box_server("info_comparecohorts", "Compare Cohorts", "info_comparecohorts.md")
  # mod_info_box_server("info_phewas", "CodeWAS", "info_phewas.md")
  #
}
