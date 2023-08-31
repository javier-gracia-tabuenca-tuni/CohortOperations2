


mod_timeCodeWAS_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    #
    shiny::tags$h4("Database"),
    shiny::uiOutput(ns("selectDatabases_pickerInput_uiOutput")),
    htmltools::hr(),
    shiny::tags$h4("Cohorts"),
    shiny::uiOutput(ns("selectCaseCohort_pickerInput_uiOutput")),
    shiny::uiOutput(ns("selectControlCohort_pickerInput_uiOutput")),
    htmltools::hr(),
    shiny::tags$h4("Settings"),
    #
    shinyWidgets::pickerInput(
      inputId = ns("selectCovariates"),
      label = "Select database where to match cohorts:",
      choices = list(
        Conditions = list(
          `Conditions` = "useConditionOccurrence",
          `Conditions in Primary Inpatient` = "useConditionOccurrencePrimaryInpatient",
          `Conditions SNOMED Group` = "useConditionEraGroupOverlap"
        ),
        Drugs = list(
          `Drugs` = "useDrugExposure",
          `Drugs ATC Group` = "useDrugEraGroupOverlap"
        ),
        Procedures = list(
          `Procedures` = "useProcedureOccurrence"
        ),
        Others = list(
          `Device Exposure` = "useDeviceExposure",
          `Measurement` = "useMeasurement",
          `Observation` = "useObservation"
        )
      ),
      selected = c("useConditionOccurrence", "useDrugExposure", "useProcedureOccurrence", "useDeviceExposure", "useMeasurement", "useObservation"),
      options = list(`actions-box` = TRUE),
      multiple = TRUE),
    shiny::tags$h5("Select ranges"),
    mod_temporalRanges_ui(ns("selectRanges")),
    #
    htmltools::hr(),
    shiny::tags$h4("Summary"),
    shiny::verbatimTextOutput(ns("newCohortName_text")),
    shiny::tags$br(),
    shiny::actionButton(ns("run_actionButton"), "Run Study"),
    #
    htmltools::hr(),
    shiny::tags$h4("Results"),
    reactable::reactableOutput(ns("reactableResults")),
    shiny::tags$br(),
    shiny::downloadButton(ns("download_actionButton"), "Download"),
    shiny::actionButton(ns("view_actionButton"), "Open Viewer"),
  )
}


mod_timeCodeWAS_server <- function(id, r_connectionHandlers) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns



    #
    # reactive variables
    #
    r_ranges <- mod_temporalRanges_server("selectRanges")

    r <- shiny::reactiveValues(
      studySettings = NULL
    )

    rf_timeCodeWasCounts <- shiny::reactiveVal()

    # A reactive value with the inputs to modalWithLog_server
    .r_l <- shiny::reactiveValues(
      .l = NULL
    )


    #
    # render selectDatabases_pickerInput with database names
    #
    output$selectDatabases_pickerInput_uiOutput <- shiny::renderUI({
      databaseIdNamesList <- fct_getDatabaseIdNamesListFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      shinyWidgets::pickerInput(
        inputId = ns("selectDatabases_pickerInput"),
        label = "Select database where to match cohorts:",
        choices = databaseIdNamesList,
        selected = databaseIdNamesList[1],
        multiple = FALSE)
    })

    #
    # render selectCaseCohort_pickerInput with cohort names in selectDatabases_pickerInput database
    #
    output$selectCaseCohort_pickerInput_uiOutput <- shiny::renderUI({
      shiny::req(input$selectDatabases_pickerInput)

      cohortIdAndNames <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler$getCohortIdAndNames()
      cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, cohortIdAndNames$cohortName))
      cohortIdAndNamesList <- c(list(`----` = as.character(NA)), cohortIdAndNamesList)

      shiny::selectInput(
        inputId = ns("selectCaseCohort_pickerInput"),
        label = "Select Case cohort:",
        choices = cohortIdAndNamesList,
        selected = cohortIdAndNamesList[["----"]],
        multiple = FALSE)
    })


    #
    # render matchToCohortId_pickerInput with cohort names not in selectCaseCohort_pickerInput
    #
    output$selectControlCohort_pickerInput_uiOutput <- shiny::renderUI({
      shiny::req(input$selectDatabases_pickerInput)
      shiny::req(input$selectCaseCohort_pickerInput)

      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler

      if(input$selectCaseCohort_pickerInput != "NA"){
        cohortIdAndNames <- cohortTableHandler$getCohortIdAndNames() |>
          dplyr::filter(!(cohortId %in% input$selectCaseCohort_pickerInput))
        cohortIdAndNamesList <- as.list(setNames(cohortIdAndNames$cohortId, cohortIdAndNames$cohortName))
      }else{
        cohortIdAndNamesList <- list()
      }
      cohortIdAndNamesList <- c(list(`----` = as.character(NA)), cohortIdAndNamesList)

      shiny::selectInput(
        inputId = ns("selectControlCohort_pickerInput"),
        label = "Select Control Cohort:",
        choices = cohortIdAndNamesList,
        selected = cohortIdAndNamesList[["----"]],
        multiple = FALSE)
    })

    #
    # activate settings if cohors have been selected
    #
    observe({
      condition <- !is.null(input$selectControlCohort_pickerInput) & input$selectControlCohort_pickerInput!="NA"
      shinyjs::toggleState("selectCovariates", condition = condition )
      shinyjs::toggleState("selectRanges-addBtn", condition = condition )
      shinyjs::toggleState("run_actionButton", condition = condition )
    })

    #
    # create settings
    #
    observe({
      shiny::req(input$selectDatabases_pickerInput)
      shiny::req(input$selectCaseCohort_pickerInput)
      shiny::req(input$selectCaseCohort_pickerInput!="NA")
      shiny::req(input$selectControlCohort_pickerInput)
      shiny::req(input$selectControlCohort_pickerInput!="NA")
      shiny::req(input$selectCovariates)
      shiny::req(r_ranges()$temporalStartDays)
      shiny::req(r_ranges()$temporalEndDays)

      studySettings <- list(
        cohortIdCases = input$selectCaseCohort_pickerInput,
        cohortIdControls = input$selectControlCohort_pickerInput,
        temporalCovariateSettings = list(
          temporalStartDays = r_ranges()$temporalStartDays,
          temporalEndDays =   r_ranges()$temporalEndDays
        )
      )

      for(covaraiteSetting in input$selectCovariates){
        studySettings$temporalCovariateSettings[[covaraiteSetting]] <- TRUE
      }

      r$studySettings <- studySettings

    })

    #
    # Render temporal name
    #
    output$newCohortName_text <- shiny::renderText({
      if(!shiny::isTruthy(r$studySettings)){
        "----"
      }else{
        yaml::as.yaml(r$studySettings)
      }
    })

    #
    # click to run
    #
    shiny::observeEvent(input$run_actionButton, {
      shiny::req(r$studySettings)
      # copy studySettings to .r_l$.l
      cohortTableHandler <- r_connectionHandlers$databasesHandlers[[input$selectDatabases_pickerInput]]$cohortTableHandler

      l <- r$studySettings

      .r_l$.l <- list(
        cohortTableHandler = cohortTableHandler,
        studySettings = l
      )
    })


    # Take parameters, run function in a future, open modal with log, close modal when ready, return value
    rf_timeCodeWasCounts <- modalWithLog_server(
      id = "sss",
      .f = function(
          cohortTableHandler,
          studySettings
        ){
        ParallelLogger::logInfo("Start timeCodeWasCounts")
        temporalCovariateSettings <- do.call(FeatureExtraction::createTemporalCovariateSettings, studySettings$temporalCovariateSettings)
        #browser()
        timeCodeWasCounts <- HadesExtras::CohortDiagnostics_runTimeCodeWAS(
          connection = cohortTableHandler$connectionHandler$getConnection(),
          cdmDatabaseSchema = cohortTableHandler$cdmDatabaseSchema,
          vocabularyDatabaseSchema = cohortTableHandler$vocabularyDatabaseSchema,
          cohortDatabaseSchema = cohortTableHandler$cohortDatabaseSchema,
          cohortTable = cohortTableHandler$cohortTableNames$cohortTable,
          cohortIdCases = as.integer(studySettings$cohortIdCases),
          cohortIdControls = as.integer(studySettings$cohortIdControls),
          covariateSettings = temporalCovariateSettings
        )
        ParallelLogger::logInfo("End timeCodeWasCounts")
        return(timeCodeWasCounts)
      },
      .r_l = .r_l,
      logger = shiny::getShinyOption("logger"))


    #
    # display results
    #
    output$reactableResults <- reactable::renderReactable({
      shiny::req(rf_timeCodeWasCounts)

      rf_timeCodeWasCounts() |>
      reactable::reactable(
        sortable = TRUE,
        resizable = TRUE,
        filterable = TRUE,
        defaultPageSize = 5
      )

    })

    #
    # activate settings if cohors have been selected
    #
    shiny::observe({
      condition <- !is.null(r$timeCodeWasCounts)
      shinyjs::toggleState("download_actionButton", condition = condition )
      shinyjs::toggleState("view_actionButton", condition = condition )
    })


    output$download_actionButton <- shiny::downloadHandler(
      filename = function(){"thename.csv"},
      content = function(fname){
        write.csv(r$timeCodeWasCounts, fname)
      }
    )



  })
}





















