


mod_cohortsIncidence_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    #
    shiny::tags$h4("Cohorts"),
    shiny::uiOutput(ns("selectCohort_pickerInput_uiOutput")),
    htmltools::hr(),
    #
    shiny::tags$h4("Settings"),
    shiny::numericInput(
      inputId = ns("minCellCount_numericInput"),
      label = "Min Cell Count",
      value = 1,
      min = 1,
      max = 1000
    ),
    htmltools::hr(),
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
    shiny::downloadButton(ns("download_actionButton"), "Download to Sandbox"),
    shiny::downloadButton(ns("download_actionButton2"), "Download out of Sandbox"),
    shiny::actionButton(ns("view_actionButton"), "Open Viewer"),
  )
}


mod_cohortsIncidence_server <- function(id, r_connectionHandlers) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns



    #
    # reactive variables
    #

    r <- shiny::reactiveValues(
      studySettings = NULL
    )

    rf_cohortsIncidenceCounts <- shiny::reactiveVal()

    # A reactive value with the inputs to modalWithLog_server
    .r_l <- shiny::reactiveValues(
      .l = NULL
    )


    #
    # render selectDatabases_pickerInput with database names
    #
    output$selectCohort_pickerInput_uiOutput <- shiny::renderUI({
      databaseIdNamesList <- fct_getDatabaseIdNamesListFromDatabasesHandlers(r_connectionHandlers$databasesHandlers)

      cohortIdAndNamesList <- list()
      for(databaseIdName in databaseIdNamesList){
        cohortIdAndNames <- r_connectionHandlers$databasesHandlers[[databaseIdName]]$cohortTableHandler$getCohortIdAndNames()
        cohortIdAndNamesList[databaseIdName] <- list(as.list(setNames(paste0(databaseIdName, "-", cohortIdAndNames$cohortId), cohortIdAndNames$cohortName)))
      }

      shinyWidgets::pickerInput(
        inputId = ns("selectCohort_pickerInput"),
        label = "Select one or more cohorts:",
        choices = cohortIdAndNamesList,
        selected = cohortIdAndNamesList,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE)
        )
    })


    #
    # activate settings if cohors have been selected
    #
    shiny::observe({
      condition <- !is.null(input$selectCohort_pickerInput)
      shinyjs::toggleState("run_actionButton", condition = condition )
    })

    #
    # create settings
    #
    shiny::observe({
      shiny::req(input$selectCohort_pickerInput)
      shiny::req(input$selectCohort_pickerInput!="NA")
      shiny::req(input$minCellCount_numericInput)

      # convert vector of strings databaseId-cohortId to tibble databaseId and cohortIds
      databaseIdsCohortIdsTibble<- data.frame(
        databaseId = gsub(pattern = "-.*", replacement = "", x = input$selectCohort_pickerInput),
        cohortId = gsub(pattern = ".*-", replacement = "", x = input$selectCohort_pickerInput)
      )

      databaseIdsCohorsIdsList <- list()
      for(databaseId in unique(databaseIdsCohortIdsTibble$databaseId)){
        databaseIdsCohorsIdsList[[databaseId]] <- databaseIdsCohortIdsTibble |> dplyr::filter(databaseId == !!databaseId) |> dplyr::pull(cohortId)
      }

      studySettings <- list(
        studyType = "cohortsIncidence",
        databaseIdsCohorsIdsList = databaseIdsCohorsIdsList,
        minCellCount = input$minCellCount_numericInput
        )

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
      databasesHandlers <- r_connectionHandlers$databasesHandlers


      l <- r$studySettings

      .r_l$.l <- list(
        databasesHandlers = databasesHandlers,
        studySettings = l,
        sqlRenderTempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
      )
    })


    # Take parameters, run function in a future, open modal with log, close modal when ready, return value
    rf_cohortsIncidenceCounts <- modalWithLog_server(
      id = "sss",
      .f = function(
    databasesHandlers,
    studySettings,
    sqlRenderTempEmulationSchema
      ){
        # needs to be set in the future
        options(sqlRenderTempEmulationSchema=sqlRenderTempEmulationSchema)
        #
        ParallelLogger::logInfo("Start cohortsIncidence")
        for(databaseId in names(studySettings$databaseIdsCohorsIdsList)){
          ParallelLogger::logInfo("databaseId = ", databaseId)
          cohortTableHandler <-databasesHandlers[[databaseId]]$cohortTableHandler

          CohortDiagnostics:: executeDiagnostics(
            cohortDefinitionSet = cohortTableHandler$cohortDefinitionSet,
            exportFolder = file.path(tempdir(), databaseId),
            databaseId = cohortTableHandler$databaseName,
            cohortDatabaseSchema = cohortTableHandler$cohortDatabaseSchema,
            databaseName = cohortTableHandler$databaseName,
            databaseDescription = cohortTableHandler$databaseName,
            connection = cohortTableHandler$connectionHandler$getConnection(),
            cdmDatabaseSchema = cohortTableHandler$cdmDatabaseSchema,
            cohortTable = cohortTableHandler$cohortTableNames$cohortTable,
            vocabularyDatabaseSchema = cohortTableHandler$vocabularyDatabaseSchema,
            cohortIds = studySettings$databaseIdsCohorsIdsList[[databaseId]],
            runInclusionStatistics = FALSE,
            runIncludedSourceConcepts = FALSE,
            runOrphanConcepts = FALSE,
            runTimeSeries = FALSE,
            runVisitContext = FALSE,
            runBreakdownIndexEvents = FALSE,
            runIncidenceRate = TRUE,
            runCohortRelationship = FALSE,
            runTemporalCohortCharacterization = FALSE,
            minCellCount = 1,
            incremental = FALSE
          )
        }

        #merge files
        cohortsIncidence <- NULL
        for(databaseId in names(studySettings$databaseIdsCohorsIdsList)){
          ParallelLogger::logInfo("databaseId = ", databaseId)
          incidenceRate <- readr::read_csv(file.path(tempdir(), databaseId, "incidence_rate.csv"))
          cohortsIncidence <- dplyr::bind_rows(cohortsIncidence, incidenceRate)
        }

        ParallelLogger::logInfo("End cohortsIncidence")
        return(cohortsIncidence)
      },
    .r_l = .r_l,
    logger = shiny::getShinyOption("logger"))


    #
    # display results
    #
    output$reactableResults <- reactable::renderReactable({
      shiny::req(rf_cohortsIncidenceCounts)

      rf_cohortsIncidenceCounts() |>
        reactable::reactable(
          sortable = TRUE,
          resizable = TRUE,
          filterable = TRUE,
          defaultPageSize = 5
        )

    })

    #
    # activate settings if cohorts have been selected
    #
    shiny::observe({
      condition <- !is.null(rf_cohortsIncidenceCounts())
      shinyjs::toggleState("download_actionButton", condition = condition )
      shinyjs::toggleState("view_actionButton", condition = condition )
    })


    output$download_actionButton <- shiny::downloadHandler(
      filename = function(){"analysisName_cohortsIncidence.zip"},
      content = function(fname){

        sweetAlert_spinner("Preparing files for download")

        # create a new directory in random temp directory
        tmpDir <- file.path(tempdir(), "cohortOperationsStudy")
        dir.create(tmpDir)

        # save cohorts used in analysis
        usedCohortsIds <- c(r$studySettings$cohortIdCases, r$studySettings$cohortIdControls)
        usedCohortsSummaryDatabases <- fct_getCohortsSummariesFromDatabasesHandlers(r_connectionHandlers$databasesHandlers) |>
          dplyr::filter(cohortId %in% usedCohortsIds) |>
          dplyr::select(databaseName, cohortId, shortName, cohortName,cohortEntries, cohortSubjects)

        write.csv(usedCohortsSummaryDatabases, file.path(tmpDir, "cohortsSummary.csv"))

        # save analysis settings
        yaml::write_yaml(r$studySettings, file.path(tmpDir, "studySettings.yaml"))

        # save analysis results
        write.csv(rf_cohortsIncidenceCounts(), file.path(tmpDir, "results.csv"))

        # zip all files
        zip::zipr(zipfile = fname, files = tmpDir)

        remove_sweetAlert_spinner()

        return(fname)
      }
    )



  })
}





















