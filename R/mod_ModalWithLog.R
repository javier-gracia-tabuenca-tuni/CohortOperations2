


modalWithLog_server <- function(id,.f,.r_l, logger) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # set up future
    queue <- ipc::shinyQueue()
    queue$consumer$start(100) # Execute signals every 100 milliseconds
    inter <- ipc::AsyncInterruptor$new()

    # set up modal
    autoUpdate <- shiny::reactiveTimer(1000)

    # intermediary result
    result_val <- reactiveVal()

    # when params update run function
    shiny::observeEvent(.r_l$.l,{
      .l = .r_l$.l
      ParallelLogger::logInfo("Launching future in modalWithLog_server")
      future::future({
        # register logger
        ParallelLogger::registerLogger(logger)
        # run function
        result <- do.call(.f, .l)
        # set result to reactive
        queue$producer$fireAssignReactive("result_val",result)
        # set interrupter
        inter$execInterrupts()
      })

      shiny::showModal(shiny::modalDialog(
        shiny::tagList(
          ui_load_spinner(shiny::plotOutput(outputId = "plot", width = "500px", height = "100px"), proxy.height = "90px"),
          shiny::verbatimTextOutput(ns("modalContent"))
        ),
        footer = tagList(
          shiny::actionButton(ns("interrupt"), "Interrupt")
        ),
        size = "xl"
      ))
      #Return something other than the future so we don't block the UI
      NULL
    })

    # Update the modal content every second
    output$modalContent <- shiny::renderText({
      autoUpdate()
      file_path <- logger$appenders[[1]]$fileName
      num_lines <- 10

      lines <- readLines(file_path)
      last_lines <- tail(lines, num_lines)

      paste0(last_lines, collapse = "\n")
    })


    # if clicked interrup, interrupt and close modal
    shiny::observeEvent(input$interrupt,{
      inter$interrupt("Stop that future")
      shiny::removeModal()
    })

    # when result ready close modal and return
    shiny::reactive({
      removeModal()
      result_val()
    })

  })
}

