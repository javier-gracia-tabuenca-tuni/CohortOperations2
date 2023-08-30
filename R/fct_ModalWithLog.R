





modalWithLog_ui <- function(id) {
  ns <- shiny::NS(id)

  modalDialog(
    shiny::textOutput(ns("modalContent"))
  )
}

modalWithLog_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    autoUpdate <- reactiveTimer(1000)

    # Update the modal content every second
    output$modalContent <- shiny::renderText({
      autoUpdate()
      Sys.time()
    })

  })
}



showModalWithLog <- function(id) {
  shiny::showModal(modalWithLog_ui(id))
}




removeModalWithLog <- function() {
  shiny::removeModal()
}

