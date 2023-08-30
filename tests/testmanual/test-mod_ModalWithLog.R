

# run module --------------------------------------------------------------
devtools::load_all(".")

shiny::shinyApp(
  shiny::fluidPage(
    shiny::titlePanel("Modal Dialog with Updating Content"),
    shiny::actionButton("showModal", "Show Modal")
  ),
  function(input,output,session){

    modalWithLog_server("ttt")

    r <- shiny::reactiveValues(
      run = NULL
    )

    # Show the modal dialog when the button is clicked
    shiny::observeEvent(input$showModal, {
      showModalWithLog("ttt")

      r$run <- 10

    })

    shiny::observeEvent(r$run,{
      #browser()
      Sys.sleep(r$run)
      removeModal()
    })


  },
  options = list(launch.browser=TRUE)
)



