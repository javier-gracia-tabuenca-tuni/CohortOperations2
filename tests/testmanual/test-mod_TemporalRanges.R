

shiny::shinyApp(
  shiny::fluidPage(
    .mod_timeRange_ui(id = "test")
  ),
  function(input,output,session){
    .mod_timeRange_server("test")
  },
  options = list(launch.browser=TRUE)
)


# run module --------------------------------------------------------------
devtools::load_all(".")

shiny::shinyApp(
  shiny::fluidPage(
    mod_temporalRanges_ui("test")
  ),
  function(input,output,session){
    output <- mod_temporalRanges_server("test", session)

    observe({
        print(output())
    })

  },
  options = list(launch.browser=TRUE)
)
