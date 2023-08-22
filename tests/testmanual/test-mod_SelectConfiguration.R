# build parameters --------------------------------------------------------------
devtools::load_all(".")
source(testthat::test_path("setup.R"))
source(testthat::test_path("helper.R"))


r_connectionHandlers <- shiny::reactiveValues(
  databasesHandlers = NULL
)


# run module --------------------------------------------------------------
devtools::load_all(".")

shiny::shinyApp(
  shiny::fluidPage(
    mod_selectDatabases_ui("select_configuration")
  ),
  function(input,output,session){
    mod_selectDatabases_server("select_configuration", configurationList, r_connectionHandlers)
  }
)


