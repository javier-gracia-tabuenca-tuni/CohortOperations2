
devtools::load_all(".")

cohortOperationsSettings <- yaml::read_yaml(pathToCohortOperationsConfigYalm)

r_connectionHandlers <- shiny::reactiveValues(
  databasesHandlers = NULL
)




shinyApp(
  fluidPage(
    mod_SelectDatabases_ui("select_configuration")
  ),
  function(input,output,session){
    mod_SelectDatabases_server("select_configuration", cohortOperationsSettings, r_connectionHandlers)
  }
)


