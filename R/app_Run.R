#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(pathToCohortOperationsConfigYalm, ...) {

  checkmate::assertFileExists(pathToCohortOperationsConfigYalm, extension = "yml")
  configurationList <- yaml::read_yaml(pathToCohortOperationsConfigYalm)
  checkmate::assertList(configurationList, names = "named")


    app  <- shiny::shinyApp(
        ui = app_ui,
        server = app_server,
        ...
      )

    app$appOptions$configurationList  <- configurationList

    return(app)
}
