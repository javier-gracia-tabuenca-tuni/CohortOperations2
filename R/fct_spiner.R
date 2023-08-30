


#' ui_load_spinner
#'
#' Adds dna spinner to a ui_element
#'
#' @param ui_element ui_element to add spinner to
#' @param ... other parameters to shinycustomloader::withLoader
#'
#' @return
#' @export
#'
#' @importFrom shinycustomloader withLoader
ui_load_spinner <- function(ui_element, ...) {
  shinycustomloader::withLoader(
    ui_element,
    type = "html",
    loader = "dnaspin",
    ...
  )
}
