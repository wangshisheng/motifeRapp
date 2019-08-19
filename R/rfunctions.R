#' Runs the motifeR Shiny web application.
#' @export
motifeR_app <- function() {
  shiny::runApp(system.file('motifeRapp', package='motifeR'),
                host=getOption("0.0.0.0"), port =getOption("8989"))
}
