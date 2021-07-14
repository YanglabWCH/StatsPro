#' Run the StatsPro Shiny web application locally.
#' @export
StatsPro_app <- function() {
  shiny::runApp(system.file('StatsProapp', package='StatsPro'),
                host=getOption("0.0.0.0"), port =getOption("8989"))
}
