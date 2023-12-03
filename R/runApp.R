#' Launch the application
#' @export
run_application <- function() {
  appDir <- file.path("inst", "app.R")
  if (appDir == "") {
    stop("Could not find app file. Try re-installing `bis620.2023`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

