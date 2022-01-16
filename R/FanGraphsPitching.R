FanGraphsPitching <- function() {
  appDir <- system.file("shiny-examples",
                        "FanGraphsPitching",
                        package = "CareerTrajectoryGraphs")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `CareerTrajectory`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
