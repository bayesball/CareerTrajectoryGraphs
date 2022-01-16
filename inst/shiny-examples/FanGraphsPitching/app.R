library(readr)
library(dplyr)
library(ggplot2)
library(geomtextpath)

fg_pitching <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/fgpitching_complete.csv")

compare_plot2 <- function(midYearRange, minIP,
                          measure, xvar, hof){

  fg_pitching %>%
    filter(midYear >= midYearRange[1],
           midYear <= midYearRange[2],
           cIP >= minIP) -> fg_subset
  measure_label <- measure
  if(measure == "K/9"){
    measure <- "K_9"
  }
  if(measure == "BB/9"){
    measure <- "BB_9"
  }
  if(measure == "HR/9"){
    measure <- "HR_9"
  }
  if(measure == "LOB%"){
    measure <- "LOB_Pct"
  }
  if(measure == "GB%"){
    measure <- "GB_Pct"
  }
  if(measure == "HR/FB"){
    measure <- "HR_FB"
  }
  HOF_label <- ""
  if(hof == "yes"){
    fg_subset %>%
      filter(inducted == "Y") -> fg_subset
    HOF_label <- "HOF"
  }
  plot1 <- ggplot(fg_subset,
                  aes_string(xvar, measure,
                             color = quote(Name),
                      weight = quote(IP),
                      label = quote(Name))) +
    geom_textsmooth(se = FALSE,
                method = "loess",
                formula = "y ~ x") +
    labs(title = paste(HOF_label, measure_label,
                       "Career Trajectories"),
         subtitle = paste("Midyear: (",
                          midYearRange[1], ", ",
                          midYearRange[2], "), Min IP: ",
                          minIP, sep = "")) +
    ylab(measure_label) +
    theme(text = element_text(size = 15),
          plot.title = element_text(colour = "red",
                                    size = 18,
                                    hjust = 0.5,
                                    vjust = 0.8,
                                    angle = 0),
          plot.subtitle = element_text(colour = "blue",
                                    size = 16,
                                    hjust = 0.5,
                                    vjust = 0.8,
                                    angle = 0)
          ) +
    theme(legend.position = "none")
  list(plot1 = plot1, S = fg_subset)
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("slate"),
  h2("FanGraphs Career Pitching Trajectories"),
  column(3,
  sliderInput("midyear", "Select Range of Mid Season:",
              1900, 2020,
              value = c(1980, 1985), sep = ""),
  sliderInput("minInnings", "Select Minimum Innings Pitched:",
              1000, 5000, 3000, sep = ""),
  selectInput("measure",
               "Select Measure:",
               c("K/9", "BB/9", "HR/9",
                 "BABIP", "LOB%", "GB%", "HR/FB",
                 "ERA", "FIP", "WAR"),
               selected = "WAR"),
  radioButtons("xvar",
               "Plot Against:",
               c("Season", "Age"),
               inline = TRUE),
  radioButtons("hof",
               "Hall of Fame?",
               c("no", "yes"),
               inline = TRUE),
  downloadButton("downloadData", "Download Data"),
  ),
  column(9,
         plotOutput("plot1",
                    height = '500px'))
)
server <- function(input, output, session) {
  options(warn=-1)

  output$plot1 <- renderPlot({
   compare_plot2(input$midyear, input$minInnings,
                 input$measure, input$xvar,
                 input$hof)$plot1
  }, res = 96)

  output$downloadData <- downloadHandler(
    filename = "trajectory_output.csv",
    content = function(file) {
      out <- compare_plot2(input$midyear, input$minInnings,
                          input$measure, input$xvar,
                          input$hof)
      out$S$MidYearLo <- input$midyear[1]
      out$S$MidYearHi <- input$midyear[2]
      out$S$MinIP <- input$minInnings
      write.csv(out$S, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
