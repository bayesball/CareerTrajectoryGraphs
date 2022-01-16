library(dplyr)
library(ggplot2)
library(Lahman)
library(geomtextpath)
library(purrr)

compare_plot <- function(midYearRange, minGames,
                         position, measure, xvar,
                         hof){
  # find eligible players
  Fielding %>%
    filter(POS == position) %>%
    group_by(playerID) %>%
    summarize(minYear = min(yearID),
              maxYear = max(yearID),
              midYear = (minYear + maxYear) / 2,
              Innings = sum(InnOuts / 3, na.rm = TRUE),
              G = sum(G),
              .groups = "drop")  %>%
    filter(midYear <= midYearRange[2],
           midYear >= midYearRange[1],
           G >= minGames) -> C_info

  HOF_label <- ""
  if(hof == "yes"){
    hof_players <- filter(HallOfFame,
                          inducted == "Y") %>%
      select(playerID) %>% pull()
    C_info <- filter(C_info,
                     playerID %in% hof_players)
    HOF_label <- "HOF"
  }
  # obtain season to season statistics
  Fielding %>%
    filter(POS == position) %>%
    filter(playerID %in% C_info$playerID) %>%
    inner_join(select(Master, playerID,
                      nameFirst, nameLast),
               by = "playerID") %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    group_by(Name, yearID) %>%
    summarize(Innings = sum(InnOuts / 3),
              PO = sum(PO),
              A = sum(A),
              E = sum(E),
              G = sum(G),
              RF9 = 9 * (PO + A) / Innings,
              RFG = (PO + A) / G,
              FPct = (PO + A) / (PO + A + E),
              .groups = "drop") -> S

  # function to obtain birthyear for player
  get_birthyear <- function(playerid) {
    Master %>%
      filter(playerID == playerid)  %>%
      mutate(Name = paste(nameFirst, nameLast),
             birthyear = ifelse(birthMonth >= 7,
                                birthYear + 1, birthYear)) %>%
      select(Name, birthyear)
  }
  # collect birthyears and compute ages for each
  # player and season
  S1 <- map_df(C_info$playerID, get_birthyear)
  inner_join(S, S1, by = "Name") %>%
    mutate(Age = yearID - birthyear) -> S
  # define outcome depending on input
  if(measure == "RF/9"){
    S$Outcome <- S$RF9
    S$Weight <- S$G
    YLAB <- "RF/9"
  }
  if(measure == "RF/G"){
    S$Outcome <- S$RFG
    S$Weight <- S$G
    YLAB <- "RF/G"
  }
  if(measure == "Fld%"){
    S$Outcome <- S$FPct
    S$Weight <- S$G
    YLAB <- "Fld%"
  }
  # plot versus season or age?
  if(xvar == "year"){
    S$XVAR <- S$yearID
    XLAB <- "Season"
  }
  if(xvar == "age"){
    S$XVAR <- S$Age
    XLAB <- "Age"
  }
  plot1 <- ggplot(S,
                  aes(XVAR, Outcome, color = Name,
                      weight = Weight,
                      label = Name)) +
   # geom_point(size = 3) +
    geom_textsmooth(se = FALSE,
                method = "loess",
                formula = "y ~ x") +
    ylab(YLAB) +
    xlab(XLAB) +
    labs(title = paste(HOF_label,
                       position,
                          YLAB, "Career Trajectories"),
         color = "Player",
         subtitle = paste("Midyear: (",
                          midYearRange[1], ", ",
                          midYearRange[2], "), Min Games: ",
                          minGames, sep = "")) +
    theme(text = element_text(size = 15),
          plot.title = element_text(colour = "red",
                                    size = 18,
                  hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "blue",
                                    size = 16,
                                    hjust = 0.5, vjust = 0.8, angle = 0)) +
    theme(legend.position = "none")
  list(plot1 = plot1, S = S)
  }

ui <- fluidPage(
  theme = shinythemes::shinytheme("slate"),
  h2("Comparing Many Career Fielding Trajectories"),
  column(3,
  selectInput("position",
              "Select Position:",
              choices = c("SS", "2B", "OF", "C",
                          "1B", "3B", "P")),
  sliderInput("midyear", "Select Range of Mid Season:",
              1900, 2020,
              value = c(1980, 1985), sep = ""),
  sliderInput("minGames",
              "Select Minimum Career Games:",
              500, 2000, 1000, sep = ""),
  radioButtons("type",
               "Select Measure:",
               c("Fld%", "RF/9", "RF/G"),
               inline = TRUE),
  radioButtons("xvar",
               "Plot Against:",
               c("year", "age"),
               inline = TRUE),
  radioButtons("hof",
               "Hall of Fame?",
               c("no", "yes"),
               inline = TRUE),
  downloadButton("downloadData", "Download Data")
  ),
  column(9,
         plotOutput("plot1",
                    height = '500px'))
)
server <- function(input, output, session) {
  options(warn=-1)

  output$plot1 <- renderPlot({
    compare_plot(input$midyear, input$minGames,
                 input$position, input$type,
                 input$xvar,
                 input$hof)$plot1
  }, res = 96)
  output$downloadData <- downloadHandler(
    filename = "trajectory_output.csv",
    content = function(file) {
      out <- compare_plot(input$midyear, input$minGames,
                          input$position, input$type,
                          input$xvar,
                          input$hof)
      out$S$MidYearLo <- input$midyear[1]
      out$S$MidYearHi <- input$midyear[2]
      out$S$MinInnings <- input$minInnings
      out$S$HOF <- input$hof
      write.csv(out$S, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
