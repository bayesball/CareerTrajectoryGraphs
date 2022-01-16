library(dplyr)
library(readr)
library(ggplot2)
library(Lahman)
library(geomtextpath)
library(purrr)

fg <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/woba_wts.csv")

compare_plot <- function(midYearRange, minIP,
                         measure, xvar, hof, fg){

  Pitching %>%
    mutate(IP = IPouts / 3) %>%
    group_by(playerID) %>%
    summarize(minYear = min(yearID),
              maxYear = max(yearID),
              midYear = (minYear + maxYear) / 2,
              IP = sum(IP),
              .groups = "drop")  %>%
    filter(midYear <= midYearRange[2],
           midYear >= midYearRange[1],
           IP >= minIP) -> C_info

  HOF_label <- ""
  if(hof == "yes"){
    hof_players <- filter(HallOfFame,
                          inducted == "Y") %>%
      select(playerID) %>% pull()
    C_info <- filter(C_info,
                     playerID %in% hof_players)
    HOF_label <- "HOF"
  }

  Pitching %>%
    filter(playerID %in% C_info$playerID) %>%
    inner_join(select(Master, playerID,
                      nameFirst, nameLast),
               by = "playerID") %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    group_by(Name, yearID) %>%
    summarize(IP = sum(IPouts / 3),
              H = sum(H),
              HR = sum(HR),
              ER = sum(ER),
              BB = sum(BB),
              SO = sum(SO),
              HBP = sum(HBP),
              ERA = ER / IP * 9,
              FIP = ((HR * 13) + (3 * (BB + HBP)) -
                       (2 * SO)) / IP,
              WHIP = (H + BB) / IP,
              SO_Rate = 9 * SO / IP,
              BB_Rate = 9 * BB / IP,
              .groups = "drop") -> S
  # merge fangraphs weights for wOBA
  # compute wOBA for each player each season
  inner_join(S, select(fg, Season, cFIP),
             by = c("yearID" = "Season"))  %>%
    mutate(FIP = FIP + cFIP) -> S
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
  if(measure == "ERA"){
    S$Outcome <- S$ERA
    S$Weight <- S$IP
    YLAB <- "ERA"
  }
  if(measure == "FIP"){
    S$Outcome <- S$FIP
    S$Weight <- S$IP
    YLAB = "FIP"
  }
  if(measure == "WHIP"){
    S$Outcome <- S$WHIP
    S$Weight <- S$IP
    YLAB = "WHIP"
  }
  if(measure == "SO Rate"){
    S$Outcome <- S$SO_Rate
    S$Weight <- S$IP
    YLAB = "SO Rate"
  }
  if(measure == "BB Rate"){
    S$Outcome <- S$BB_Rate
    S$Weight <- S$IP
    YLAB = "BB Rate"
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
  #  geom_point(size = 3) +
    geom_textsmooth(se = FALSE,
                method = "loess",
                formula = "y ~ x") +
    ylab(YLAB) +
    xlab(XLAB) +
    labs(title = paste(HOF_label, YLAB,
                       "Career Trajectories"),
         color = "Player",
         subtitle = paste("Midyear: (",
                          midYearRange[1], ", ",
                          midYearRange[2], "), Min Innings: ",
                          minIP, sep = "")) +
    theme(text = element_text(size = 15),
          plot.title = element_text(colour = "red",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8,
                                    angle = 0),
          plot.subtitle = element_text(colour = "blue",
                                    size = 16,
                                    hjust = 0.5, vjust = 0.8,
                                    angle = 0)) +
    theme(legend.position = "none")
  list(plot1 = plot1, S = S)
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("slate"),
  h2("Comparing Many Career Pitching Trajectories"),
  column(3,
         sliderInput("midyear", "Select Range of Mid Season:",
                     1900, 2020,
            value = c(1980, 1985), sep = ""),
  sliderInput("minInnings", "Select Minimum Innings Pitched:",
              1000, 5000, 3000, sep = ""),
  radioButtons("type",
               "Select Measure:",
               c("ERA", "WHIP", "FIP",
                 "SO Rate", "BB Rate"),
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
     compare_plot(input$midyear,
                  input$minInnings,
                  input$type,
                  input$xvar,
                  input$hof,
                  fg)$plot1
  }, res = 96)
  output$downloadData <- downloadHandler(
    filename = "trajectory_output.csv",
    content = function(file) {
      out <- compare_plot(input$midyear,
                          input$minInnings,
                          input$type,
                          input$xvar,
                          input$hof,
                          fg)
      out$S$MidYearLo <- input$midyear[1]
      out$S$MidYearHi <- input$midyear[2]
      out$S$MinInnings <- input$minInnings
      out$S$HOF <- input$hof
      write.csv(out$S, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
