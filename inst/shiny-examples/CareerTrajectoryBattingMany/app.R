library(readr)
library(dplyr)
library(ggplot2)
library(Lahman)
library(geomtextpath)
library(purrr)

# data is read from a Github repository

fg <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/woba_wts.csv")

compare_plot2 <- function(midYearRange, minPA,
                          measure, xvar, hof, fg){
  # find career PA and midyears and filter
  Batting %>%
    mutate(PA = ifelse(is.na(SF) == FALSE,
                       AB + BB + SH + SF + HBP,
                       AB + BB + SH + HBP)) %>%
    group_by(playerID) %>%
    summarize(minYear = min(yearID),
              maxYear = max(yearID),
              midYear = (minYear + maxYear) / 2,
              PA = sum(PA),
              .groups = "drop")  %>%
    filter(midYear <= midYearRange[2],
           midYear >= midYearRange[1],
           PA >= minPA) -> C_info

  HOF_label <- ""
  if(hof == "yes"){
    hof_players <- filter(HallOfFame,
                          inducted == "Y") %>%
      select(playerID) %>% pull()
    C_info <- filter(C_info,
                     playerID %in% hof_players)
    HOF_label <- "HOF"
  }
  # compute stats for each player in list
  Batting %>%
    filter(playerID %in% C_info$playerID) %>%
    inner_join(select(Master, playerID,
                      nameFirst, nameLast),
               by = "playerID") %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    group_by(Name, yearID) %>%
    summarize(H = sum(H),
              AB = sum(AB),
              HR = sum(HR),
              H = sum(H),
              X2B = sum(X2B),
              X3B = sum(X3B),
              X1B = H - X2B - X3B - HR,
              TB = X1B + 2 * X2B + 3 * X3B + 4 * HR,
              BB = sum(BB),
              HBP = sum(HBP),
              SF = sum(SF),
              SO = sum(SO),
              AVG = H / AB,
              RC = TB * (H + BB) / (AB + BB),
              .groups = "drop") -> S
  # merge fangraphs weights for wOBA
  # compute wOBA for each player each season
  inner_join(S, select(fg, Season, wBB, wHBP, w1B,
                           w2B, w3B, wHR),
             by = c("yearID" = "Season"))  %>%
    mutate(wOBA_num = (wBB * BB + wHBP * HBP + w1B * X1B +
                         w2B * X2B + w3B * X3B + wHR * HR),
           wOBA_den = ifelse(is.na(SF) == FALSE,
                             AB + BB + SF + HBP,
                             AB + BB + HBP),
           wOBA = wOBA_num / wOBA_den,
           BB_Rate = 100 * BB / wOBA_den,
           SO_Rate = 100 * SO / wOBA_den,
           HR_Rate = 100 * HR / wOBA_den) -> S
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
  if(measure == "AVG"){
    S$Outcome <- S$AVG
    S$Weight <- S$AB
    YLAB <- "AVG"
  }
  if(measure == "HR Rate"){
    S$Outcome <- S$HR_Rate
    S$Weight <- S$wOBA_den
    YLAB = "Home Run Rate"
  }
  if(measure == "wOBA"){
    S$Outcome <- S$wOBA
    S$Weight <- S$wOBA_den
    YLAB = "wOBA"
  }
  if(measure == "RC"){
    S$Outcome <- S$RC
    S$Weight <- S$wOBA_den
    YLAB = "RC"
  }
  if(measure == "SO Rate"){
    S$Outcome <- S$SO_Rate
    S$Weight <- S$wOBA_den
    YLAB = "SO Rate"
  }
  if(measure == "BB Rate"){
    S$Outcome <- S$BB_Rate
    S$Weight <- S$wOBA_den
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
  # the graph
  plot1 <- ggplot(S,
                  aes(XVAR, Outcome, color = Name,
                      weight = Weight,
                      label = Name)) +
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
                          midYearRange[2], "), Min PA: ",
                          minPA, sep = "")) +
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
  list(plot1 = plot1, S = S)
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("slate"),
  h2("Comparing Many Career Batting Trajectories"),
  column(3,
  sliderInput("midyear", "Select Range of Mid Season:",
              1900, 2020,
              value = c(1980, 1985), sep = ""),
  sliderInput("minpa", "Select Minimum Career PA:",
              1000, 12000, 9000, sep = ""),
  radioButtons("type",
               "Select Measure:",
               c("AVG", "HR Rate", "wOBA", "RC",
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
  downloadButton("downloadData", "Download Data"),
  ),
  column(9,
         plotOutput("plot1",
                    height = '500px'))
)
server <- function(input, output, session) {
  options(warn=-1)

  output$plot1 <- renderPlot({
   compare_plot2(input$midyear, input$minpa,
                 input$type, input$xvar,
                 input$hof,
                 fg)$plot1
  }, res = 96)

  output$downloadData <- downloadHandler(
    filename = "trajectory_output.csv",
    content = function(file) {
      out <- compare_plot2(input$midyear, input$minpa,
                           input$type, input$xvar,
                           input$hof, fg)
      out$S$MidYearLo <- input$midyear[1]
      out$S$MidYearHi <- input$midyear[2]
      out$S$MinPA <- input$minpa
      out$S$HOF <- input$hof
      write.csv(out$S, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
