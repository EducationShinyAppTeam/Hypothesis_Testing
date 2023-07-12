# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(dplyr)
library(ggplot2)
library(DT)
library(boastUtils)


# Global Constants, Functions, and Data ----
# Got data from basketball-reference.com
# Download data as csv then multiply FTP by 100 then load in data
playerData <- read.csv(file = "nba22Full.csv", header = TRUE) %>%
  dplyr::select(Player, G, FT, FTA, FTP)

minGames <- min(playerData$G)
maxGames <- max(playerData$G)
minAttempts <- min(playerData$FTA)
maxAttempts <- max(playerData$FTA)

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "purple",
    ## Header ----
    dashboardHeader(
      title = "Hypothesis Testing",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Hypothesis_Testing")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "Overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Filtering", tabName = "Explore", icon = icon("wpexplorer")),
        menuItem("Testing", tabName = "Testing", icon = icon("wpexplorer")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      ### Overview ----
      tabItems(
        tabItem(
          tabName = "Overview",
          withMathJax(),
          h1("Hypothesis Test for One Sample Proportion"),
          p("There are two goals with this app. First, you will explore what
            happens to a population when different filters are applied to your
            data. Second, you'll explore the relationships between null hypothesis
            testing for a single proportion and a associated confidence interval."),
          p("In both cases, you'll be working with NBA player's free throw
            success percentages. Keep in mind that percentage is
            just a proportion mulitplied by 100. Thus, we can divide a player's
            free throw percentage by 100 to get a proportion."),
          tags$figure(
            class = "centerFigure",
            tags$image(
              src = "freeThrow.jpg",
              height = 275,
              alt = "Free throw during a Knicks versus Pistons game"
            ),
            tags$figcaption("Pictured free throw in a Knicks vs. Pistons game")
          ),
          h2("Instructions"),
          tags$ul(
            tags$li("On the Prerequisites page you will learn about filtering,
                    hypothesis tests, and confidence intervals. The information 
                    here will help you understand the app."),
            br(),
            tags$li("On the Filtering page you will look at how the
                    population distribution of all the players' free throw
                    percentages is affected by filtering (restricting attention
                    to a subpopulation)."),
            br(),
            tags$li("On the Testing page you will examine hypothesis tests
                    about an individual player's free throw percentages along with
                    the related confidence interval."),
            br(),
            tags$li("Head to the Prerequisites page to begin!"),
            br(),
            div(
              style = "text-align: center;",
              bsButton(
                inputId = "prereq",
                label = "Prerequisites",
                size = "large",
                icon = icon("book")
              )
            )
          ),
          br(),
          h2("Acknowledgements"),
          p("This app was originally developed and programmed in 2017 by David
            Robinson. Hypothesis testing was added in 2018 by Ryan Voyack. The app
            was updated in 2020 by Xuefei Wang and Neil Hatfield and by Jing Fu in
            2022, and by Rob Chappell in 2023.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 7/12/2023 by RC.")
          )
        ),
        ###Prerequisites ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the 
            following general explanations of terms."),
          tags$ul(
            tags$li(
              tags$strong("Filtering: "), "When you apply a data filter, you 
              essentially restrict your attention to a smaller part of the 
              population of interest. This allows you to create a subset of the 
              data for analysis that satisfies what sub-group you want to focus on.
              Filtering is temporary in the sense that we keep the complete data
              set is kept (i.e., we don't delete the data that don't match our 
              filter), allowing us to change the filter or apply other filters
              to better address other questions."
            ),
            tags$li(
              tags$strong("Hypothesis tests: "), "A statistical hypothesis test 
              allows us to address whether a particular model (i.e., a null hypothesis)
              provides a reasonable explanation of our data. We do this by examining
              a test statistic designed to measure how inconsistent our observed
              data are from the null hypothesis. We can use this test statistic
              to then calculate the probability of getting a result (i.e., our
              observed value of the test statistic) at least as extreme as what
              we've observed if the null hypothesis were true. This probability 
              is called the ", tags$em("p"), "-value. Smaller ", tags$em("p"), 
              "-values are generally viwed as evidence that the data are not 
              consistent with the null hypothesis."
            ),
            tags$li(
              tags$strong("Confidence intervals: "), "An interval estimate that
              gives a set of values for our parameter of interest. If we were to
              use these values in the null hypothesis, we would find that our data
              would be consistent with such models. Equivalently, these values in
              our null hypothesis would result in our finding of ",
              tags$em("p"), "-values greater our chosen siginficance level 
              (i.e., 1-confidence level)."
            )
          )
        ),
        ### Explore ----
        tabItem(
          tabName = "Explore",
          withMathJax(),
          h2("Filtered Populations"),
          p("Explore what happens to a population distribution when you 
            apply a filter and create subpopulations. Select the filter you wish
            to apply and then move the sliders to adjust the filter (all values
            between the two sliders will be kept)."),
          p("Each player attempted some number of free throw shots across the
            entire season. The Free Throw Percentage tells us what percentage
            of a player's free throw attempts they made."
          ),
          fluidRow(
            column(
              width = 4,
              h3("Filter Controls"),
              wellPanel(
                selectInput(
                  inputId = "filterType",
                  label = "Select which filter to use",
                  choices = c(
                    "Games Played" = "G",
                    "Free Throw Attempts" = "FTA"
                  )
                ),
                sliderInput(
                  inputId = "exploreFilter",
                  label = "Interval selected",
                  min = 1,
                  max = 100,
                  value = c(1, 100)
                )
              )
            ),
            column(
              ## Histogram
              width = 8,
              plotOutput("exploreHistogram"),
            )
          ),
          h3("Questions to Ponder"),
          tags$ul(
            tags$li("What happens to the histogram as you change the number of 
                    games played or the number of free throws attempted?"),
            tags$li("Which interval of values for number of games played (or the
                    number of free throws attempted) has more variation in the
                    percentage of successful free throws?"),
            tags$li("Think about the values of the median and mean success
                    percentages. What interval of games played (attempts made)
                    would you anticipate most players to fall into? How could
                    you check?")
          )
        ),
        ### Testing ----
        tabItem(
          tabName = "Testing",
          withMathJax(),
          h2("Testing Proportions"),
          p("Here you will explore hypothesis testing and confidence intervals 
            for a single proportion. To begin, you'll need to select a 2022-2023
            NBA player to study. We've filtered the data based on what percentage
            of games they each played during the 2022-2023 season. You may adjust
            the filter as you see fit. If you wish, you may select 'Pick for me'
            and we'll randomly choose a player for you."),
          p("Once you've chosen a player, you'll need to set a null hypothesis.
            You may choose either a pre-set value (the player's true free throw
            proportion or the overall NBA free throw percentage) or set your own.
            You will also need to choose how many free throw shots you want the
            app to simulate for your player. Once ready, press Simulate."),
          hr(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                sliderInput(
                  inputId = "percentGames",
                  label = "Minimum percentage of games played",
                  min = 50,
                  max = 100,
                  value = 80,
                  post = "%"
                ),
                selectInput(
                  inputId = "playerSelect",
                  label = "Select a player",
                  choices = list(
                    "Select a player",
                    "Pick for me",
                    "Players"
                  )
                ),
                radioButtons(
                  inputId = "nullSetMethod",
                  label = "Set the null hypothesis",
                  choices = list(
                    "Player's true value" = "player",
                    "NBA's value" = "nba",
                    "Use the slider to pick up a null value" = "manual"
                  ),
                  selected = "manual"
                ),
                sliderInput(
                  inputId = "nullValue",
                  label = "Null value",
                  min = 0,
                  max = 1,
                  value = 0.5
                ),
                sliderInput(
                  inputId = "sampleSize",
                  label = "Number of shots to simulate",
                  min = 5,
                  max = 60,
                  value = 30
                ),
                bsButton(
                  inputId = "simulate",
                  label = "Simulate",
                  icon = icon("retweet"),
                  size = "large"
                )
              )
            ),
            column(
              width = 8,
              plotOutput("samplePlot", height = "300px"),
              checkboxInput(
                inputId = "showNHST",
                label = "Show null hypothesis test results"
              ),
              conditionalPanel(
                condition = "input.showNHST",
                DT::dataTableOutput(
                  outputId = "testResults",
                  width = "75%"
                )
              ),
              checkboxInput(
                inputId = "showCI",
                label = "Show confidence interval plot"
              ),
              conditionalPanel(
                condition = "input.showCI",
                plotOutput("ciPlot", height = "250px"),
              )
            )
          ),
          h3("Questions to Ponder"),
          tags$ol(
            tags$li("After simulating your player shooting free throws, can you
                    determine whether we can reject the null hypothesis from the
                    bar chart? Why or why not?"),
            tags$li("What happens when you press the Simulate button again?
                    And again?"),
            tags$li("What happens when you change the number of shots (and press
                    the Simulate button)?"),
            tags$li("Looking at the Null Hypothesis Test Results, what do the
                    numbers in the table mean?"),
            tags$li("Looking at the Null Hypothesis Test Results, what would you
                    decide?"),
            tags$li("What is the relationship between the confidence interval
                    plot and the simulated free throws plot?"),
            tags$li("What happens to both the test results and the confidence
                    interval plot when you change the Null value slider?"),
            tags$li("Does increasing the sample size make it easier or
                harder to get a significantly low p-value? (Hint: Hit the
                'Simulate' button several times to see how different samples
                under the same conditions behave.)")
          )
        ),
        # References ----
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v0.61.1). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield., N. J. (2023). boastUtils: BOAST utilities.
            (v0.1.11.2). [R Package]. Available from
            https://github.com/EducationShinyappTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2021). shinydashboard: Create dashboards
            with 'Shiny'. (v0.7.2). [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J.J., Sievert, C., Schloerke, B.,
            Xie, Y., Allen, J., McPherson, J., Dipert, A., and Borges, B. (2022).
            shiny: Web application framework for R. (v1.7.4). [R Package].
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Berera, M., Wikimedia Commons. (2021). New York Knicks vs. Detroit
            Pistons December 2021 24 (free throw)"
          ),
          p(
            class = "hangingindent",
            "Sports Reference LLC (2023). Basketball-Reference.com - 
            Season Totals 2022. Available from
            https://www.basketball-reference.com/leagues/NBA_2022_totals.html"),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2023). shinyWidgets: Custom
            inputs widgets for shiny. (v0.7.6). [R Package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
            ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            (v3.4.2). [R Package]. New York:Springer-Verlag. Available from
            https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K., and Vaughan, D.
            (2023). dplyr: A grammar of data manipulation. (v1.1.2). [R Package].
            Available from https://CRAN.R-project.org/package=dplyr"
          ),
          p(
            class = "hangingindent",
            "Xie Y, Cheng J, Tan X (2023). DT: A Wrapper of the JavaScript 
            Library 'DataTables'. (v0.28). [R package]. Available from
            https://CRAN.R-project.org/package=DT"
          ),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define the server ----
server <- function(input, output, session) {
  
  ## Information button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Information",
        text = "Explore the free throw shooting accuracy of NBA players
            filtered by how many games or shots they took or test
            hypotheses about your favorite player's accuracy.",
        type = "info"
      )
    }
  )
  
  ## Prereq Button ----
  observeEvent(
    eventExpr = input$prereq, 
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )

  ## Explore Page Code ----
  ### Update the filter slider ----
  observeEvent(
    eventExpr = input$filterType, 
    handlerExpr = {
    if (input$filterType == "G") {
      updateSliderInput(
        session = session,
        inputId = "exploreFilter",
        label = "Number of games played",
        min = minGames,
        max = maxGames,
        value = c(minGames, maxGames)
      )
    } else {
      updateSliderInput(
        session = session,
        inputId = "exploreFilter",
        label = "Number of attempted free throws",
        min = minAttempts,
        max = maxAttempts,
        value = c(minAttempts, maxAttempts)
      )
    }
  })

  ### Explore Page's Data Set ----
  explorePageData <- reactive( 
    x = {
    if (input$filterType == "G") {
      temp1 <- playerData %>%
        filter(
          dplyr::between(G, input$exploreFilter[1], input$exploreFilter[2])
        )
    } else {
      temp1 <- playerData %>%
        filter(
          dplyr::between(FTA, input$exploreFilter[1], input$exploreFilter[2])
        )
    }
    return(temp1)
  })
  
  
  observe(
    x = {
    expAltText <- NULL
    
    if (input$filterType == "G") {
      if (input$exploreFilter[1] < 27) {
        expAltText <- "Looking at the histogram, there is a large number of
        players with a zero for their percent, then a gap between them and the
        next group of players. The graph is left-skewed with an average of
        around 67%."
      } else if (27 <= input$exploreFilter[1]) {
        expAltText <- "Looking at the histogram, there is no longer any players
        with a zero percent. The graph is more left-skewed as the minimum value
        gets greater."
      }
    } else if (input$filterType == "FTA") {
      if (input$exploreFilter[1] == 0) {
        expAltText <- "Looking at the histogram, there is a number of players
        with zero free throw attempts, along with a gap between them and the
        next group of players around 25. There are around 230 players in total."
      } else if (0 < input$exploreFilter[1]) {
        expAltText <- "Looking at the histogram, there are no more players with
        zero attempts. The minimum number of attempts is around 30, and the
        number of players has decreased to around 100."
      } else if (75 <= input$exploreFilter[1]) {
        expAltText <- "Looking at the histogram, there are only 10 players left
        in the histogram. The average is around 80%, and the minimum is around
        65%."
      }
    }
  
  
  ### Free Throw Histogram ----
  output$exploreHistogram <- renderPlot(
    expr = {
      ggplot(
        data = explorePageData(),
        mapping = aes(x = FTP)
      ) +
        geom_histogram(
          binwidth = 10,
          boundary = 0,
          closed = "left",
          col = "black",
          fill = psuPalette[6],
          na.rm = TRUE
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = 0)) +
        scale_x_continuous(
          limits = c(0, 100),
          expand = expansion(mult = 0, add = c(0, 5))
        ) +
        labs(
          title = "Histogram of Free Throw Percentages",
          x = "Percent of attempts made",
          y = "Number of players"
        ) +
        theme_bw() +
        theme(
          plot.caption = element_text(size = 18),
          text = element_text(size = 18),
          axis.title = element_text(size = 16)
        )
    },
    alt = expAltText
  )
})

  
  ## Testing Page Code ----
  challengeData <- reactiveVal()

  ### Reactive Player List ----
  challengePlayerList <- eventReactive(
    eventExpr = input$percentGames,
    valueExpr = {
      temp2 <- playerData %>%
        filter(G >= floor(input$percentGames / 100 * maxGames))
      return(temp2$Player)
    }
  )

  ### Update Player List ----
  observe(
    x = {
      updateSelectInput(
        session = session,
        inputId = "playerSelect",
        choices = c(
          "Pick for me",
          "----------------------",
          challengePlayerList()
        )
      )
    }
  )
  
  
  ### Picking a Player ----
  observeEvent(
    eventExpr = input$simulate,
    handlerExpr = {
      badChoices <- c("Select a player", "----------------------", "player")
      if (input$playerSelect %in% badChoices) {
        sendSweetAlert(
          session = session,
          title = "Player Selection Error",
          text = "You need to either pick a player you wish to follow or select
          the 'Pick for me' option to have us randomly select a player for you.",
          type = "error"
        )
      } else if (input$playerSelect == "Pick for me") {
        randPlayer <- sample(challengePlayerList(), size = 1, replace = FALSE)
        updateSelectInput(
          session = session,
          inputId = "playerSelect",
          selected = randPlayer
        )
        challengeData(
          playerData %>%
            filter(Player == randPlayer)
        )
      } else {
        challengeData(
          playerData %>%
            filter(Player == input$playerSelect)
        )
      }
    },
    ignoreInit = TRUE
  )
  
  ### Setting null hypothesis value ----
  observeEvent(
    eventExpr = input$nullSetMethod, 
    handlerExpr = {
      if (input$nullSetMethod == "player") {
        updateSliderInput(
          session = session,
          inputId = "nullValue",
          value = round(challengeData()$FTP / 100, digits = 2)
        )
      } else if (input$nullSetMethod == "nba") {
        updateSliderInput(
          session = session,
          inputId = "nullValue",
          value = round(mean(playerData$FTP, na.rm = TRUE) / 100, digits = 2)
        )
      }
    }
  )
  
  observeEvent(
    eventExpr = input$nullValue,
    handlerExpr = {
      if (input$nullSetMethod == "player" &&
          input$nullValue != round(challengeData()$FTP / 100, digits = 2)) {
        updateRadioButtons(
          session = session,
          inputId = "nullSetMethod",
          selected = "manual"
        )
      } else if (input$nullSetMethod == "nba" &&
                 input$nullValue != round(mean(playerData$FTP, na.rm = TRUE) / 100, digits = 2)) {
        updateRadioButtons(
          session = session,
          inputId = "nullSetMethod",
          selected = "manual"
        )
      }
    }
  )
  
  ### Simulate button ----
  simulatedData <- eventReactive(
    eventExpr = input$simulate,
    valueExpr = {
      validate(
        need(
          expr = challengeData(),
          message = "Challenge data missing"
        )
      )
      rbinom(n = input$sampleSize, size = 1, prob = challengeData()$FTP / 100)
    },
    ignoreNULL = FALSE
  )
  
  ### Bar chart alt text ----
  
  valuesSampPlot <- reactiveValues(madePercent = NULL, missPercent = NULL)
  
  observeEvent(
    eventExpr = challengeData(),
    handlerExpr = {
      valuesSampPlot$madePercent <- sum(simulatedData() == 1) / length(simulatedData())
      valuesSampPlot$missPercent <- 1 - valuesSampPlot$madePercent
    }
  )
  
  observe(
    x = {
    sampAltTxt <- reactive({
      paste0(
        "Simulated Free Throws for ", challengeData()$Player,
        ". The plot shows the percentage of shots made (", round(valuesSampPlot$madePercent * 100, 2), "%)",
        " and missed (", round(valuesSampPlot$missPercent * 100, 2), "%).",
        " The x-axis represents the results (shots made or missed),",
        " and the y-axis represents the percentage."
      )
    })
    
    ### Bar Chart ----
    
    output$samplePlot <- renderPlot(
      expr = {
      validate(
        need(
          expr = challengeData(),
          message = "Select a player, then set parameters, and finally, press
          the Simulate button."
        )
      )
      ggplot(
        data = data.frame(
          attempt = ifelse(simulatedData() == 1, "Shots made", "Shots missed")
        ),
        mapping = aes(x = attempt)
      ) +
        geom_bar(
          mapping = aes(y = after_stat(count) / sum(after_stat(count))),
          fill = psuPalette[6],
          col = "black"
        ) +
        labs(
          title = paste("Simulated Free Throws for", challengeData()$Player),
          x = "Results",
          y = "Percentage"
        ) +
        scale_y_continuous(
          limits = c(0, 1),
          expand = expansion(mult = c(0, 0.01))
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(size = 24),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)
        )
    }, alt = sampAltTxt())
  })
  
  ### CI alt text ----
  
  valuesCI <- reactiveValues(pHat = NULL, sePhat = NULL, lowerbound = NULL, upperbound = NULL)
  
  observeEvent(
    eventExpr = challengeData(),
    handlerExpr = {
      # Update reactive values
      valuesCI$pHat <- mean(simulatedData(), na.rm = TRUE)
      valuesCI$sePhat <- sqrt(valuesCI$pHat * (1 - valuesCI$pHat) / length(simulatedData()))
      valuesCI$lowerbound <- max(valuesCI$pHat - 1.96 * valuesCI$sePhat, 0)
      valuesCI$upperbound <- min(valuesCI$pHat + 1.96 * valuesCI$sePhat, 1)
    }
  )
  
  observe({
    ciAltText <- reactive(
      x = {
      paste("A plot showing the confidence interval for the success proportion of free throws.",
            "The plot ranges from", valuesCI$lowerbound, "to", valuesCI$upperbound, 
            "with the estimated proportion as", valuesCI$pHat, ".")
    })
    
    ### Confidence Interval ----
    
    output$ciPlot <- renderPlot(
      expr = {
      validate(
        need(
          expr = challengeData(),
          message = "Select a player, then set parameters, and finally, press the Simulate button."
        )
      )
      
      localCIScale <- if (between(input$nullValue, valuesCI$lowerbound, 
                                  valuesCI$upperbound)) {
        scale_color_manual(
          values = c(
            "estimate" = psuPalette[1],
            "null" = psuPalette[7]
          ),
          labels = c(
            "estimate" = expression(paste(hat(p), " and CI")),
            "null" = expression(p[0])
          )
        )
      } else {
        scale_color_manual(
          values = c(
            "estimate" = psuPalette[2],
            "null" = psuPalette[7]
          ),
          labels = c(
            "estimate" = expression(paste(hat(p), " and CI")),
            "null" = expression(p[0])
          )
        )
      }
      
      ggplot(
        data = data.frame(
          point = valuesCI$pHat,
          lower = max(valuesCI$pHat - 1.96 * valuesCI$sePhat, 0),
          upper = min(valuesCI$pHat + 1.96 * valuesCI$sePhat, 1)
        )
      ) +
        geom_pointrange(
          mapping = aes(
            y = 0,
            x = point,
            xmin = lower,
            xmax = upper,
            color = "estimate"
          ),
          key_glyph = "path",
          size = 2
        ) +
        geom_point(
          mapping = aes(x = input$nullValue, y = 0, color = "null"),
          size = 12,
          shape = 18
        ) +
        scale_x_continuous(limits = c(0, 1)) +
        scale_y_continuous(limits = c(-0.1, 0.1)) +
        localCIScale +
        labs(
          title = "Confidence Interval for Success Proportion",
          x = "Proportion of Successful Free Throws",
          y = NULL,
          color = NULL
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(size = 24),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.text = element_text(size = 18),
          legend.position = "bottom"
        )
    }, alt = ciAltText())
  })
  
  ### Null Hypothesis Test Results ----
  output$testResults <- DT::renderDataTable(
    expr = {
      validate(
        need(
          expr = challengeData(),
          message = "Challenge data missing"
        )
      )
      pHat <- sum(simulatedData()) / length(simulatedData())
      sePhat <- sqrt(pHat * (1 - pHat) / length(simulatedData()))
      z <- (pHat - input$nullValue) / (sePhat)
      temp3 <- binom.test(
        x = sum(simulatedData()),
        n = length(simulatedData()),
        p = input$nullValue,
        alternative = "two.sided",
        conf.level = 0.95
      )
      data.frame(
        row.names = c("Normal Approximation", "Exact Binomial"),
        Statistic = 
          ifelse(
            (is.finite(c(z,temp3$estimate))),
            round(c(z, temp3$estimate), digits = 3),
            c("Infinite","Infinite")),
        `p-value` = ifelse(
          c(2*pnorm(-abs(z)), temp3$p.value) < 0.001,
          "< 0.001",
          round(
            c(2*pnorm(-abs(z)), temp3$p.value),
            digits = 3
          )
        )
      )
    },
    caption = HTML(
      paste0("Null Hypothesis Test Results with p", tags$sub("0"), " = ", 
             input$nullValue, " and n = ", length(simulatedData())
      )
    ),
    style = "bootstrap4",
    rownames = TRUE,
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      ordering = FALSE,
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    )
  )
  
  ## Use Simulate to disable/rename buttons ----
  observeEvent(
    eventExpr = input$simulate,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "simulate",
        label = "Resimulate"
      )
    }
  )
}
  
# Boast app call ----
boastUtils::boastApp(ui = ui, server = server)