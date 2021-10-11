# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(DT)
library(boastUtils)

# Global Constants, Functions, and Data ----
playerdata <- read.csv(file = "NBA1819.csv", header = TRUE)
playerData <- playerdata %>%
  dplyr::select(Player, G, FT, FTA, `FT.`)

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
      tags$li(class = "dropdown",
              actionLink(inputId = "info", label = icon("info"), class = "myClass")),
      tags$li(
        class = "dropdown",
        tags$a(
          target = "_blank", icon("comments"),
          href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Hypothesis_Testing"
        )
      ),
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
        menuItem("Explore Filtering", tabName = "Explore", icon = icon("wpexplorer")),
        menuItem("Explore Testing", tabName = "Testing", icon = icon("wpexplorer")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
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
            happens to a population when apply different filters to your data.
            Second, you'll explore the relationships between null hypothesis
            testing for a single proportion and a confidence interval for a single
            proportion."),
          p("In both cases, you'll be working with NBA player's Free Throw
            percentages. These are what percent of the free throws each player
            attempted which they actually made. Keep in mind that percentage is
            just a proportion mulitplied by 100. Thus, we can divide a player's
            free throw percentage by 100 to get a proportion."),
          div(
            style = "text-align: center;",
            img(
              src = "fthrow2.png",
              alt = "This picture shows threee famous basketball players that are
                  doing free throw.",
              width = "40%"
            ),
            p("Picture by Hoop Hounds (2016)")
          ),
          h2("Instructions"),
          tags$ul(
            tags$li("On the Explore Filtering page you will look at how the
                    population distribution of all the players' free throw
                    percentages is affected by filtering (restricting attention
                    to a subpopulation)."),
            div(
              style = "text-align: center;",
              bsButton(
                inputId = "go1",
                label = "Filtering",
                size = "large",
                icon = icon("bolt")
              )
            ),
            br(),
            tags$li("On the Explore Testing page you will explore hypothesis tests
                    about an individual player's free throw percentages."),
            div(
              style = "text-align: center;",
              bsButton(
                inputId  = "go2",
                label = "Testing",
                size = "large",
                icon = icon("bolt")
              )
            )
          ),
          br(),
          h2("Acknowledgements"),
          p("This app was originally developed and programmed in 2017 by David
            Robinson. Hypothesis testing was added in 2018 by Ryan Voyack. In
            2020, Xuefei Wang and Neil Hatfield brought the app up to current
            Style Guide standards and implemented many improvements.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 11/5/2020 by NJH.")
          )
        ),
        ### Explore ----
        tabItem(
          tabName = "Explore",
          withMathJax(),
          h2("Filtered Populations"),
          p("Explore what happens to a population distribution when you begin to
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
                  inputId ="filterType",
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
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('exploreHistogram').setAttribute('aria-label',
                `Histogram of free thow percentages which you may manipulate by
                using the filter controls`)
                })"
              ))
            )
          ),
          h3("Questions to Ponder"),
          tags$ul(
            tags$li("What happens as your change the number of games played or
                    the number of free throws attempted to the histogram?"),
            tags$li("Which interval of values for number of games played (or the
                    number of free throws attempted) has more variation in the
                    percentage of successful free throws?"),
            tags$li("Think about the values of the median and mean success
                    percentages. What interval of games played (attempts made)
                    would you anticipate most players to fall into? How could
                    you check?")
          )
        ),
        ### Challenge ----
        tabItem(
          tabName = "Testing",
          withMathJax(),
          h2("Testing Proportions"),
          p("Here, you will explore hypothesis testing for a single proportion.
            To begin, you'll need to select 2018-2019 NBA player to study. We've
            filtered the data based on what percentage of games they each played
            during the 2018-2019 season. You may adjust the filter as you see fit.
            If you wish, you may select 'Pick for me' and we'll randomly choose
            a player for you."),
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
                bsButton(
                  inputId = "pickPlayer",
                  label = "Choose",
                  size = "large"
                ),
                conditionalPanel(
                  condition = "input.playerSelect != 'Select a player' &&
                  input.playerSelect != '----------------------' &&
                  input.playerSelect != 'Pick for me' &&
                  input.pickPlayer > 0",
                  br(),
                  radioButtons(
                    inputId = "nullSetMethod",
                    label = "Set the null hypothesis",
                    choices = list(
                      "Player's true value" = "player",
                      "NBA's value" = "nba",
                      "Custom-use the slider" = "manual"
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
                    icon = icon("bolt"),
                    size = "large"
                  )
                )
              )
            ),
            column(
              width = 8,
              plotOutput("samplePlot", height = "300px"),
              tags$script(HTML(
                "$(document).ready(function() {
                  document.getElementById('samplePlot').setAttribute('aria-label',
                  `This bar plot shows the simulated free throws for your chosen
                  player.`)
                  })"
              )),
              checkboxInput(
                inputId = "showNHST",
                label = "Show null hypothesis test results"
              ),
              conditionalPanel(
                condition = "input.showNHST == 1",
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
                condition = "input.showCI == 1",
                plotOutput("ciPlot", height = "250px"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('ciPlot').setAttribute('aria-label',
                  `This plot shows the confidence interval associated with your
                  sample and your null hypothesis test.`)
                  })"
                )),
              )
            )
          ),
          h3("Questions to Ponder"),
          tags$ol(
            tags$li("After simulating your player shooting free throws, can you
                    determine whether we can reject the null hypothesis from the
                    bar chart? Why or why not?"),
            tags$li("What happens when press the Simulate button again?
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
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny,
            R package. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities, R
            Package.
            Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
            dashboards with 'Shiny', R Package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019). shiny: Web application framework for R, R Package.
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Hoop Hounds. Free Throw Shooting Tips. Retrieved from
            http://www.hoophounds.com/articles/free-throw-shooting-tips/"
          ),
          p(
            class = "hangingindent",
            "Meschiari, S. (2015). latex2exp: Use LaTeX Expressions in Plots,
            R Package. Available from https://CRAN.R-project.org/package=latex2exp"
          ),
          p(
            class = "hangingindent",
            "NBA (2019), NBA Advanced Stats, [It contains information about
            performances of NBA players in 2018-2019]. Available at
            https://stats.nba.com/leaders/?Season=2018-19&SeasonType=Regular%20Season&PerMode=Totals"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., Granjon, D. (2020). shinyWidgets:
            Custom Inputs Widgets for Shiny, R Package. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            'Wickham, H. (2016). "ggplot2: Elegant graphics for data analysis",
            R Package. Springer-Verlag New York. Available at
            https: // ggplot2.tidyverse.org'
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K. (2020).
            dplyr: A Grammar of Data Manipulation, R Package.
            Available from https://CRAN.R-project.org/package=dplyr"
          ),
          p(
            class = "hangingindent",
            "Xie, Y., Cheng, J., and Tan, X. (2020). DT: A wrapper of the
            JavaScript library 'DataTables', R Package. Available from
            https://CRAN.R-project.org/package=DT"
          )
        )
      )
    )
  )
)

# Define the server ----
server <- function(input, output, session) {

  ## Information button ----
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Information",
      text = "Explore the free throw shooting accuracy of NBA players
            filtered by how many games or shots they took or test
            hypotheses about your favorite player's accuracy.",
      type = "info"
    )
  })

  ## Explore Button ----
  observeEvent(input$go1, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "Explore"
    )
  })

  ## Challenge Button ----
  observeEvent(input$go2, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "Testing"
    )
  })

  ## Explore Page Code ----
  ### Update the filter slider ----
  observeEvent(input$filterType, {
    if(input$filterType == "G") {
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
  explorePageData <- reactive({
    if(input$filterType == "G") {
      temp1 <- playerData %>%
        dplyr::filter(dplyr::between(G,
                                     input$exploreFilter[1],
                                     input$exploreFilter[2])
        )
    } else {
      temp1 <- playerData %>%
        dplyr::filter(dplyr::between(FTA,
                                     input$exploreFilter[1],
                                     input$exploreFilter[2])
        )
    }
    return(temp1)
  })

  ### Free Throw Histogram ----
  output$exploreHistogram <- renderPlot({
    ggplot(
      data = explorePageData(),
      mapping = aes(x = FT.)
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
  })

  ## Challenge Page Code ----
  challengeData <- reactiveVal()

  ### Reactive Player List ----
  challengePlayerList <- eventReactive(input$percentGames, {
    temp2 <- playerData %>%
      filter(G >= floor(input$percentGames / 100 * maxGames))
    return(temp2$Player)
  })

  ### Update Player List ----
  observe({
    updateSelectInput(
      session = session,
      inputId = "playerSelect",
      choices = c(
        "Pick for me",
        "----------------------",
        challengePlayerList()
      )
    )
  })

  ### Picking a Player ----
  observeEvent(
    eventExpr = c(input$pickPlayer, input$simulate),
    handlerExpr = {
      badChoices <- c("Select a player", "----------------------", "player")
      if(input$playerSelect %in% badChoices) {
        sendSweetAlert(
          session = session,
          title = "Player Selection Error",
          text = "You need to either pick a player you wish to follow or select the
        'Pick for me' option to have us randomaly select a player for you.",
          type = "error"
        )
      } else if(input$playerSelect == "Pick for me") {
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
  observeEvent(input$nullSetMethod, {
    if(input$nullSetMethod == "player") {
      updateSliderInput(
        session = session,
        inputId = "nullValue",
        value = round(challengeData()$FT. / 100, digits = 2)
      )
    } else if (input$nullSetMethod == "nba") {
      updateSliderInput(
        session = session,
        inputId = "nullValue",
        value = round(mean(playerData$FT., na.rm = TRUE) / 100, digits = 2)
      )
    }
  })

  ### Simulate button ----
  simulatedData <- eventReactive(
    eventExpr = input$simulate,
    valueExpr = {
      validate(
        need(challengeData(),
             message = "Select a player, then set paramters, and finally, press
             the Simulate button."
        )
      )
      rbinom(n = input$sampleSize, size = 1, prob = challengeData()$FT. / 100)
    },
    ignoreNULL = FALSE
  )

  ### Sample Plot ----
  output$samplePlot <- renderPlot({
    validate(
      need(!is.na(simulatedData()[1]),
           message = "Select a player, then set paramters, and finally, press
             the Simulate button."
      )
    )
    if(is.na(simulatedData()[1])) {
      print("Initial pass")
    } else {
      ggplot(
        data = data.frame(
          attempt = ifelse(simulatedData() == 1, "Shots made", "Shots missed")
        ),
        mapping = aes(x = attempt)
      ) +
        geom_bar(
          mapping = aes(y = ..count.. / sum(..count..)),
          fill = psuPalette[6],
          col = "black"
        ) +
        labs(
          title = paste("Simulated Free Throws for", challengeData()$Player),
          x = "Results",
          y = "Percentage"
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(size = 24),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)
        )
    }

  })

  ### Confidence Interval Plot ----
  output$ciPlot <- renderPlot({
    validate(
      need(!is.na(simulatedData()[1]),
           message = "Select a player, then set paramters, and finally, press
             the Simulate button."
      )
    )
    pHat <- mean(simulatedData(), na.rm = TRUE)
    sePhat <- sqrt(pHat * (1 - pHat) / length(simulatedData()))
    ggplot(
      data = data.frame(
        point = pHat,
        lower = max(pHat - 1.96 * sePhat, 0),
        upper = min(pHat + 1.96 * sePhat, 1)
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
        size = 2
      ) +
      geom_point(
        mapping = aes(x = input$nullValue, y = 0, color = "null"),
        size = 12,
        shape = 18
      ) +
      scale_x_continuous(limits = c(0,1)) +
      scale_y_continuous(limits = c(-0.1, 0.1)) +
      scale_color_manual(
        values = c(
          "estimate" = psuPalette[1],
          "null" = psuPalette[7]
        ),
        labels = c(
          "estimate" = parse(text = TeX("$\\widehat{p}$ and CI ")),
          "null" = parse(text = TeX("$p_0$"))
        )
      ) +
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
  })

  ### Null Hypothesis Test Results ----
  output$testResults <- DT::renderDataTable(
    expr = {
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
        row.names = c("Normal Approximation", "Exact Binominal"),
        Statistic = round(c(z, temp3$estimate), digits = 3),
        `p-value` = ifelse(
            c(pnorm(z, mean = 0, sd =1, lower.tail = FALSE), as.integer(temp3$p.value)) < 0.0001,
          "<0.0001",
          round(
            c(pnorm(z, mean = 0, sd =1, lower.tail = FALSE), as.integer(temp3$p.value)),
            digits = 4
          )
        )
      )
    },
    caption = "Null Hypothesis Test Results",
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
  observeEvent(input$simulate, {
    updateButton(
      session = session,
      inputId = "pickPlayer",
      disabled = TRUE
    )

    updateButton(
      session = session,
      inputId = "simulate",
      icon = icon("retweet"),
      label = "Re-simulate/Switch Players"
    )
  })
}

# Boast app call ----
boastUtils::boastApp(ui = ui, server = server)
