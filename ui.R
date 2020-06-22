library(shiny)
library(shinyBS)
library(DT)
library(boastUtils)

## App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Title of the app"
APP_DESCP  <<- paste(
  "In this app the goal is to learn about the reasoning of
  a hypothesis test about proportions."
)
## End App Meta Data------------------------------------------------------------

#Data Wrangling
playerdata <- read.csv(file = "NBA1819.csv", header = TRUE)
# Filter the player data so that it does not choose a player who has no free throw attempts => no free throw %
index1 <- which(((playerdata$FTA >= 1) * (playerdata$FTA <= 1000)) == 1)
playerdata2 <- playerdata[index1, ]
# create a list of just the players names to be selected from later
PlayerNames <- playerdata2[, 1]

#  "ui file must return UI object", perform ui construction:
dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Hypothesis Testing", titleWidth = 250,
                  tags$li(class = "dropdown", actionLink("info", icon("info"))),
                  tags$li(class = "dropdown",
                          tags$a(href='https://shinyapps.science.psu.edu/',
                                 icon("home")))
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
      menuItem("Explore", tabName = "Explore", icon = icon("wpexplorer")),
      menuItem("Challenge", tabName = "Challenge", icon = icon("cogs")),
      menuItem("Data", tabName = "Data", icon = icon("table")),
      menuItem("References", tabName = "References", icon = icon("leanpub"))
    ),
    tags$div(
      class = "sidebar-logo",
      boastUtils::psu_eberly_logo("reversed")
    )
  ),
  
  # Content within the tabs
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css",
                href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
    ),
    # This is contents for the first tab, "Overview"
    tabItems(
      tabItem(
        tabName = "Overview",
        withMathJax(),
        h1("Hypothesis Test for Means"),
        p("In this app the goal is to learn about the reasoning of a hypothesis test about proportions."),
        h2("Instructions"),
        tags$li("In Part 1 you will look at how the population distribution of all the players' free throw
                percentages is affected by filtering (restricting attention to a subpopulation)."),
        div(
          style = "text-align: center",
          bsButton(
            input = "go1",
            label = "Explore", 
            size = "large",
            icon = icon("bolt")
          )
        ),
        tags$li("In Part 2 you will explore hypothesis tests about an individual players' free throw percentages."),
        div(
          style = "text-align: center;",
          bsButton(
            input = "go2",
            label = "Challenge", 
            size = "large",
            icon = icon("bolt")
          )
        ),
        br(),
        br(),
        h2("Acknowledgements"),
        p("This app was developed and programmed in 2017 by David Robinson. The hypothesis testing features
          in part 2 were edited and improved with additional programming in 2018 by Ryan Voyack. The updated
          version of this app was improved in 2020 by Xuefei Wang."),
        br(),
        img(src = "fthrow2.png", height = 250, width = 650, algin = "middle"),
        br(),
        br(),
        div(class = "updated", "Last Update: 5/13/2020 by XW.")
      ),
      
      # This is content for the second tab, "Explore"
      tabItem(
        tabName = "Explore",
        withMathJax(),
        h1("Histogram"),
        fluidRow(
          # Include LaTeX functioality although I don't think I used it for this
          withMathJax(),
          
          # Column 1 has inputs for how to filter and is of width 4
          column(
            4,
            
            selectInput("filtertype", 
                        "Select how you would like to filter.", 
                        choices = c(GamesPlayed = "games", FreeThrowAttempts = "FTA")),
            
            conditionalPanel(
              "input.filtertype == 'games'",
              sliderInput(
                inputId = "gamesplayed",
                "Filter on number of games played:",
                min = 0,
                max = 82,
                step = 5,
                value = c(0, 82)
              )
            ),
            conditionalPanel(
              "input.filtertype == 'FTA'",
              sliderInput(
                inputId = "FTA1",
                "Filter on number of free throws attempted:",
                min = 0,
                max = 881,
                step = 5,
                value = c(0, 881)
              )
            ),
            img(src = "Giannis.png", height = 219, width = 300, align = "middle")
          ),
          
          # Column two displays the Histogram of the distrubition of the free throw attempts
          column(
            8,
            
            plotOutput("histogramNBA"),
            # Add rollover for Histogram of Free Throw Proportion Plot
            bsPopover(id = "histogramNBA", title = " ", content = "This histogram filters the NBA players based on games played or free throw attempts. Changing the slider will adjust the number of players that fit the selected criteria.", placement = "left", trigger = "hover", options = NULL)
          ),
          
          # A paragraph with information to get students thinking and transitioning into part 2
          p("Try to think about what the median and mean of FT% are and what range you might expect most of the players to fall in.")
        )
      ),
      
      
      # This is content for the third tab, "Challenge"
      tabItem(
        tabName = "Challenge",
        withMathJax(),
        fluidRow(
          column(
            12,
            h4("Here, we will create a sample 'p-hat' for any player's free throw percentage and test it against a particular null hypothesis. By default, we test it against the NBA average percentage of 74%. Also, we will only be selecting from all NBA players that played no less than half of the games their teams played in during the 2018-2019 season.")
          )
        ),
        fluidRow(
          # This is a text output that displays what the hypothesis is they are testing and who the player is
          column(
            4,
            # Conditional based on how the user would like to select a player for the hypothesis test
            selectInput(inputId = "howToChooseNBA", "Would you like to select a random player, or a player of your choice?", choices = c(Random = "rand", Select = "sel")),
            conditionalPanel(
              "input.howToChooseNBA == 'sel'",
              selectizeInput(inputId = "player", "Select your player from the drop down list below:", choices = PlayerNames, multiple = FALSE, options = list(placeholder = "Select Your Player"), selected = NULL)
            ),
            
            # Random button
            actionButton(inputId = "rand", label = "Choose"),
            
            
            # after the user selects generate, we pull up option to choose null and sample size
            conditionalPanel(
              condition = "input.rand",
              
              
              # The H0 value the user would like to test against
              numericInput("null.valNBA", "Select a value for the null hypothesis. ", min = 0, max = 1, value = 0.74, step = 0.01),
              textOutput("text3NBA"),
              
              tags$head(tags$style("#text3NBA{color: black;font-style: bold;}")),
              br(),
              
              ### User now selects what their sample size would be ie how many shots they are simulating for the player
              # simulates shots based on the players actual FT%
              # h4("Simulate your player shooting free throws and guess whether or not we can reject the null hypothesis"),
              sliderInput("samp.sizeNBA", ("Input the number of shots in the sample:  "), min = 5, max = 60, value = 30, step = 8),
              
              actionButton(inputId = "resample", label = "Submit"),
              
              conditionalPanel(
                condition = "input.resample",
                checkboxInput("iftestNBA", h5("Show Hypothesis Test Output")),
                checkboxInput("significancebounds", h5("Plot significance bounds"))
              )
              
            )
            
            
            # include an image
            # img(src = "fthrow.png", height = 150, width =100)
          ),
          
          column(
            8,
            
            p("CHALLENGE: Simulate your player shooting free throws and determine whether or not we can reject the null hypothesis based on the plot."),
            p("CHALLENGE: Does increasing the sample size make it easier or harder to get a significantly low p-value?"),
            
            plotOutput("proportion2NBA"),
            bsPopover(id = "proportion2NBA", title = " ", content = "This bar plot shows us the sampled proportion and the hypothesized proportion that are being tested in our hypothesis test.", placement = "left", trigger = "hover", options = NULL),
            # Output some info about the graphs and the conditional part
            # h4("The red line shows the proportion from the null hypothesis"),
            # h4("The purple line shows the sample proportion"),
            # conditionalPanel("input.true==true",
            #                  h4("The blue line shows the players actual free throw proportion from the 2018-19 season")
            # )
            conditionalPanel(
              "input.resample",
              uiOutput("text1NBA"),
              uiOutput("text2NBA")
            ),
            conditionalPanel(
              "input.iftestNBA==true",
              h4("Normal approximation hypothesis test"),
              tableOutput("testtableNBA"),
              h4("Exact hypothesis test"),
              tableOutput("exactTesttableNBA")
            )
            
            # ),
            # column(8,
            #       conditionalPanel("input.iftestNBA==true",
            #                        tableOutput("exactTesttableNBA")
            #       )
          )
        )
      ),
      
      
      # This is content for the fourth tab, "Data"
      tabItem(
        tabName = "Data",
        withMathJax(),
        DT::DTOutput(outputId = "mtPlayer")
      ),
      
      # This is content for the fifth tab, "References"
      tabItem(
        tabName = "References",
        withMathJax(),
        h2("References"),
        p(
          class = "hangingindent",
          "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
        ),
        p(
          class = "hangingindent",
          "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0).
            [R Package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
        ),
        p(
          class = "hangingindent",
          "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
        ),
        p(
          class = "hangingindent",
          "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019). shiny: Web application framework for R. (v1.4.0)
            [R Package]. Available from https://CRAN.R-project.org/package=shiny"
        ),
        p(
          class = "hangingindent",
          "Hanisch, J. (2016). USA Today Sports"
        ),
        p(
          class = "hangingindent",
          "Hoop Hounds. Free Throw SHooting Tips. Retrieved from http://www.hoophounds.com/articles/free-throw-shooting-tips/"
        ),
        p(
          class = "hangingindent",
          "NBA (2019), NBA Advanced Stats, [It contains information about performances of NBA players in 2018-2019].
            Available at https://stats.nba.com/leaders/?Season=2018-19&SeasonType=Regular%20Season&PerMode=Totals"
        ),
        p(
          class = "hangingindent",
          "Xie, Y., Cheng, J., Tan, X. (2020).DT: A Wrapper of the JavaScript Library 'DataTables'.(v0.13) 
          [R Package]. Available from https://CRAN.R-project.org/package=DT"
        ),
        p(
          class = "hangingindent",
          "Wickham, W. (2016). ggplot2: Elegant graphics for data analysis.
            [R Package]. Springer-Verlag New York. Available from
            https://ggplot2.tidyverse.org"
        )
      )
      
      
    )
  )
)

