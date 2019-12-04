library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)

playerdata <- read.csv("NBA1617E.csv", header = TRUE)

# Filter the player data so that it does not choose a player who has no free throw attempts => no free throw %
index1 <- which(((playerdata$FTA >= 1) * (playerdata$FTA <= 1000)) == 1)
playerdata2 <- playerdata[index1, ]

# create a list of just the players names to be selected from later
PlayerNames <- playerdata2[, 1]

#  "ui file must return UI object", perform ui construction:
dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Hypothesis Testing with NBA data", titleWidth = 300),

  # Sidebar
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Filtering", tabName = "filtering", icon = icon("wpexplorer")),
      menuItem("Hypothesis Testing", tabName = "hypothesis_testing", icon = icon("cogs")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    )
  ),

  # Content within the tabs
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
    ),
    tabItems(
      tabItem(
        tabName = "overview",
        tags$a(href = "http://stat.psu.edu/", tags$img(src = "PS-HOR-RGB-2C.png", align = "left", width = 180)),
        br(), br(), br(),

        h3(strong("About:")),
        h4("In this app the goal is to learn about the reasoning of a hypothesis test about proportions."),
        h4("This app uses 2016-2017 data for the NBA regular season."),
        br(),
        h3(strong("Instructions:")),
        h4(tags$li("In Part 1 you will look at how the population distribution of all the players' free throw percentages is affected by filtering (restricting attention to a subpopulation).")),
        tags$head(
          tags$style(HTML("#go{background-color: #367fa9}"))
        ),
        div(
          style = "text-align: center;",
          bsButton("go1", "Go to filtering exploration", icon = icon("bolt"))
        ),
        h4(tags$li("In Part 2 you will explore hypothesis tests about an individual players' free throw percentages.")),
        tags$head(
          tags$style(HTML("#go{background-color: #367fa9}"))
        ),
        div(
          style = "text-align: center;",
          bsButton("go2", "Go to hypothesis tester", icon = icon("bolt"))
        ),
        br(),
        h3(strong("Acknowledgements:")),
        h4("This app was developed and programmed in 2017 by David Robinson. The hypothesis testing features in part 2 were edited and improved with additional programming in 2018 by Ryan Voyack."),

        br(),
        br(),
        img(src = "fthrow2.png", height = 250, width = 650, algin = "middle")
      ),

      # Define the content contained within part 1 ie. tabname "filtering"
      tabItem(
        tabName = "filtering",
        div(
          style = "display: inline-block;vertical-align:top;",
          tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 15))
        ),
        fluidRow(
          # Include LaTeX functioality although I don't think I used it for this
          withMathJax(),

          # Column 1 has inputs for how to filter and is of width 4
          column(
            4,

            selectInput("filtertype", h2("Select how you would like to filter."), choices = c(GamesPlayed = "games", FreeThrowAttempts = "FTA")),

            conditionalPanel(
              "input.filtertype == 'games'",
              sliderInput(
                inputId = "gamesplayed",
                "Filter on number of games played:",
                min = 0,
                max = 82,
                value = c(0, 85)
              )
            ),
            conditionalPanel(
              "input.filtertype == 'FTA'",
              sliderInput(
                inputId = "FTA1",
                "Filter on number of free throws attempted:",
                min = 0,
                max = 881,
                value = c(0, 881)
              )
            ),
            img(src = "Giannis.png", height = 219, width = 300, align = "middle")
          ),

          # Column two displays the Histogram of the distrubition of the free throw attempts
          column(
            8,
            h1("Histogram"),

            plotOutput("histogramNBA"),
            # Add rollover for Histogram of Free Throw Proportion Plot
            bsPopover(id = "histogramNBA", title = " ", content = "This histogram filters the NBA players based on games played or free throw attempts. Changing the slider will adjust the number of players that fit the selected criteria.", placement = "left", trigger = "hover", options = NULL)
          ),

          # A box with information to get students thinking and transitioning into part 2
          box(width = 12, background = "blue", h4("Try to think about what the median and mean of FT% are and what range you might expect most of the players to fall in. "))
        )
      ),


      ###############################
      #### Define Content in tab 2####
      #
      tabItem(
        tabName = "hypothesis_testing",
        div(
          style = "display: inline-block;vertical-align:top;",
          tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 15))
        ),
        fluidRow(
          column(
            12,
            h4("Here, we will create a sample 'p-hat' for any player's free throw percentage and test it against a particular null hypothesis. By default, we test it against the NBA average percentage of 74%. Also, we will only be selecting from all NBA players that played no less than half of the games their teams played in during the 2016-2017 season.")
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
              sliderInput("samp.sizeNBA", ("Input the number of shots in the sample:  "), min = 5, max = 60, value = 30, step = 1),

              actionButton(inputId = "resample", label = "Sample!"),

              conditionalPanel(
                condition = "input.resample",
                checkboxInput("iftestNBA", h5("Show Hypothesis Test Output")),
                checkboxInput("significancebounds", h5("Plot significance bounds"))
              )

              # Conditional using checkbox if they want to see what the true population proportion is for their player
              # checkboxInput("trueNBA", h6("Plot the true free throw percentage")),
              # conditionalPanel("input.trueNBA==true",
              #                textOutput("text1NBA")
              # ),
            )


            # include an image
            # img(src = "fthrow.png", height = 150, width =100)
          ),

          column(
            8,

            h4("CHALLENGE: Simulate your player shooting free throws and determine whether or not we can reject the null hypothesis based on the plot."),
            h4("CHALLENGE: Does increasing the sample size make it easier or harder to get a significantly low p-value?"),

            plotOutput("proportion2NBA"),
            bsPopover(id = "proportion2NBA", title = " ", content = "This bar plot shows us the sampled proportion and the hypothesized proportion that are being tested in our hypothesis test.", placement = "left", trigger = "hover", options = NULL),
            # Output some info about the graphs and the conditional part
            # h4("The red line shows the proportion from the null hypothesis"),
            # h4("The purple line shows the sample proportion"),
            # conditionalPanel("input.true==true",
            #                  h4("The blue line shows the players actual free throw proportion from the 2016-17 season")
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


      # The fourth tab just shows all of the data that I used
      tabItem(
        tabName = "data",
        div(
          style = "display: inline-block;vertical-align:top;",
          tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 15))
        ),
        fluidRow(
          column(
            6,
            h3("Data"),
            tableOutput("samp.table")
          )
        )
      )
    )
  )
)
