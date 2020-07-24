library(shiny)
library(shinyBS)
library(dplyr)
library(shinyWidgets)
library(boastUtils)
library(shinydashboard)

## App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Hypothesis Testing"
APP_DESCP  <<- paste(
  "In this app the goal is to learn about the reasoning of
  a hypothesis test about proportions."
)
## End App Meta Data------------------------------------------------------------

#  "ui file must return UI object", perform ui construction:
dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "Hypothesis Testing", 
    titleWidth = 250,
    tags$li(
      class = "dropdown",
      actionLink(inputId = "info", label = icon("info"), class = "myClass")),
    tags$li(class = "dropdown",
            tags$a(href='https://shinyapps.science.psu.edu/',
                  icon("home")))
  ),
  
  ## Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
      menuItem("Explore", tabName = "Explore", icon = icon("wpexplorer")),
      menuItem("Challenge", tabName = "Challenge", icon = icon("cogs")),
      menuItem("References", tabName = "References", icon = icon("leanpub"))
    ),
    tags$div(
      class = "sidebar-logo",
      boastUtils::psu_eberly_logo("reversed")
    )
  ),
  
  ## Content within the tabs
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css",
        href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css"
                )
    ),
    ### This is contents for the first tab, "Overview"
    tabItems(
      tabItem(
          tabName = "Overview",
          withMathJax(),
          h1("Hypothesis Test for Means"),
          p("The goal of this app is to understand the reasoning of a hypothesis
            test about proportions."),
          h2("Instructions"),
          tags$ul(
          tags$li("In Part 1 you will look at how the population distribution of 
                  all the players' free throw percentages is affected by filtering
                  (restricting attention to a subpopulation)."),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "Explore", 
              size = "large",
              icon = icon("bolt")
              )
        ),
          tags$li("In Part 2 you will explore hypothesis tests about an individual
                  players' free throw percentages."),
        div(
          style = "text-align: center;",
          bsButton(
            inputId  = "go2",
            label = "Challenge", 
            size = "large",
            icon = icon("bolt")
          )
        )),
        br(),
        br(),
        h2("Acknowledgements"),
        p("This app was developed and programmed in 2017 by David Robinson. The 
          hypothesis testing features in part 2 were edited and improved with 
          additional programming in 2018 by Ryan Voyack. The updated version of 
          this app was improved in 2020 by Xuefei Wang."),
        br(),
        div(
          style = "text-align: center;",
          img(
            src = "fthrow2.png", 
            alt = "This picture shows threee famous basketball players that are
                  doing free throw.",
            height = "20%",
            width = "40%")),
        br(),
        br(),
        br(),
        div(class = "updated", "Last Update: 7/14/2020 by XW.")
      ),
      
      ### This is content for the second tab, "Explore"
      tabItem(
        tabName = "Explore",
        withMathJax(),
        h2("Filtered Populations"),
        p("Please select filters that you would like to investigate. Then, try 
          to change the slider to adjust the number of players that fit the 
          selected criteria."),
        fluidRow(
          column(
            4,
            h3("Filters"),
            wellPanel(
              selectInput(
                inputId ="filtertype", 
                label = "Select how you would like to filter.", 
                choices = c("Games Played" = "games",
                            "Free Throw Attempts" = "FTA")),
            
              conditionalPanel("input.filtertype == 'games'",
                sliderInput(
                  inputId = "gamesplayed",
                  label = "Filter on number of games played:",
                  min = 1,
                  max = 82,
                  value = c(1, 82)
                )
              ),
              conditionalPanel("input.filtertype == 'FTA'",
                sliderInput(
                  inputId = "FTA1",
                  label = "Filter on number of free throws attempted:",
                  min = 1,
                  max = 719,
                  value = c(1, 719)
                )
              )
            )  
          ),
          
          #### Column two displays the Histogram of the distrubition of the
          #### free throw attempts
          column(
            8,
            plotOutput("histogramNBA"),
            tags$script(HTML(
              "$(document).ready(function() {
              document.getElementById('histogramNBA').setAttribute('aria-label',
              `Histogram of free thow proportion`)
              })"
            )),
            ##### Add rollover for Histogram of Free Throw Proportion Plot
            bsPopover(
             id = "histogramNBA",
             title = "",
             content = "Try to think about what the median and mean of free throw 
                        percentage are and what range you might expect most of 
                        the players to fall in.",
             placement = "bottom",
             trigger = "hover", 
             options = NULL)
            )
        )
      ),
      
      
      ### This is content for the third tab, "Challenge"
      tabItem(
        tabName = "Challenge",
        withMathJax(),
        h2("Sample Proportion vs. True Proportion"),
          p("In this activity, you'll test any player's free throw percentage 
            against a null hypothesis (provided the player played in at least half
            of all games during the 2018-2019 season). You will use the player's
            overall free throw percentage to generate samples and calculate the 
            sample proportion, \\(\\hat{p}\\). The default null hypothesis is 
            \\(p_0=0.74\\); however, you can change this through a slider.")
,
        fluidRow(
          #### This is a text output that displays what the hypothesis is they 
          #### are testing and who the player is
          column(
            width = 4,
            ##### Conditional based on how the user would like to select a player
            ##### for the hypothesis test
            wellPanel(
              sliderInput(
                inputId = "percentage",
                label = "Filter the percentage of game played",
                min = 50,
                max = 100,
                value = 80,
                post = "%"
              ),
              selectInput(
                inputId = "howToChooseNBA", 
                label = "Would you like to select a random player,
                        or a player of your choice?", 
                choices = c(Random = "rand", Select = "sel")),
              conditionalPanel("input.howToChooseNBA == 'sel'",
                uiOutput('playernames')
              ),
            
              ##### Random button
              actionButton(inputId = "rand", label = "Choose"),
              ##### after the user selects generate, we pull up option to choose
              ##### null and sample size
              conditionalPanel(condition = "input.rand",
                             
                ###### The H0 value the user would like to test against
                sliderInput(
                  inputId = "null.valNBA", 
                  label = "Select a value for the null hypothesis.", 
                  min = 0, 
                  max = 1,
                  value = 0.74),
                textOutput("text3NBA"),
                br(),
              
                sliderInput(
                  inputId = "samp.sizeNBA",
                  label = "Input the number of shots in the sample:",
                  min = 5,
                  max = 60, 
                  value = 30),
              
                actionButton(inputId = "resample", label = "Submit"),
              
                conditionalPanel(
                  condition = "input.resample",
                  checkboxInput(
                    inputId = "iftestNBA", 
                    label = "Show Hypothesis Test Output"),
                  checkboxInput(
                    inputId = "significancebounds",
                    label = "Plot significance bounds")
              )
            )
            ) 
          ),
          
          column(
            width = 8,
            fluidRow(
              plotOutput("proportion2NBA"),
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('proportion2NBA').setAttribute('aria-label',
                `This bar plot shows proportions of sampled and hypothesized`)
              })"
              )),
              bsPopover(
                id = "proportion2NBA", 
                title = " ", 
                content = "This bar plot shows us the sampled proportion and the 
                          hypothesized proportion that are being tested in our
                          hypothesis test.",
                placement = "left", 
                trigger = "hover",
                options = NULL)),
            
            p("CHALLENGE: Simulate your player shooting free throws and determine
              whether or not we can reject the null hypothesis based on the plot."),
            p("CHALLENGE: Does increasing the sample size make it easier or harder 
              to get a significantly low p-value?"),
            
            conditionalPanel(
              "input.resample",
              uiOutput("text1NBA"),
              uiOutput("text2NBA")
            ),
            conditionalPanel(
              "input.iftestNBA==true",
              h3("Normal Approximation Hypothesis Test"),
              tableOutput("testtableNBA"),
              h3("Exact [Binomial] Hypothesis Test"),
              tableOutput("exactTesttableNBA")
            )
          )
        )
      ),
      
  
      
      # This is content for the fourth tab, "References"
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
          "Carey, R. (2019). boastUtils: BOAST Utilities, R Package.
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
          citahttps://stats.nba.com/leaders/?Season=2018-19&SeasonType=Regular%
          20Season&PerMode=Totals"
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
        ) 
      )
      
    )
  )
)

