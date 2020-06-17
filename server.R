library(ggplot2)
library(reshape2)
library(magrittr)
library(dplyr)
library(shinyBS)
library(shiny)
library(shinyjs)
library(latex2exp)
library(boastUtils)
library(DT)

playerdata <- read.csv(file = "NBA1819.csv", header = TRUE)

#  "server file must return a server function", create server:
function(input, output, session) {

  # Setup locker configuration
  config <- list(
    base_url = "https://learning-locker.stat.vmhost.psu.edu/",
    auth = "Basic YWVlMjQ2ZDJmMzk2OWMwYTk0NTY3ZTQ0ZThiMDU3NDI3MjhhNWFiYjpmYWU4NDkwNTVlMzNiMDEyNzY0OGIyOGI5YzliZjI2NjMyYzFhYzJk",
    agent = rlocker::createAgent()
  )

  # Initialize Learning Locker connection
  connection <- rlocker::connect(session, config)
  
  .generateStatement <- function(session, verb = NA, object = NA, description = NA, value = NA) {
    if(is.na(object)){
      object <- paste0("#shiny-tab-", session$input$tabs)
    } else {
      object <- paste0("#", object)
    }
    
    stmt <- list(
      verb =  verb,
      object = list(
        id = paste0(boastUtils::getCurrentAddress(session), object),
        name = paste0(APP_TITLE),
        description = description
      )
    )
    
    if(!is.na(value)){
      stmt$result <- list(
        response = value
      ) 
    }
    
    statement <- rlocker::createStatement(stmt)
    response <- rlocker::store(session, statement)
    
    return(response)   
  }
  
  dataFilter <- reactive({
    games <- input$gamesplayed
    gameindex <- which(((playerdata$G >= games[1]) * (playerdata$G <= games[2])) == 1)

    fta <- input$FTA1
    index <- which(((playerdata$FTA >= fta[1]) * (playerdata$FTA <= fta[2])) == 1)
    playerdata <- playerdata[index, ]
    bballdata <- playerdata[gameindex, ]
  })
  
  observeEvent(input$filtertype, {
    .generateStatement(session, object = "filtertype", verb = "interacted", description = "Changed histogram filter.", value = input$filtertype)
  }, ignoreInit = TRUE)

  observeEvent(input$gamesplayed, {
    .generateStatement(session, object = "gamesplayed", verb = "interacted", description = "Filtering data on GamesPlayed.", value = paste(input$gamesplayed, collapse = ", "))
  }, ignoreInit = TRUE)
  
  observeEvent(input$FTA1, {
    .generateStatement(session, object = "FTA1", verb = "interacted", description = "Filtering data on FreeThrowAttempts.", value = paste(input$FTA1, collapse = ", "))
  }, ignoreInit = TRUE)
  
  observeEvent(input$go1, {
    updateTabItems(session, "tabs", "Explore")
  })

  observeEvent(input$go2, {
    updateTabItems(session, "tabs", "Challenge")
  })
  
  observeEvent(input$howToChooseNBA, {
    .generateStatement(session, object = "howToChooseNBA", verb = "interacted", description = "Would you like to select a random player, or a player of your choice?", value = input$howToChooseNBA)
  }, ignoreInit = TRUE)
  
  observeEvent(input$player, {
    .generateStatement(session, object = "player", verb = "interacted", description = "Select your player from the drop down list below:", value = input$player)
  }, ignoreInit = TRUE)
  
  observeEvent(input$rand, {
    .generateStatement(session, object = "rand", verb = "interacted", description = "Selection made.", value = paste(input$howToChooseNBA, input$player, collapse = ", "))
  }, ignoreInit = TRUE)
  
  observeEvent(input[['null.valNBA']], {
    .generateStatement(session, object = "null.valNBA", verb = "interacted", description = "Select a value for the null hypothesis.", value = input[['null.valNBA']])
  }, ignoreInit = TRUE)
  
  observeEvent(input[['samp.sizeNBA']], {
    .generateStatement(session, object = "samp.sizeNBA", verb = "interacted", description = "Input the number of shots in the sample:", value = input[['samp.sizeNBA']])
  }, ignoreInit = TRUE)

  observeEvent(input$resample, {
    .generateStatement(session, object = "resample", verb = "interacted", description = "Selection made.")
  }, ignoreInit = TRUE)

  observeEvent(input$iftestNBA, {
    .generateStatement(session, object = "iftestNBA", verb = "interacted", description = "Show Hypothesis Test Output", value = input$iftestNBA)
  }, ignoreInit = TRUE)
  
  observeEvent(input$significancebounds, {
    .generateStatement(session, object = "significancebounds", verb = "interacted", description = "Plot significance bounds", value = input$significancebounds)
  }, ignoreInit = TRUE)
  
  observeEvent(input$tabs, {
    .generateStatement(session, verb = "experienced", description = paste0("Navigated to ", input$tabs, " tab."))
  }, ignoreInit = TRUE)

  player.select <- reactive({
    # Filter the player data so that it does not choose a player who has no free throw attempts => no free throw %
    # will be used in first app
    index1 <- which(((playerdata$FTA >= 1) * (playerdata$FTA <= 1000)) == 1)
    playerdata2 <- playerdata[index1, ]

    # Randomly select a player if it is random
    decision <- input$howToChooseNBA
    if (decision == "rand") {
      s1 <- playerdata2[sample(nrow(playerdata2), 1), ]
      name <- s1$Player
    }
    else {
      name <- input$player
    }

    # Random Button
    input$rand

    # If it is not random use the player that the user selected
    index <- which(playerdata2$Player == name)
    namedata <- playerdata2[index, ]
  })

  player.select2 <- reactive({
    # Filter the player data so that it only chooses players who played more than half of the games
    # will be used in second app
    playerdata2 <- playerdata %>% filter(G >= max(G) / 2)

    # Randomly select a player if it is random
    decision <- input$howToChooseNBA
    if (decision == "rand") {
      s1 <- playerdata2[sample(nrow(playerdata2), 1), ]
      name <- s1$Player
    }
    else {
      name <- input$player
    }

    # Random Button
    input$rand

    # If it is not random use the player that the user selected
    index <- which(playerdata2$Player == name)
    namedata <- playerdata2[index, ]
  })


  # This is a reactive element for how many shots will be simulated
  nNBA <- reactive({
    return(input$samp.sizeNBA)
  })

  #### This is a reactive element for what the user chooses for the null value####
  hNBA <- reactive({
    return(input$null.valNBA)
  })

  truepropNBA <- reactive({
    return(input$trueNBA)
  })


  ######
  # Output text for what the free throw percentage is for the player
  output$text1NBA <- renderUI({
    namedata <- player.select2()
    ftp <- namedata$FT / namedata$FTA

    p <- "p"
    withMathJax(h4(sprintf(
      "The true free throw proportion for %s is %f.", namedata$Player, round(ftp, 2)
    )))
  })

  # Output text for what the sampled free throw percentage is for the player
  output$text2NBA <- renderUI({
    namedata <- player.select2()
    phat <- phat()

    withMathJax(h4(sprintf(
      "The sampled free throw proportion ( \\(\\hat{p}\\) ) for %s is %f.", namedata$Player, round(phat, 2)
    )))
  })

  # Output text for the null hypothesis
  output$text3NBA <- renderText({
    namedata <- player.select2()
    h1 <- hNBA()
    paste("Test the hypothesis that the free throw percentage for ", namedata$Player, "is equal to", h1, "against a two-sided alternative.")
  })

  #### Output plot, the histogram for "filtering", part 1####
  output$histogramNBA <- renderPlot({
    validate(
      need(input$gamesplayed > 0,
        message = "Please input a valid number of games played"
      )
    )

    bballdata <- dataFilter()
    n <- nrow(bballdata)
    y <- numeric(length = n)

    # Calculates the free throw percentages for all the players and puts it into a variable
    # Produces NAN's for players that haven't taken any free throws
    # Doesn't matter for the Histogram though because the Histogram won't display the NaN's
    # I take out the NaN's in a different part where it is needed
    for (i in 1:n) {
      y[i] <- bballdata$FT[i] / bballdata$FTA[i]
    }

    # The actual histogram
    par(bg = "white")
    hist(y, xlab = "Free Throw Proportion", main = "Histogram of Free Throw Proportion", col = "firebrick")
  })


  #### make phat ####
  phat <- eventReactive(
    {
      input$resample
      input$samp.sizeNBA
    },
    {
      h0 <- hNBA()
      namedata <- player.select2()
      ftp <- namedata$FT / namedata$FTA
      n1 <- nNBA()
      true1 <- truepropNBA()
      phat <- 0
      sim1 <- rbinom(n = n1, size = 1, prob = ftp)

      for (i in 1:n1) {
        if (sim1[i] == 1) {
          phat <- phat + 1
        }
        else {
          phat <- phat
        }
      }
      phat <- phat / n1
    }
  )

  #### making event reactive to create non changing samp dist####
  temp2 <- eventReactive(input$rand, {
    h0 <- hNBA()
    namedata <- player.select2()
    ftp <- namedata$FT / namedata$FTA
    n1 <- nNBA()
    true1 <- truepropNBA()

    # sampling distribuion
    phat <- 0
    phats <- c()
    j <- 1
    for (j in 1:2000) {
      i <- 1
      sim1 <- rbinom(n = 40, size = 1, prob = ftp)
      phat <- 0
      for (i in 1:40) {
        if (sim1[i] == 1) {
          phat <- phat + 1
        }
        else {
          phat <- phat
        }
        i <- i + 1
      }
      phat <- phat / 40
      phats[j] <- phat
      j <- j + 1
    }
    # phats = rnorm(n=500, mean=ftp, sd=sqrt(ftp*(1-ftp)))

    data.frame(length = phats)
  })

  #### make conditional for significance bounds####
  # true2 <- eventReactive(input$resample, {
  #
  # })

  #### output plot of the plot in part 2, "Three proportions"####
  output$proportion2NBA <- renderPlot({
    validate(
      need(input$resample,
        message = 'Please finish choosing options, and click the "Submit" button.'
      )
    )

    # input$resample
    h0 <- hNBA()
    namedata <- player.select2()
    ftp <- namedata$FT / namedata$FTA
    n1 <- nNBA()
    true1 <- truepropNBA()

    phat <- phat()
    options(digits = 6)
    dat <- round(playerdata$FT / playerdata$FTA, 6)
    dat <- as.numeric(na.omit(dat))

    stanerr1 <- sqrt(h0 * (1 - h0) / n1)
    z1 <- (phat - h0) / stanerr1
    z1 <- round(z1, digits = 3)

    # lower=ifelse(z1<0,max(dat[which(dat<(z1*stanerr1+h0))]),max(dat[which(dat<(h0-z1*stanerr1))]))
    # upper=ifelse(z1>0,min(dat[which(dat>(z1*stanerr1+h0))]),min(dat[which(dat>(h0-z1*stanerr1))]))
    if (input$significancebounds) {
      lower <- max(dat[which(dat < (h0 - 1.96 * stanerr1))])
      upper <- min(dat[which(dat > (h0 + 1.96 * stanerr1))])
      upper <- ifelse(upper == 1, .98, upper)
      lower <- ifelse(lower <= .25, .251, lower)
    }

    data <- melt(data.frame(
      p_hat = c(phat), p0 = c(h0),
      hypothesis = c("%made")
    ),
    variable_name = "p"
    )

    g <- ggplot(data, aes(x = hypothesis, y = value, fill = variable)) + geom_bar(position = "dodge", stat = "identity") + ylab("Proportion")
    g <- g + theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold")
    )
    g <- g + if (input$significancebounds) {
      geom_hline(aes(yintercept = lower), color = "black")
    } else {
      NULL
    }
    g <- g + if (input$significancebounds) {
      geom_hline(aes(yintercept = upper), color = "black")
    } else {
      NULL
    }
    g + scale_fill_discrete(
      name = "variables",
      breaks = c("p_hat", "p0"),
      labels = unname(TeX(c("\\hat{p}", "p_0")))
    )
  })


  #### hypothesis test output in part 2####
  output$testtableNBA <- renderTable({
    validate(
      need(input$samp.sizeNBA > 0,
        message = "Please input a valid number of shots"
      )
    )

    namedata <- player.select2()
    h1 <- hNBA()
    ftp <- namedata$FT / namedata$FTA
    n4 <- nNBA()
    phat <- phat()


    stanerr1 <- sqrt(h1 * (1 - h1) / n4)
    z1 <- (phat - h1) / stanerr1
    z1 <- round(z1, digits = 3)
    # paste(round(z1,digits = 3))
    # dat <- round(playerdata$FT / playerdata$FTA, 6)
    # dat <- as.numeric(na.omit(dat))

    # sd(dat) is # .141861

    if (phat > h1) {
      p1 <- pnorm(z1, lower.tail = FALSE) * 2
      # p1 = (sum(dat>min(dat[which((dat-h1)/.141861*sqrt(n4)>z1)]))/438) # one sided probability because the distribution is not symmetric (?) #this isnt right tho im pretty sure
      # p1 = sum(dat>min(dat[which(dat>phat)]))/438
      # actually, these are wrong because they arent affected by null changing
    } else {
      p1 <- pnorm(z1, lower.tail = TRUE) * 2
      # p1 = (sum(dat<max(dat[which((dat-h1)/.141861*sqrt(n4)<z1)]))/438) # one sided probability because the distribution is not symmetric (?)
      # p1 = sum(dat<max(dat[which(dat<phat)]))/438
      # these are wrong because they arent affected by null changing
    }

    if (input$iftestNBA) {
      ctable <- matrix(c(z1, p1), nrow = 1)
      colnames(ctable) <- c("z-statistic", "p-value")
      ctable
    }
  })

  # exact hypothesis test using binomial
  output$exactTesttableNBA <- renderTable({
    validate(
      need(input$samp.sizeNBA > 0,
        message = "Please input a valid number of shots"
      )
    )

    namedata <- player.select2()
    h1 <- hNBA()
    ftp <- namedata$FT / namedata$FTA
    n4 <- nNBA()
    phat <- phat()

    p <- binom.test(phat * n4, n4, h1)$p.value
    p <- round(p, digits = 4)
    if (input$iftestNBA) {
      ctable <- matrix(c(phat, p), nrow = 1)
      colnames(ctable) <- c("p-hat", "p-value")
      ctable
    }
  })

    
    playerData <- playerdata[,c("Player", "G", "FT", "FTA")]
  
    ## Use Short but Meaningful Column Names
    names(playerData) <- c("Player", "# of Game", "Free Throw", "Free throw Attempted")
    output$mtPlayer <- DT::renderDT(
      expr = playerData,
      caption = "NBA Player Data, 2018-2019 Models", # Add a caption to your table
      style = "bootstrap4", 
      rownames = TRUE,
      options = list( # You must use these options
        responsive = TRUE, # allows the data table to be mobile friendly
        scrollX = TRUE, # allows the user to scroll through a wide table
        columnDefs = list(  # These will set alignment of data values
          # Notice the use of ncol on your data frame; leave the 1 as is.
          list(className = 'dt-center', targets = 1:ncol(playerData))
        )
      )
    )
}
