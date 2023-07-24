# Copyright statement:
# This shiny application is developed by Lientje Maas to be used for educational purposes.
# Is is part of a program sponsered by the Education Incentive Funds of Utrecht University. 
# The layout for the shiny applications for this program is developed by Kimberley Lek. 
# The application is licensed under the GNU General Public License V3.0 

# Author Comment:
# I have tried to code this according to the Google R Style Guide to improve readability:
# https://google.github.io/styleguide/Rguide.xml
# For any questions or comments you can contact me at j.a.m.maas@uu.nl.

# File description:
# This file contains the server for the application related to sampling distributions.



# Loading libraries 
library(shiny)
library(MASS) 
library(ggplot2)
library(Rmisc)

# Server design
server <- function(input, output, session) {
  
  observe(updateSelectInput(session, "pop.distr2",   selected = input$pop.distr))
  observe(updateSelectInput(session, "pop.distr",    selected = input$pop.distr2))
  observe(updateSelectInput(session, "sample.size2", selected = input$sample.size))
  observe(updateSelectInput(session, "sample.size",  selected = input$sample.size2))
  
  N <- 1000 # population size
  pop <- reactiveValues(
    par1          = NULL,
    par2          = NULL,
    width         = NULL, # binwidth for dotplots
    population    = NULL,
    mu            = NULL,
    sd            = NULL,
    par21_1       = NULL, # tab2 group1 param1
    par21_2       = NULL,
    par22_1       = NULL,
    par22_2       = NULL,
    width21       = NULL, # binwidth for dotplots tab2 group 1
    width22       = NULL,
    population2_1 = NULL,
    population2_2 = NULL,
    poprange2     = NULL,
    mu2_1         = NULL,
    mu2_2         = NULL,
    mu2_diff      = NULL,
    sd2_1         = NULL,
    sd2_2         = NULL
  )
  
  # dynamic input for population distribution parameters tab1
  output$ui <- renderUI({
    
    freezeReactiveValue(input, "pop.pars_1")
    freezeReactiveValue(input, "pop.pars_2")
    
    tagList(
      switch(input$pop.distr,
             "Beta"    = numericInput(inputId = "pop.pars_1", label = "\u03b1", value = 20, min = 0),
             "Uniform" = numericInput(inputId = "pop.pars_1", label = "a",      value = 4),
             "Normal"  = numericInput(inputId = "pop.pars_1", label = "\u03BC", value = 7)
      ),
      switch(input$pop.distr,
             "Beta"    = numericInput(inputId = "pop.pars_2", label = "\u03b2", value = 7, min = 0),
             "Uniform" = numericInput(inputId = "pop.pars_2", label = "b",      value = 13),
             "Normal"  = numericInput(inputId = "pop.pars_2", label = "\u03C3", value = 1, min = 0)
      ))
    
  })
  
  
  # dynamic input for population distributions parameters tab2
  output$ui2_1 <- renderUI({
    
    freezeReactiveValue(input, "pop.pars21_1") #tab2 group1 param1
    freezeReactiveValue(input, "pop.pars21_2")
    
    tagList(
      switch(input$pop.distr2,
             "Beta"    = numericInput(inputId = "pop.pars21_1", label = "\u03b1 group 1", value = 20, min = 0),
             "Uniform" = numericInput(inputId = "pop.pars21_1", label = "a group 1",      value = 4),
             "Normal"  = numericInput(inputId = "pop.pars21_1", label = "\u03BC group 1", value = 7)
      ),
      switch(input$pop.distr2,
             "Beta"    = numericInput(inputId = "pop.pars21_2", label = "\u03b2 group 1", value = 7, min = 0),
             "Uniform" = numericInput(inputId = "pop.pars21_2", label = "b group 1",      value = 13),
             "Normal"  = numericInput(inputId = "pop.pars21_2", label = "\u03C3 group 1", value = 1, min = 0)
      )
    )
    
  })
  
  output$ui2_2 <- renderUI({
    
    freezeReactiveValue(input, "pop.pars22_1") #tab2 group2 param1
    freezeReactiveValue(input, "pop.pars22_2")
    
    tagList(
      switch(input$pop.distr2,
             "Beta"    = numericInput(inputId = "pop.pars22_1", label = "\u03b1 group 2", value = 11, min = 0),
             "Uniform" = numericInput(inputId = "pop.pars22_1", label = "a group 2",      value = 4),
             "Normal"  = numericInput(inputId = "pop.pars22_1", label = "\u03BC group 2", value = 6)
      ),
      switch(input$pop.distr2,
             "Beta"    = numericInput(inputId = "pop.pars22_2", label = "\u03b2 group 2", value = 5, min = 0),
             "Uniform" = numericInput(inputId = "pop.pars22_2", label = "b group 2",      value = 13),
             "Normal"  = numericInput(inputId = "pop.pars22_2", label = "\u03C3 group 2", value = 1, min = 0)
      )
    )
    
    
  })
  
  # if new population is obtained reset sample values
  observeEvent(c(input$genButton, input$pop.distr), {
    sample$means    = NULL 
    sample$sample   = NULL
  })
  
  observeEvent(c(input$genButton2, input$pop.distr2), {
    sample$means2_1    = NULL 
    sample$means2_2    = NULL 
    sample$means2_diff = NULL 
    sample$sample2_1   = NULL
    sample$sample2_2   = NULL
  })
  
  # empty vectors to store samples (indicators) and means in.
  sample <- reactiveValues(
    means       = NULL, 
    sample      = NULL,
    means2_1    = NULL, 
    means2_2    = NULL,
    means2_diff = NULL,
    sample2_1   = NULL,
    sample2_2   = NULL
  )
  
  # function to sample with certain sample size and store sample means
  observeEvent(input$samplebutton,{
    
    # ensure value falls in range of histogram
    toplot <- 10; xlowlim <- xuplim <- 1
    while(!(toplot > xlowlim & toplot < xuplim)){
      
      if(input$sample.size == 10){
        sample$sample     <- sample(x=(1:N), size = 10, replace = F)
      }
      if(input$sample.size == 30){
        sample$sample     <- sample(x=(1:N), size = 30, replace = F)
      }
      if(input$sample.size == 100){
        sample$sample     <- sample(x=(1:N), size = 100, replace = F)
      }
      
      # define value to plot and range of the histogram for the while loop check
      toplot  <- mean(pop$population[sample$sample])
      xlowlim <- floor(mean(pop$population)   - 3.5*sd(pop$population)/sqrt(10))
      xuplim  <- ceiling(mean(pop$population) + 3.5*sd(pop$population)/sqrt(10))
    }

    sample$means      <- c(sample$means, mean(pop$population[sample$sample]))
  }
  )
  
  observeEvent(input$sample10button,{
    for(i in 1:10){
      
      # ensure value falls in range of histogram
      toplot <- 10; xlowlim <- xuplim <- 1
      while(!(toplot > xlowlim & toplot < xuplim)){
      
        if(input$sample.size == 10){
        sample$sample     <- sample(x=(1:N), size = 10, replace = F)
        }
        if(input$sample.size == 30){
          sample$sample   <- sample(x=(1:N), size = 30, replace = F)
        }
        if(input$sample.size == 100){
          sample$sample   <- sample(x=(1:N), size = 100, replace = F)
        }
        
        # define value to plot and range of the histogram for the while loop check
        toplot  <- mean(pop$population[sample$sample])
        xlowlim <- floor(mean(pop$population)   - 3.5*sd(pop$population)/sqrt(10))
        xuplim  <- ceiling(mean(pop$population) + 3.5*sd(pop$population)/sqrt(10))
        
      }
      
      sample$means      <- c(sample$means, mean(pop$population[sample$sample]))
    }
  }
  )
  
  observeEvent(input$sample1000button,{
    for(i in 1:1000){
      
      # ensure value falls in range of histogram
      toplot <- 10; xlowlim <- xuplim <- 1
      while(!(toplot > xlowlim & toplot < xuplim)){
        
        if(input$sample.size == 10){
          sample$sample     <- sample(x=(1:N), size = 10, replace = F)
        }
        if(input$sample.size == 30){
          sample$sample   <- sample(x=(1:N), size = 30, replace = F)
        }
        if(input$sample.size == 100){
          sample$sample   <- sample(x=(1:N), size = 100, replace = F)
        }
        
        # define value to plot and range of the histogram for the while loop check
        toplot  <- mean(pop$population[sample$sample])
        xlowlim <- floor(mean(pop$population)   - 3.5*sd(pop$population)/sqrt(10))
        xuplim  <- ceiling(mean(pop$population) + 3.5*sd(pop$population)/sqrt(10))
        
      }
      
      sample$means      <- c(sample$means, mean(pop$population[sample$sample]))
    }
  }
  )
  
  observeEvent(input$samplebutton2,{
    
    # ensure value falls in range of histogram
    toplot <- 10; xlowlim <- xuplim <- 1
    while(!(toplot > xlowlim & toplot < xuplim)){
      
      if(input$sample.size2 == 10){
        sample$sample2_1  <- sample(x=(1:N/2), size = 5, replace = F)
        sample$sample2_2  <- sample(x=(1:N/2), size = 5, replace = F)
      }
      if(input$sample.size2 == 30){
        sample$sample2_1     <- sample(x=(1:N/2), size = 15, replace = F)
        sample$sample2_2     <- sample(x=(1:N/2), size = 15, replace = F)
      }
      if(input$sample.size2 == 100){
        sample$sample2_1     <- sample(x=(1:N/2), size = 50, replace = F)
        sample$sample2_2     <- sample(x=(1:N/2), size = 50, replace = F)
      }
      
      # define value to plot and range of the histogram for the while loop check
      toplot  <- mean(pop$population2_1[sample$sample2_1]) - mean(pop$population2_2[sample$sample2_2])
      xlowlim <- floor(pop$mu2_diff   - 3.5*sqrt((var(pop$population2_1)+var(pop$population2_2))/10))
      xuplim  <- ceiling(pop$mu2_diff + 3.5*sqrt((var(pop$population2_1)+var(pop$population2_2))/10))
    
    }
    
    sample$means2_1      <- c(sample$means2_1, mean(pop$population2_1[sample$sample2_1]))
    sample$means2_2      <- c(sample$means2_2, mean(pop$population2_1[sample$sample2_1]))
    sample$means2_diff   <- c(sample$means2_diff, mean(pop$population2_1[sample$sample2_1])-mean(pop$population2_2[sample$sample2_2]))
  }
  )
  
  observeEvent(input$sample10button2,{
   for(i in 1:10){
     
     # ensure value falls in range of histogram
     toplot <- 10; xlowlim <- xuplim <- 1
     while(!(toplot > xlowlim & toplot < xuplim)){
       
        if(input$sample.size2 == 10){
        sample$sample2_1  <- sample(x=(1:N/2), size = 5, replace = F)
        sample$sample2_2  <- sample(x=(1:N/2), size = 5, replace = F)
        }
        if(input$sample.size2 == 30){
          sample$sample2_1     <- sample(x=(1:N/2), size = 15, replace = F)
          sample$sample2_2     <- sample(x=(1:N/2), size = 15, replace = F)
        }
        if(input$sample.size2 == 100){
          sample$sample2_1     <- sample(x=(1:N/2), size = 50, replace = F)
          sample$sample2_2     <- sample(x=(1:N/2), size = 50, replace = F)
        }
       
        # define value to plot and range of the histogram for the while loop check
        toplot  <- mean(pop$population2_1[sample$sample2_1]) - mean(pop$population2_2[sample$sample2_2])
        xlowlim <- floor(pop$mu2_diff   - 3.5*sqrt((var(pop$population2_1)+var(pop$population2_2))/10))
        xuplim  <- ceiling(pop$mu2_diff + 3.5*sqrt((var(pop$population2_1)+var(pop$population2_2))/10))
       
     }
      sample$means2_1      <- c(sample$means2_1, mean(pop$population2_1[sample$sample2_1]))
      sample$means2_2      <- c(sample$means2_2, mean(pop$population2_2[sample$sample2_2]))
      sample$means2_diff   <- c(sample$means2_diff, mean(pop$population2_1[sample$sample2_1])-mean(pop$population2_2[sample$sample2_2]))
   }
  }
  )
  
  observeEvent(input$sample1000button2,{
    for(i in 1:1000){
      
      # ensure value falls in range of histogram
      toplot <- 10; xlowlim <- xuplim <- 1
      while(!(toplot > xlowlim & toplot < xuplim)){
        
        if(input$sample.size2 == 10){
          sample$sample2_1  <- sample(x=(1:N/2), size = 5, replace = F)
          sample$sample2_2  <- sample(x=(1:N/2), size = 5, replace = F)
        }
        if(input$sample.size2 == 30){
          sample$sample2_1     <- sample(x=(1:N/2), size = 15, replace = F)
          sample$sample2_2     <- sample(x=(1:N/2), size = 15, replace = F)
        }
        if(input$sample.size2 == 100){
          sample$sample2_1     <- sample(x=(1:N/2), size = 50, replace = F)
          sample$sample2_2     <- sample(x=(1:N/2), size = 50, replace = F)
        }
        
        # define value to plot and range of the histogram for the while loop check
        toplot  <- mean(pop$population2_1[sample$sample2_1]) - mean(pop$population2_2[sample$sample2_2])
        xlowlim <- floor(pop$mu2_diff   - 3.5*sqrt((var(pop$population2_1)+var(pop$population2_2))/10))
        xuplim  <- ceiling(pop$mu2_diff + 3.5*sqrt((var(pop$population2_1)+var(pop$population2_2))/10))
        
      }
      sample$means2_1      <- c(sample$means2_1, mean(pop$population2_1[sample$sample2_1]))
      sample$means2_2      <- c(sample$means2_2, mean(pop$population2_2[sample$sample2_2]))
      sample$means2_diff   <- c(sample$means2_diff, mean(pop$population2_1[sample$sample2_1])-mean(pop$population2_2[sample$sample2_2]))
    }
  }
  )
  
  # reset sample history if sample size is changed
  observeEvent(input$sample.size, {
    sample$means    = NULL 
    sample$sample   = NULL
    pop$sampleind   = rep(0, N)
  })
  
  observeEvent(input$sample.size2, {
    sample$means2_1    = NULL 
    sample$means2_2    = NULL 
    sample$means2_diff = NULL
    sample$sample2_1   = NULL
    sample$sample2_2   = NULL
  })
  
  # reset sample history if reset button is pressed
  observeEvent(input$reset, {
    sample$means    = NULL 
    sample$sample   = NULL
  })
  
  observeEvent(input$reset2, {
    sample$means2_1    = NULL 
    sample$means2_2    = NULL 
    sample$means2_diff = NULL
    sample$sample2_1   = NULL
    sample$sample2_2   = NULL
  })
  
  
  
  # population parameters for single mean tab
  ppars <- eventReactive(c(input$pop.pars_1, input$pop.pars_2), {
    
    pop$par1 <- input$pop.pars_1
    pop$par2 <- input$pop.pars_2
    
  })
  
  # binwidth dotplots for single mean tab
  pwidth <- eventReactive(c(input$pop.distr,input$genButton), {
    
    if(input$pop.distr == "Beta")   {a       <- pop$par1; b <- pop$par2
    beta_sd <- sqrt( a*b / ((a+b)^2*(a+b+1)) )
    pop$width <- beta_sd*0.8}
    if(input$pop.distr == "Uniform"){pop$width <- (pop$par2 - pop$par1) / 50}
    if(input$pop.distr == "Normal") {pop$width <- pop$par2*0.09}
    
  })
  
  # population data for single mean tab.
  pplot <- eventReactive(c(input$pop.distr,input$genButton), {
    
    # population distribution
    if(input$pop.distr == "Beta"){
      pop$population <- rbeta(N, input$pop.pars_1, input$pop.pars_2)*10
      pop$mu         <- mean(pop$population)
      pop$sd         <- sqrt((N-1)/N) * sd(pop$population)
    } 
    if(input$pop.distr == "Uniform"){
      pop$population <- runif(n = N, min = input$pop.pars_1, max = input$pop.pars_2)
      pop$mu         <- mean(pop$population)
      pop$sd         <- sqrt((N-1)/N) * sd(pop$population)
    } 
    if(input$pop.distr == "Normal"){
      # pop$population <- rnorm(n = N, mean = input$pop.pars_1, sd = input$pop.pars_2)
      pop$population <- input$pop.pars_1 + input$pop.pars_2*scale(rnorm(n = N))
      pop$mu         <- mean(pop$population)
      pop$sd         <- sd(pop$population)
    }
    
  })
  
  # population parameters for two mean tab
  ppars2 <- eventReactive(c(input$pop.pars21_1, input$pop.pars_21_2,
                            input$pop.pars22_1, input$pop.pars_22_2), {
                              
                              pop$par21_1 <- input$pop.pars21_1 #tab2 group1 param1
                              pop$par21_2 <- input$pop.pars21_2
                              pop$par22_1 <- input$pop.pars22_1
                              pop$par22_2 <- input$pop.pars22_2
                              
                            })
  
  # binwidth dotplots for two mean tab
  pwidth2 <- eventReactive(c(input$pop.distr2,input$genButton2, input$pop.pars21_1, input$pop.pars_21_2,
                             input$pop.pars22_1, input$pop.pars_22_2), {
    
    if(input$pop.distr == "Beta")   {a1 <- input$pop.pars21_1; b1 <- input$pop.pars21_2; beta_sd1 <- sqrt( a1*b1 / ((a1+b1)^2*(a1+b1+1)) )
                                     a2 <- input$pop.pars22_1; b2 <- input$pop.pars22_2; beta_sd2 <- sqrt( a2*b2 / ((a2+b2)^2*(a2+b2+1)) )
                                     pop$width2_1 <- beta_sd1*0.8
                                     pop$width2_2 <- beta_sd2*0.8}
    if(input$pop.distr == "Uniform"){pop$width2_1 <- (input$pop.pars21_2 - input$pop.pars21_1) / 50
                                     pop$width2_2 <- (input$pop.pars22_2 - input$pop.pars22_1) / 50}
    if(input$pop.distr == "Normal") {pop$width2_1 <- input$pop.pars21_2*0.09
                                     pop$width2_2 <- input$pop.pars22_2*0.09}
    
  })
  
  # population data for two mean tab.
  pplot2 <- eventReactive(c(input$pop.distr2,input$genButton2), {
    
    # population distribution
    if(input$pop.distr2 == "Beta"){
      pop$population2_1 <- rbeta(N/2, input$pop.pars21_1, input$pop.pars21_2)*10 # pop.parsXY_Z: tab X, group X, par Z
      pop$population2_2 <- rbeta(N/2, input$pop.pars22_1, input$pop.pars22_2)*10
      pop$sd2_1         <- sqrt((N/2-1)/(N/2)) * sd(pop$population2_1)
      pop$sd2_2         <- sqrt((N/2-1)/(N/2)) * sd(pop$population2_2)
    } 
    if(input$pop.distr2 == "Uniform"){
      pop$population2_1 <- runif(n = N/2, min = input$pop.pars21_1, max = input$pop.pars21_2)
      pop$population2_2 <- runif(n = N/2, min = input$pop.pars22_1, max = input$pop.pars22_2)
      pop$sd2_1         <- sqrt((N/2-1)/(N/2)) * sd(pop$population2_1)
      pop$sd2_2         <- sqrt((N/2-1)/(N/2)) * sd(pop$population2_2)
    } 
    if(input$pop.distr2 == "Normal"){
      pop$population2_1 <- input$pop.pars21_1 + input$pop.pars21_2*scale(rnorm(n = N/2))
      pop$population2_2 <- input$pop.pars22_1 + input$pop.pars22_2*scale(rnorm(n = N/2))
      pop$sd2_1         <- sd(pop$population2_1)
      pop$sd2_2         <- sd(pop$population2_2)
    }
    
    pop$mu2_1           <- mean(pop$population2_1)
    pop$mu2_2           <- mean(pop$population2_2)
    pop$mu2_diff        <- pop$mu2_1 - pop$mu2_2
    
    pop$poprange2       <- range(c(pop$population2_1, pop$population2_2))
    
  })
  
  
  # population plot for single mean tab.
  output$popplot <- renderPlot({
    
    ppars()
    pwidth()
    pplot()
    
    # data frame of values to plot
    plotdat        <- as.data.frame(cbind(pop$population, rep(0, N)))
    names(plotdat) <- c("population", "sampleind")
    plotdat$sampleind[sample$sample] <- 1
    
    # create a dotplot for population
    if(length(pop$population) > 0){
      
      plot <- ggplot(plotdat, aes(x = population)) +
        geom_dotplot(binwidth = pop$width, color = 'gray48', fill = 'gray48') +
        scale_y_continuous(NULL, breaks = NULL)
      
      # color the sampled points and add triangle to denote sample mean
      if(length(sample$means) > 0){
        
        plot <- ggplot(plotdat, aes(x = population, color = factor(sampleind), fill = factor(sampleind))) +
          scale_color_manual(values = c('gray48', 'blue2')) +
          scale_fill_manual(values = c('gray48', 'blue2')) +
          geom_dotplot(binwidth = pop$width, stackgroups = TRUE, binpositions = "all") +
          scale_y_continuous(NULL, breaks = NULL) +
          theme(legend.position = "none") +
          annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
                   label = paste0("italic(M) == ", round(mean(pop$population[sample$sample]), 2)), 
                   parse = TRUE, size = 5, color = 'red') +
          geom_point(aes(x=mean(pop$population[sample$sample]), y = 0), shape = 25, 
                     size = 4.5, colour= "red", fill = "red")
      }
    }
    
    plot
    
  })
  
  
  # population table for single mean tab
  output$poptable <- renderTable({
    
    pplot()
    pop_descr           <- cbind(format(N, digits = 3), 
                                 format(pop$mu, digits = 2, nsmall = 2), 
                                 format(pop$sd, digits = 2, nsmall = 2))
    colnames(pop_descr) <- c("N", "\u03BC", "\u03C3")
    
    pop_descr
  
  }, include.colnames=FALSE,
      add.to.row = list(pos = list(0), 
      command = " <tr> <th> N </th> <th> &#956;</th> <th> &#963;</th> </tr>" ),
      caption=paste("   Population descriptives:"),
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))  
      
  
  # population table for two mean tab
  output$poptable2 <- renderTable({
    
    pplot2()
    pop_descr2_1        <- cbind(format(N/2, digits = 3), 
                                 format(pop$mu2_1, digits = 2, nsmall = 2), 
                                 format(pop$sd2_1, digits = 2, nsmall = 2))
    
    pop_descr2_2        <- cbind(format(N/2, digits = 3), 
                                 format(pop$mu2_2, digits = 2, nsmall = 2), 
                                 format(pop$sd2_2, digits = 2, nsmall = 2))
    
    pop_meandiff        <- format(pop$mu2_diff, digits = 2, nsmall = 2)
    
    cbind(pop_descr2_1, " ", " ", pop_descr2_2, " ", " ", pop_meandiff)
    
  }, include.colnames=FALSE,
  add.to.row = list(pos = list(0), 
                    command = " <tr> <th> N1 </th> <th> &#956;1 </th> <th> &#963;1 </th> <th> </th> <th> </th> <th> N2 </th> <th> &#956;2 </th> <th> &#963;2 </th> <th> </th> <th> </th> <th> &#956;1-&#956;2 </th> </tr>" ),
  caption=paste("   Population descriptives:"),
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL)) 
  
  
  # population plot for two means tab. 
  output$popplot2 <- renderPlot({
    
    ppars2()
    pwidth2()
    pplot2()
    
    xlow2 <- min(pop$poprange2)
    xup2  <- max(pop$poprange2)
    
    # data frame of values to plot
    plotdat2_1        <- as.data.frame(cbind(pop$population2_1, rep(0, N/2)))
    plotdat2_2        <- as.data.frame(cbind(pop$population2_2, rep(0, N/2)))
    names(plotdat2_1) <- names(plotdat2_2) <- c("population", "sampleind")
    plotdat2_1$sampleind[sample$sample2_1] <- 1
    plotdat2_2$sampleind[sample$sample2_2] <- 1
    
    # create a dotplot for population
    if(length(pop$population2_1) > 0){
      
      plot2_1 <- ggplot(plotdat2_1, aes(x = population)) +
        geom_dotplot(binwidth = pop$width2_1, color = 'gray48', fill = 'gray48') +
        scale_y_continuous(NULL, breaks = NULL) +
        xlim(xlow2, xup2) 
      
      plot2_2 <- ggplot(plotdat2_2, aes(x = population)) +
        geom_dotplot(binwidth = pop$width2_2, color = 'gray48', fill = 'gray48') +
        scale_y_continuous(NULL, breaks = NULL) +
        xlim(xlow2, xup2)
      
      plot    <- multiplot(plot2_1, plot2_2)
      
      # color the sampled points red
      if(length(sample$means2_1) > 0){
        
        plot2_1 <- ggplot(plotdat2_1, aes(x = population, color = factor(sampleind), fill = factor(sampleind))) +
          scale_color_manual(values = c('gray48', 'blue2')) +
          scale_fill_manual(values = c('gray48', 'blue2')) +
          geom_dotplot(binwidth = pop$width2_1, stackgroups = TRUE, binpositions = "all") +
          scale_y_continuous(NULL, breaks = NULL) +
          xlim(xlow2, xup2) +
          theme(legend.position = "none") +
          annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
                   label = paste0("italic(M[1]) == ", round(mean(pop$population2_1[sample$sample2_1]), 2)), 
                   parse = TRUE, size = 5, color = 'red') +
          geom_point(aes(x=mean(pop$population2_1[sample$sample2_1]), y = 0), shape = 25, 
                     size = 3.7, colour= "red", fill = "red")
        
        plot2_2 <- ggplot(plotdat2_2, aes(x = population, color = factor(sampleind), fill = factor(sampleind))) +
          scale_color_manual(values = c('gray48', 'blue2')) +
          scale_fill_manual(values = c('gray48', 'blue2')) +
          geom_dotplot(binwidth = pop$width2_2, stackgroups = TRUE, binpositions = "all") +
          scale_y_continuous(NULL, breaks = NULL) +
          xlim(xlow2, xup2) +
          theme(legend.position = "none") +
          annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
                   label = paste0("italic(M[2]) == ", round(mean(pop$population2_2[sample$sample2_2]), 2)), 
                   parse = TRUE, size = 5, color = 'red') +
          geom_point(aes(x=mean(pop$population2_2[sample$sample2_2]), y = 0), shape = 25, 
                     size = 3.7, colour= "red", fill = "red")
        
        plot    <- multiplot(plot2_1, plot2_2)
        
      }
    }
    plot
  }
  )
  
  
  # sampling table tab1
  output$samplingtable <- renderTable({
    
    if(length(sample$means) > 0){
      
      n_samples     <- length(sample$means)
      mean_sampmean <- mean(sample$means, na.rm = TRUE)
      sd_sampmean   <- sd(sample$means, na.rm = TRUE)
      
      samptable <- cbind(format(n_samples, nsmall = 0), 
                         format(mean_sampmean, digits = 2, nsmall = 2), 
                         format(sd_sampmean, digits = 2, nsmall = 2))
      colnames(samptable) <- c("Number of samples", "Mean of sample means", "Standard error")
      
      samptable
    }
    
  })
  
  
  # sampling table tab2
  output$samplingtable2 <- renderTable({
    
    if(length(sample$means2_diff) > 0){
      
      n_samples2     <- length(sample$means2_diff)
      mean_sampmean2 <- mean(sample$means2_diff, na.rm = TRUE)
      sd_sampmean2   <- sd(sample$means2_diff, na.rm = TRUE)
      
      samptable2 <- cbind(format(n_samples2, nsmall = 0), 
                         format(mean_sampmean2, digits = 2, nsmall = 2), 
                         format(sd_sampmean2, digits = 2, nsmall = 2))
      colnames(samptable2) <- c("Number of samples", "Mean of sample differences in means", "Standard error")
      
      samptable2
    }
    
  })
  
  # histogram tab1
  output$meanhist <- renderPlot({
    
    # create a histogram for all sampled means
    if(length(sample$means) > 0){
      
      # define range x-axis
      if(input$sample.size == 10) {n <- 10}
      if(input$sample.size == 30) {n <- 30}
      if(input$sample.size == 100){n <- 100}
      
      histrange <- 3.5*sd(pop$population)/sqrt(10)
      
      xlow_h <- floor(mean(pop$population) - histrange)
      xup_h  <- ceiling(mean(pop$population) + histrange)
      
      # define bin width for the histograms (breaks by)
      histbin <- (xup_h - xlow_h)/22
    
      hist(sample$means, xlim = c(xlow_h, xup_h),breaks = seq(xlow_h, xup_h, by=histbin), 
           xlab = "Histogram of means of the drawn samples", main = "")
      
      # obtain break values in histogram for the last sample mean
      sampbreak <- seq(from = seq(xlow_h, xup_h, by=histbin)[which(hist(plot=F,sample$means[length(sample$means)], 
                                                               breaks = seq(xlow_h, xup_h, by=histbin))$counts == 1)],
                       to = seq(xlow_h, xup_h, by=histbin)[which(hist(plot=F,sample$means[length(sample$means)], 
                                                             breaks = seq(xlow_h, xup_h, by=histbin))$counts == 1)+1], 
                       by = histbin)
      
      # obtain frequency of the break value for the last sample mean
      sampfreq <-  hist(plot=F,sample$means, 
                        breaks = seq(xlow_h, xup_h, by=histbin))$counts[which(hist(plot=F, 
                                                                          sample$means[length(sample$means)], 
                                                                          breaks = seq(xlow_h, xup_h, by=histbin))$counts==1)]
      
      # color the last sample mean red in histogram
      xx <- c(sampbreak, rev(sampbreak))
      yy <- c(sampfreq, sampfreq, sampfreq-1, sampfreq-1)
      
      polygon(x = xx, y = yy, col = "red", density = 100, angle = 0, border = "black")
      
      # add text with value of last sample mean to plot
      legend("topright", legend = bquote(italic('M') == .(round(mean(pop$population[sample$sample]), 2))), bty ="n", cex = 1, text.col = 'red')
      
    }
  }
  )
  
  
  
  # histogram tab2
  output$meanhist2 <- renderPlot({
    
    # create a histogram for all sampled differences in means
    if(length(sample$means2_diff) > 0){
      
      # define range x-axis
      if(input$sample.size2 == 10) {n <- 10}
      if(input$sample.size2 == 30) {n <- 30}
      if(input$sample.size2 == 100){n <- 100}
      
      histrange2 <- 3.5*sqrt((var(pop$population2_1)+var(pop$population2_2))/10)
      
      xlow_h2 <- floor(pop$mu2_diff - histrange2)
      xup_h2  <- ceiling(pop$mu2_diff + histrange2)
      
      # define bin width for the histograms (breaks by)
      histbin2 <- (xup_h2 - xlow_h2)/22
      
      hist(sample$means2_diff, xlim = c(xlow_h2, xup_h2),breaks = seq(xlow_h2, xup_h2, by=histbin2), 
           xlab = "Histogram of difference in group means of the drawn samples", main = "")
      
      # obtain break values in histogram for the last sample difference in means
      sampbreak <- seq(from = seq(xlow_h2, xup_h2, by=histbin2)[which(hist(plot=F,sample$means2_diff[length(sample$means2_diff)], 
                                                               breaks = seq(xlow_h2, xup_h2, by=histbin2))$counts == 1)],
                       to = seq(xlow_h2, xup_h2, by=histbin2)[which(hist(plot=F,sample$means2_diff[length(sample$means2_diff)], 
                                                             breaks = seq(xlow_h2, xup_h2, by=histbin2))$counts == 1)+1], 
                       by = histbin2)
      
      # obtain frequency of the break value for the last sample difference in means
      sampfreq <-  hist(plot=F,sample$means2_diff, 
                        breaks = seq(xlow_h2, xup_h2, by=histbin2))$counts[which(hist(plot=F, 
                                                                          sample$means2_diff[length(sample$means2_diff)], 
                                                                          breaks = seq(xlow_h2, xup_h2, by=histbin2))$counts==1)]
      
      # color the last sample difference in means red in histogram
      xx <- c(sampbreak, rev(sampbreak))
      yy <- c(sampfreq, sampfreq, sampfreq-1, sampfreq-1)
      
      polygon(x = xx, y = yy, col = "red", density = 100, angle = 0, border = "black")
      
      # add text with value of last sample difference in means to plot
      legend("topright", legend = bquote(italic(M[1]-M[2]) == .(round(mean(pop$population2_1[sample$sample2_1])-mean(pop$population2_2[sample$sample2_2]), 2))), 
             bty ="n", cex = 1, text.col = 'red')
      
    }
  }
  )
  
  
}