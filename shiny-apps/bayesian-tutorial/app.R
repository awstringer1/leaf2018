# Shiny app for Bayesian tutorial
# 
# 
# Load packages
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)


### User Interface ###
ui <- fluidPage(
  titlePanel("Bayesian Coin Flipping"),
  sidebarLayout(
    sidebarPanel(
      h4("Set Model Parameters"),
      p("Here you can change model parameters and observed data to get a feel for the effect 
        each has on the resulting frquentist and bayesian inferences. Refer to the 
        Bayesian Statistics tutorial for context."),
      numericInput(
        "numberflips",
        "Number of Coin Flips",
        value = 10,
        min = 1,
        max = 1000
      ),
      numericInput(
        "propnheads",
        "Proportion of Heads",
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.05
      ),
      numericInput(
        "priormean",
        "Prior Mean",
        value = 0.5,
        min = 0.01,
        max = 0.99,
        step = 0.05
      ),
      numericInput(
        "priorsd",
        "Prior Standard Deviation",
        value = 0.1,
        min = 0.0001,max = 0.5,
        step = 0.05
      )
    ),
    mainPanel(
      plotOutput("theplot"),
      br(),
      textOutput("postpoint"),
      textOutput("freqpoint"),
      textOutput("bayes_int"),
      textOutput("freq_int"),
      br(),
      plotOutput("interval_plot")
    )
  )
)

### Server ###
server <- function(input,output) {
  # Prior, posterior, point and interval estimation
  prior <- function(theta,a,b) dbeta(theta,shape1 = a,shape2 = b)
  posterior <- function(theta,a,b,x,n) dbeta(theta,shape1 = x + a,shape2 = n - x + b)
  frequentist_point <- function(x,n) x / n
  frequentist_interval <- function(x,n) c(x/n - 1.96* sqrt((x/n)*(1- x/n))/n,x/n + 1.96* sqrt((x/n)*(1- x/n))/n)
  bayes_point <- function(x,n,a,b) {
    minus_post <- function(theta) -1 * posterior(theta,a,b,x,n)
    opt <- tryCatch(
      nlminb(objective = minus_post,start = frequentist_point(x,n)),
      error = function(e) list(par = -1,message = e)
    )
    opt$par
  }
  bayes_interval <- function(x,n,a,b) {
    c(qbeta(0.025,shape1 = x + a,shape2 = n - x + b),qbeta(0.975,shape1 = x + a,shape2 = n - x + b))  
  }
  
  # Compute the prior hyperparameters from the prior mean and variance
  compute_hyperparams <- function(pm,ps) {
    list(
      aa = pm * ( (pm*(1-pm)) / (ps^2) - 1),
      bb = (1-pm) * ( (pm*(1-pm)) / (ps^2) - 1)
    )
  }
  
  get_data <- function(p,n) list(x = p * n,n = n)
  
  # Plot the distributions
  plot_it <- function(params,dat) {
    freq_int <- frequentist_interval(dat$x,dat$n)
    bayes_int <- bayes_interval(dat$x,dat$n,params$aa,params$bb)
    data_frame(x = c(0.01,0.99)) %>%
      ggplot(aes(x = x)) +
      theme_classic() + 
      stat_function(fun = dbeta,
                    args = list(shape1 = params$aa,shape2 = params$bb),
                    colour = "blue") +
      stat_function(fun = dbeta,
                    args = list(shape1 = params$aa + dat$x,shape2 = params$bb + dat$n - dat$x),
                    colour = "purple") +
      labs(title = "Prior and Posterior for Theta",
           subtitle = "Blue: Prior. Purple: Posterior.",
           x = "Theta",
           y = "Density") +
      scale_x_continuous(breaks = seq(0,1,by=0.1))
  }
  
  output$theplot <- renderPlot({
    plot_it(params = compute_hyperparams(input$priormean,input$priorsd),dat = get_data(input$propnheads,input$numberflips))
  })
  
  # Point Estimates and intervals
  output$postpoint <- renderText({
    dt <- get_data(input$propnheads,input$numberflips)
    params <- compute_hyperparams(input$priormean,input$priorsd)
    tryit <- bayes_point(dt$x,dt$n,params$aa,params$bb)
    
    if (tryit == 0) {
      "Could not compute posterior mode; optimization failed with error"
    }
    else {
      str_c("Posterior mode point estimate: ",round(tryit,3))
    }
  })
  
  output$freqpoint <- renderText({
    dt <- get_data(input$propnheads,input$numberflips)
    est <- frequentist_point(dt$x,dt$n)
    str_c("Frequentist Point Estimate: ",round(est,3))
  })
  
  output$bayes_int <- renderText({
    dt <- get_data(input$propnheads,input$numberflips)
    params <- compute_hyperparams(input$priormean,input$priorsd)
    
    theint <- round(bayes_interval(dt$x,dt$n,params$aa,params$bb),3)
    
    str_c("Bayesian Credible Interval = (",theint[1],",",theint[2],")")
  })
  output$freq_int <- renderText({
    dt <- get_data(input$propnheads,input$numberflips)
    params <- compute_hyperparams(input$priormean,input$priorsd)
    
    theint <- round(frequentist_interval(dt$x,dt$n),3)
    
    str_c("Frequentist Confidence Interval = (",theint[1],",",theint[2],")")
  })
  
  plot_intervals <- function(params,dt) {
    freq_int <- frequentist_interval(dt$x,dt$n)
    bayes_int <- bayes_interval(dt$x,dt$n,params$aa,params$bb)
    freq_point <- frequentist_point(dt$x,dt$n)
    bayes_point <- bayes_point(dt$x,dt$n,params$aa,params$bb)
    
    data_frame(
      type = c(rep("Frequentist",3),rep("Bayesian",3)),
      value = c(freq_int,freq_point,bayes_int,bayes_point)
    ) %>%
      ggplot(aes(x = type,y = value,fill = type,group = type)) +
      theme_classic() +
      geom_line(aes(colour = type)) +
      geom_point(pch = 21,colour = "black") +
      labs(title = "Point and Interval Estimates",
           y = "Estimate",
           x = "") +
      coord_flip() +
      guides(fill = FALSE) +
      scale_y_continuous(breaks = seq(0,1,by=0.1))
  }
  
  output$interval_plot <- renderPlot({
    plot_intervals(params = compute_hyperparams(input$priormean,input$priorsd),dt = get_data(input$propnheads,input$numberflips))
  })
  
}


### App ###
shinyApp(ui = ui,server = server)
