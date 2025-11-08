# app.R

library(shiny)
library(ggplot2)
library(stats4)

ui <- fluidPage(
  titlePanel("Maximum Likelihood Estimation for Univariate Distributions"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose a Distribution:",
                  choices = c("Normal", "Exponential", "Poisson", "Uniform", "Binomial")),
      numericInput("n", "Sample Size:", value = 50, min = 10, max = 500),
      actionButton("generate", "Generate New Sample"),
      hr(),
      uiOutput("distParams")
    ),
    
    mainPanel(
      plotOutput("likelihoodPlot"),
      tableOutput("mleTable")
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactiveVal()
  
  observeEvent(input$generate, {
    n <- input$n
    switch(input$dist,
           "Normal" = data(rnorm(n, mean = 5, sd = 2)),
           "Exponential" = data(rexp(n, rate = 1/2)),
           "Poisson" = data(rpois(n, lambda = 4)),
           "Uniform" = data(runif(n, min = 0, max = 10)),
           "Binomial" = data(rbinom(n, size = 10, prob = 0.5))
    )
  })
  
  output$distParams <- renderUI({
    switch(input$dist,
           "Normal" = tagList(
             numericInput("meanGuess", "Initial Mean Guess:", value = 5),
             numericInput("sdGuess", "Initial SD Guess:", value = 2)
           ),
           "Exponential" = numericInput("rateGuess", "Initial Rate Guess:", value = 0.5),
           "Poisson" = numericInput("lambdaGuess", "Initial λ Guess:", value = 3),
           "Uniform" = tagList(
             numericInput("minGuess", "Initial Min Guess:", value = 0),
             numericInput("maxGuess", "Initial Max Guess:", value = 10)
           ),
           "Binomial" = numericInput("probGuess", "Initial p Guess:", value = 0.5)
    )
  })
  
  output$likelihoodPlot <- renderPlot({
    req(data())
    
    x <- data()
    dist <- input$dist
    
    if (dist == "Normal") {
      mean_vals <- seq(min(x), max(x), length.out = 100)
      ll <- sapply(mean_vals, function(mu)
        sum(dnorm(x, mean = mu, sd = sd(x), log = TRUE)))
      df <- data.frame(mean_vals, ll)
      ggplot(df, aes(mean_vals, ll)) +
        geom_line() + labs(x = "Mean", y = "Log-Likelihood",
                           title = "Normal Distribution MLE (varying mean)")
    }
    
    else if (dist == "Exponential") {
      rate_vals <- seq(0.01, 2, length.out = 100)
      ll <- sapply(rate_vals, function(r)
        sum(dexp(x, rate = r, log = TRUE)))
      df <- data.frame(rate_vals, ll)
      ggplot(df, aes(rate_vals, ll)) +
        geom_line() + labs(x = "Rate", y = "Log-Likelihood",
                           title = "Exponential Distribution MLE (varying rate)")
    }
    
    else if (dist == "Poisson") {
      lambda_vals <- seq(0.1, max(x)+3, length.out = 100)
      ll <- sapply(lambda_vals, function(l)
        sum(dpois(x, lambda = l, log = TRUE)))
      df <- data.frame(lambda_vals, ll)
      ggplot(df, aes(lambda_vals, ll)) +
        geom_line() + labs(x = "Lambda", y = "Log-Likelihood",
                           title = "Poisson Distribution MLE (varying lambda)")
    }
    
    else if (dist == "Uniform") {
      min_vals <- seq(min(x)-2, min(x)+2, length.out = 100)
      ll <- sapply(min_vals, function(a)
        sum(dunif(x, min = a, max = max(x), log = TRUE)))
      df <- data.frame(min_vals, ll)
      ggplot(df, aes(min_vals, ll)) +
        geom_line() + labs(x = "Min", y = "Log-Likelihood",
                           title = "Uniform Distribution MLE (varying min)")
    }
    
    else if (dist == "Binomial") {
      prob_vals <- seq(0.01, 0.99, length.out = 100)
      ll <- sapply(prob_vals, function(p)
        sum(dbinom(x, size = 10, prob = p, log = TRUE)))
      df <- data.frame(prob_vals, ll)
      ggplot(df, aes(prob_vals, ll)) +
        geom_line() + labs(x = "Probability (p)", y = "Log-Likelihood",
                           title = "Binomial Distribution MLE (varying p)")
    }
  })
  
  output$mleTable <- renderTable({
    req(data())
    x <- data()
    dist <- input$dist
    
    if (dist == "Normal") {
      fit <- mle(function(mean, sd) -sum(dnorm(x, mean, sd, log=TRUE)),
                 start = list(mean = mean(x), sd = sd(x)))
      est <- coef(fit)
      data.frame(Parameter = names(est), Estimate = round(est, 3))
    }
    
    else if (dist == "Exponential") {
      fit <- mle(function(rate) -sum(dexp(x, rate, log=TRUE)),
                 start = list(rate = 1/mean(x)))
      est <- coef(fit)
      data.frame(Parameter = "Rate", Estimate = round(est, 3))
    }
    
    else if (dist == "Poisson") {
      fit <- mle(function(lambda) -sum(dpois(x, lambda, log=TRUE)),
                 start = list(lambda = mean(x)))
      est <- coef(fit)
      data.frame(Parameter = "Lambda", Estimate = round(est, 3))
    }
    
    else if (dist == "Uniform") {
      fit <- list(min = min(x), max = max(x))
      data.frame(Parameter = c("Min", "Max"), Estimate = c(fit$min, fit$max))
    }
    
    else if (dist == "Binomial") {
      fit <- mle(function(prob) -sum(dbinom(x, size = 10, prob, log=TRUE)),
                 start = list(prob = mean(x)/10))
      est <- coef(fit)
      data.frame(Parameter = "p", Estimate = round(est, 3))
    }
  })
}

shinyApp(ui, server)
