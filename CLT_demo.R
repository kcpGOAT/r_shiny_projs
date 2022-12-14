library(shiny)

ui <- fluidPage(
  titlePanel("Demonstration of the Central Limit Theorem"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", "Distribution", c("Normal", "Uniform", "Poisson", "Binomial", "Beta"), 
                   selected = "Normal"), 
      numericInput("n_sample", "Number of samples", value = 50), 
      numericInput("size", "Sample size", value = 100),
      sliderInput("n_bins", "Number of bins", min = 5, max = 50, value = 10),
      checkboxInput("prob", "Set y-axis to density", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        id = "stats",
        tabPanel("Sample means", plotOutput("hist_mean")),
        tabPanel("Sample sums", plotOutput("hist_sum")),
        tabPanel("Population", plotOutput("hist_pop"))
      ),
      tabsetPanel(
        id = "summary_stats", 
        type = "hidden", 
        tabPanel("Sample means", 
                 verbatimTextOutput("summary_mean")),
        tabPanel("Sample sums",
                 verbatimTextOutput("summary_sum")),
        tabPanel("Population",
                 verbatimTextOutput("summary_pop"))
      )
    )
  ),
  h1("Parameters", style = "font-size:20px;"),
  
  tabsetPanel(
    id = "params",
    type = "hidden",
    tabPanel("Normal",
             numericInput("mean", "Mean", value = 0),
             numericInput("sd", "SD", value = 1)
    ),
    tabPanel("Uniform",
             numericInput("min", "Min", value = 0),
             numericInput("max", "Max", value = 1)
    ),
    tabPanel("Poisson",
             numericInput("r", "Rate", value = 1)
    ),
    tabPanel("Binomial",
             numericInput("p", "Probability of success", value = 0.5),
             numericInput("n", "Number of trials", value = 10)
    ),
    tabPanel("Beta", 
             numericInput("a", "α", value = 5),
             numericInput("b", "β", value = 5)
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$dist, {
    updateTabsetPanel(inputId = "params", selected = input$dist)
  })
  observeEvent(input$stats, {
    updateTabsetPanel(inputId = "summary_stats", selected = input$stats)
  })
  
  samples <- reactive({
    switch(input$dist, 
      Normal = replicate(n = input$n_sample, 
                         rnorm(input$size, input$mean, input$sd)),
      Uniform = replicate(n = input$n_sample, 
                          runif(input$size, input$min, input$max)), 
      Poisson = replicate(n = input$n_sample, 
                          rpois(input$size, input$r)), 
      Binomial = replicate(n = input$n_sample, 
                           rbinom(input$size, input$n, input$p)),
      Beta = replicate(n = input$n_sample, 
                       rbeta(input$size, input$a, input$b))
    )
  })
  sample_dist_mean <- reactive({
      apply(samples(), MARGIN = 2, mean) |>
        unlist() |> 
        as.numeric()
  })
  sample_dist_sum <- reactive({
    apply(samples(), MARGIN = 2, sum) |>
      unlist() |>
      as.numeric()
  })
  pop_dist <- reactive({
    as.vector(samples())
  })
  
  output$hist_mean <- renderPlot({
      hist(sample_dist_mean(),
           prob = input$prob,
           main = "Sampling distribution of the mean",
           xlab = "Sample means",
           col = "blue", 
           breaks = input$n_bins, 
           density = 50)
    })
  output$hist_sum <- renderPlot({
      hist(sample_dist_sum(),
           prob = input$prob,
           main = "Sampling distribution of the sum",
           xlab = "Sample sums", 
           col = "red",
           breaks = input$n_bins, 
           density = 50)
    })
  output$hist_pop <- renderPlot({
    hist(pop_dist(),
         prob = input$prob,
         main = "Overall population distribution", 
         xlab = "x", 
         col = "purple", 
         breaks = input$n_bins, 
         density = 50)
  })

  output$summary_mean <- renderPrint({
    summary(sample_dist_mean())
  })
  output$summary_sum <- renderPrint({
    summary(sample_dist_sum())
  })
  output$summary_pop <- renderPrint({
    summary(pop_dist())
  })
}

shinyApp(ui, server)
