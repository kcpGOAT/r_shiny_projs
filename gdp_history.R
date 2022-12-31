library(shiny)
library(tidyverse)
library(readxl)

# Data cleaning/processing

country_gdp <- read_excel("/Users/ryanquach/shiny_projects/gdp_history/country_gdp.xls")
colnames(country_gdp)[1] <- "country"

country_gdp <- country_gdp %>%
  arrange(country) %>%
  select(!`Country Code`) %>%
  pivot_longer(!country, 
               names_to = "year",
               values_to = "GDP") %>%
  filter(year != 2015, !is.na(GDP)) %>%
  group_by(country) %>%
  mutate(year = as.numeric(year), 
         GDP = GDP/1000000000, 
         growth = (GDP - lag(GDP))/lag(GDP)) 

complete_countries <- with(country_gdp, 
                           tapply(year, country, FUN = function(x) {
                             min(x) == 1960 & max(x) == 2014
                           }))
complete_countries <- unique(country_gdp$country)[complete_countries]

# Building app

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("GDP Comparison Tool by Country or Region: 1960-2014"),
  sidebarLayout(
    sidebarPanel(
      selectInput("location", "Countries/Regions", 
                  choices = country_gdp$country, 
                  multiple = TRUE), 
      sliderInput("range", "Year range",
                  min = 1960, max = 2014,
                  value = c(1960, 2014), sep = ""),
      checkboxInput("complete", "Only include countries with complete data?", 
                    value = FALSE),
      p("Note: The table at the bottom right is formatted for the purpose of comparing GDP (growth) over the specified time period, with each selected country or region having its own column. Proper statistical practice would entail having a single country column that would be treated as a categorical variable, thereby ensuring that each row is an observation.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Nominal GDP", 
                 plotOutput("GDP_plot"), 
                 dataTableOutput("dt_GDP")), 
        tabPanel("GDP Growth", 
                 plotOutput("growth_plot"), 
                 dataTableOutput("dt_growth"))
      ),
      dataTableOutput("dt")
    )
  )
)

server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  location_new <- reactive({
    country_gdp[country_gdp$country %in% input$location & country_gdp$year %in% input$range[1]:input$range[2], ]
  })
  
  observeEvent(input$complete, {
    updateSelectInput(inputId = "location", 
                      choices = 
                        if (input$complete == TRUE) {
                          complete_countries
                        }
                      else {
                        country_gdp$country
                      }
    )
  })
  
  output$GDP_plot <- renderPlot({
    ggplot(data = location_new(), aes(year, GDP, group = country)) +
      geom_point() +
      geom_line(aes(col = country)) + 
      labs(x = "Year", y = "GDP (billions of current US$)", title = paste0("Nominal GDP by Country: ", input$range[1], " to ", input$range[2])) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.title.y = element_text(face = "bold", vjust = 2.5), 
            axis.title.x = element_text(face = "bold", vjust = -1), 
            axis.text = element_text(color = "white")) +
      scale_x_continuous(breaks = pretty(input$range[1]:input$range[2], n = 27))
  })
  
  output$growth_plot <- renderPlot({
    ggplot(data = location_new(), aes(year, growth, group = country)) +
      geom_point() +
      geom_line(aes(col = country)) + 
      geom_hline(yintercept = 0) +
      labs(x = "Year", y = "GDP Growth", title = paste0("GDP Growth by Country: ", input$range[1], " to ", input$range[2])) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.title.y = element_text(face = "bold", vjust = 2.5), 
            axis.title.x = element_text(face = "bold", vjust = -1), 
            axis.text = element_text(color = "white")) +
      scale_x_continuous(breaks = pretty(input$range[1]:input$range[2], n = 27))
  })
  
  output$dt_GDP <- renderDataTable({
    location_new() %>%
      select(!growth) %>% 
      pivot_wider(names_from = "country", 
                  values_from = "GDP")
  })
  
  output$dt_growth <- renderDataTable({
    location_new() %>%
      select(!GDP) %>%
      pivot_wider(names_from = "country", 
                  values_from = "growth")
  })
}

shinyApp(ui, server)
