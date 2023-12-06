#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)# Include necessary libraries
library(dplyr)
library(janitor)

# Read and preprocess the World Happiness Report dataset
# Perform data manipulations and analysis

#load data
happiness <- read.csv("data/happiness_all_years_df.csv")
#View(data)
happiness_data <- happiness|> select(-X)
#View(data)
data <- clean_names(happiness_data)
#View(data)


# Define UI
ui <- fluidPage(
  titlePanel("Impact of Global Events on Happiness Scores"),
  # Add UI elements (e.g., inputs, plots, etc.)
  # Use sidebarLayout or fluidRow/column to structure the UI
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(data$country)),
      sliderInput("years", "Select Year Range:", min = min(data$year), max = max(data$year),
                  value = c(min(data$year), max(data$year)), step = 1)
    ),
    
    mainPanel(
      plotOutput("line_plot"),
      plotOutput("heatmap")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    data %>%
      filter(country == input$country & year >= input$years[1] & year <= input$years[2])
  })
  
 
  # Line plot for happiness scores over the years
  output$line_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = year, y = happiness_score, group = country, color = country)) +
      geom_line() +
      labs(title = "Happiness Scores Over Years", x = "Year", y = "Happiness Score") +
      theme_minimal()
  })
  
  # Heatmap for correlation matrix
  output$heatmap <- renderPlot({
    corr_matrix <- cor(filtered_data()[, c("happiness_score", "gdp_per_capita", "family", "health_life_expectancy)", 
                                           "freedom", "trust_government_corruption)", "generosity", "dystopia_residual")])
    ggplot() +
      geom_tile(aes(x = colnames(corr_matrix), y = rownames(corr_matrix), fill = corr_matrix), color = "white") +
      scale_fill_gradient2(low = "blue3", high = "pink3", mid = "white", midpoint = 0,
                           limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2),
                           name = "Correlation") +
      theme_minimal() +
      labs(title = "Correlation Matrix of Happiness Factors", x = "Variables", y = "Variables")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
