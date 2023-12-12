library(shiny)
library(markdown)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)
library(here)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)



# Load datasets for each year
happiness2015 <- read.csv(here("data", "happiness_2015_clean.csv"), stringsAsFactors = TRUE)
happiness2016 <- read.csv(here("data", "happiness_2016_clean.csv"), stringsAsFactors = TRUE)
happiness2017 <- read.csv(here("data", "happiness_2017_clean.csv"), stringsAsFactors = TRUE)
happiness2018 <- read.csv(here("data", "happiness_2018_clean.csv"), stringsAsFactors = TRUE)
happiness2019 <- read.csv(here("data", "happiness_2019_clean.csv"), stringsAsFactors = TRUE)
happiness2020 <- read.csv(here("data", "happiness_2020_clean.csv"), stringsAsFactors = TRUE)
happiness2021 <- read.csv(here("data", "happiness_2021_clean.csv"), stringsAsFactors = TRUE)
happiness2022 <- read.csv(here("data", "happiness_2022_clean.csv"), stringsAsFactors = TRUE)
happiness2023 <- read.csv(here("data", "happiness_2023_clean.csv"), stringsAsFactors = TRUE)
world <- ne_countries(scale = "medium", returnclass = "sf")
countries_all <- read.csv(here("data", "countries_aggregated.csv"), stringsAsFactors = TRUE)


# Define UI
ui <- navbarPage(
  theme = shinytheme("cyborg"),
  title = "World Happiness Analysis",
  tabPanel("Plot",
           sidebarPanel(width = 3,
                        br(),
                        span("This app will help you explore the data behind the ", 
                             tags$a("World Happiness Report.", href = "http://worldhappiness.report")),
                        br(), hr(),
                        helpText("Select year"),
                        actionButton("button2015", "2015"),
                        actionButton("button2016", "2016"),
                        actionButton("button2017", "2017"),
                        actionButton("button2018", "2018"),
                        actionButton("button2019", "2019"),
                        actionButton("button2020", "2020"),
                        actionButton("button2021", "2021"),
                        actionButton("button2022", "2022"),
                        actionButton("button2023", "2023"),
                        br(), br(), 
                        helpText("Choose variables you want to see on the plot"),
                        uiOutput("variable_1"),
                        uiOutput("variable_2"),
                        br(),
                        helpText("Click while pressing `command/ctrl` in order to choose multiple countries"),
                        sliderInput("num_countries", "Number of Countries:",
                                    min = 1, max = length(countries_all$Country), value = 10),
                        uiOutput("country"),
                        br(), hr(),
                        span("Data source:", 
                             tags$a("Kaggle",
                                    href = "https://www.kaggle.com/unsdsn/world-happiness")),
                        br(), br(),
                        em(
                          span("Created by", a(href = "https://github.com/INFO526-DataViz/project-final-Plot-Pioneers/tree/main", "Plot Pioneers")),
                          br(),
                          span("Code", a(href = "https://github.com/INFO526-DataViz/project-final-Plot-Pioneers/tree/main", "on GitHub"))
                        )
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Plot",
                        br(),
                        h2(textOutput("year_name_plot")),
                        plotlyOutput("scatterplot", height = 800)
               ),
               tabPanel("Trend Comparison",
                        fluidRow(
                          column(6, plotlyOutput("timeSeriesPlot1", height = 600, width = 465)),
                          column(6, plotlyOutput("timeSeriesPlot2", height = 600, width = 465))
                        )
               ),
               tabPanel("Table", 
                        br(),
                        DT::dataTableOutput("results_table")
               ),
               tabPanel(textOutput("tab_name_rankings"),
                        h1(textOutput("year_name")),
                        h2("Top 10"),
                        wellPanel(dataTableOutput("rank_table_top")),
                        br(), 
                        h2("Bottom 10"),
                        wellPanel(dataTableOutput("rank_table_bottom"))
               ),
               tabPanel("Happiness Trends",
                        h2("Happiness Rank Time Series"),
                        p("This plot shows the changes in happiness ranks of various countries over years."),
                        plotlyOutput("covid_timeseries_plot", height = 800) 
               ),
               tabPanel("World Heatmap",
                        h2("Happiness Rank Heatmap"),
                        plotlyOutput("world_heatmap", height = 800)
               )
             )
           )),
  tabPanel("About", includeMarkdown(here("about.qmd")))
)


# Define server logic
server <- function(input, output, session) {
  # UI for selecting variables
  output$variable_1 <- renderUI({
    selectInput("variable_1", "Y-variable:", 
                choices = c("Happiness.Rank", "Happiness.Score", "Economy", 
                            "Family", "Health", "Freedom", "Government.Corruption", 
                            "Generosity", "Dystopia.Residual"), selected = "Happiness.Score")
  })
  
  output$variable_2 <- renderUI({
    selectInput("variable_2", "X-variable:", 
                choices = c("Happiness.Rank", "Happiness.Score", "Economy", 
                            "Family", "Health", "Freedom", "Government.Corruption", 
                            "Generosity", "Dystopia.Residual"), selected = "Economy")
  })
  
  # Reactive values for selected year's data
  data_year <- reactiveValues(data = happiness2015, year = "2015")
  
  # Observers for each year button
  observeEvent(input$button2015, {
    data_year$data <- happiness2015
    data_year$year <- "2015"
  })
  
  observeEvent(input$button2016, {
    showModal(modalDialog(
      title = "Loading",
      "Please wait while the data is being loaded...",
      easyClose = TRUE,
      footer = NULL
    ))
    
    data_year$data <- happiness2016
    data_year$year <- "2016"
  })
  
  observeEvent(input$button2017, {
    
    showModal(modalDialog(
      title = "Loading",
      "Please wait while the data is being loaded...",
      easyClose = TRUE,
      footer = NULL
    ))
    data_year$data <- happiness2017
    data_year$year <- "2017"
  })
  
  observeEvent(input$button2018, {
    
    showModal(modalDialog(
      title = "Loading",
      "Please wait while the data is being loaded...",
      easyClose = TRUE,
      footer = NULL
    ))
    
    data_year$data <- happiness2018
    data_year$year <- "2018"
  })
  
  observeEvent(input$button2019, {
    
    showModal(modalDialog(
      title = "Loading",
      "Please wait while the data is being loaded...",
      easyClose = TRUE,
      footer = NULL
    ))
    
    data_year$data <- happiness2019
    data_year$year <- "2019"
  })
  
  observeEvent(input$button2020, {
    
    showModal(modalDialog(
      title = "Loading",
      "Please wait while the data is being loaded...",
      easyClose = TRUE,
      footer = NULL
    ))
    
    data_year$data <- happiness2020
    data_year$year <- "2020"
  })
  
  observeEvent(input$button2021, {
    
    showModal(modalDialog(
      title = "Loading",
      "Please wait while the data is being loaded...",
      easyClose = TRUE,
      footer = NULL
    ))
    
    data_year$data <- happiness2021
    data_year$year <- "2021"
  })
  
  observeEvent(input$button2022, {
    
    showModal(modalDialog(
      title = "Loading",
      "Please wait while the data is being loaded...",
      easyClose = TRUE,
      footer = NULL
    ))
    
    data_year$data <- happiness2022
    data_year$year <- "2022"
  })
  
  observeEvent(input$button2023, {
    
    showModal(modalDialog(
      title = "Loading",
      "Please wait while the data is being loaded...",
      easyClose = TRUE,
      footer = NULL
    ))
    
    data_year$data <- happiness2023
    data_year$year <- "2023"
  })
  
  filtered_data <- reactive({
    data_year$data %>% 
      filter(Country %in% c(input$country)) %>% 
      arrange(Country) 
  })
  
  
  output$year_name_plot <- renderText({ 
    paste("Data for ", data_year$year) 
  })
  
  
  countries <- reactive({ 
    df_small <- 
      countries_all %>% 
      select(Country) %>% 
      droplevels()
    
    c("Select All", levels(df_small$Country))
    
  })
  
  
  output$country <- renderUI ({
    
    selectInput("country", "Countries:", 
                multiple = TRUE,
                choices = countries(), 
                selected = "Select All",
                selectize = FALSE,
                size = 10)
  })
  
  
  observe({
    if ("Select All" %in% input$country) {
      # choose all the choices _except_ "Select All"
      selected_choices <- setdiff(countries(), "Select All")
      updateSelectInput(session, "country", selected = selected_choices)
    }
  })
  
  
  output$scatterplot <- renderPlotly({
    if (is.null(data_year$data)) return()
    
    p <- ggplot(filtered_data()) +
      geom_point(aes_string(x = input$variable_2, y = input$variable_1, 
                            colour = "Region", label = "Country"), size = 3) +
      geom_smooth(aes_string(x = input$variable_2, y = input$variable_1), method = "lm", color = "black", se = FALSE) +
      ggtitle(paste0(input$variable_1, " vs. ", input$variable_2)) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("label", "x", "y"), height = 500) 
  })
  
  
  output$results_table <- DT::renderDataTable({
    if (is.null(data_year$data)) {
      return()
    } else {
      filtered_data()
    }
  },
  options = list(lengthChange = FALSE, 
                 scrollCollapse = TRUE,
                 scrollX = "100%"),
  selection = "single"
  )
  
  
  output$tab_name_rankings <- renderText({ 
    paste("Happiness Ranking: ", data_year$year) 
  })
  
  
  output$year_name <- renderText({ 
    paste("Happiness Ranking in ", data_year$year) 
  })
  
  
  output$rank_table_top <- DT::renderDataTable({
    if (is.null(data_year$data)) {
      return()
    } else {
      data_year$data %>% 
        mutate(Rank = Happiness.Rank) %>% 
        arrange(Rank) %>% 
        top_n(10, desc(Rank))
    }
  },
  options = list(lengthChange = FALSE,
                 scrollX = "100%")
  )
  
  
  output$rank_table_bottom <- DT::renderDataTable({
    if (is.null(data_year$data)) {
      return()
    } else {
      data_year$data %>% 
        mutate(Rank = Happiness.Rank) %>% 
        arrange(Rank) %>% 
        top_n(10, Rank)
    }
  },
  
  options = list(lengthChange = FALSE, 
                 scrollX = "100%")
  )
  
  covid_years_data <- bind_rows(
    happiness2015 %>% mutate(Year = 2015),
    happiness2016 %>% mutate(Year = 2016),
    happiness2017 %>% mutate(Year = 2017),
    happiness2018%>% mutate(Year = 2018),
    happiness2019%>% mutate(Year = 2019),
    happiness2020 %>% mutate(Year = 2020),
    happiness2021 %>% mutate(Year = 2021),
    happiness2022 %>% mutate(Year = 2022),
    happiness2023 %>% mutate(Year = 2023)
  )
  
  output$covid_timeseries_plot <- renderPlotly({
    filtered_covid_data <- covid_years_data %>%
      filter(Country %in% input$country) %>%
      arrange(Year, Country) %>%
      group_by(Year) %>%
      slice_head(n = input$num_countries) %>%
      ungroup() %>%
      mutate(Emoji = case_when(
        Happiness.Score > 6 ~ "\U0001F600", # Smiling face
        Happiness.Score > 4 ~ "\U0001F642", # Slight smile face
        TRUE ~ "\U0001F641"  # Sad face
      ))
    
    
    # Check if "Select All" is chosen and set the title accordingly
    if ("Select All" %in% input$country || length(input$country) == length(countries_all$Country)) {
      plot_title <- "Happiness Rank Trends"
    } else {
      selected_countries <- paste(input$country, collapse = ", ")
      plot_title <- paste("Happiness Rank Trends for", selected_countries)
    }
    
    
    #time series plot
    plot_ly(data = filtered_covid_data, x = ~Year, y = ~Happiness.Rank, color = ~Region, type = 'scatter', mode = 'lines+markers', hoverinfo = 'text',
            text = ~paste("Country:", Country, "<br>Year:", Year, "<br>Happiness Rank:", Happiness.Rank, Emoji)) %>%
      layout(title = list(text = plot_title),
             margin = list(t = 50),
             xaxis = list(title = 'Year', tickvals = c('2015','2016','2017','2018','2019','2020', '2021', '2022', '2023'), 
                          ticktext = c('2015','2016','2017','2018','2019','2020', '2021', '2022', '2023')),
             yaxis = list(title = 'Happiness Score'),
             hovermode = 'closest')
  })
  
  world_happiness_data <- reactive({
    selected_year_data <- switch(data_year$year,
                                 "2015" = happiness2015,
                                 "2016" = happiness2016,
                                 "2017" = happiness2017,
                                 "2018" = happiness2018,
                                 "2019" = happiness2019,
                                 "2020" = happiness2020,
                                 "2021" = happiness2021,
                                 "2022" = happiness2022,
                                 "2023" = happiness2023)
    happiness_data_with_emoji <- selected_year_data %>%
      mutate(Emoji = case_when(
        Happiness.Score > 6 ~ "\U0001F600", # Smiling face
        Happiness.Score > 4 ~ "\U0001F642", # Slight smile face
        TRUE ~ "\U0001F641"  # Sad face
      ))
    
    
    world %>% 
      left_join(happiness_data_with_emoji, by = c("name" = "Country"))
  })
  
  # Render the heatmap
  output$world_heatmap <- renderPlotly({
    ggplot_data <- world_happiness_data()
    
    # Create ggplot object with specified hover information
    p <- ggplot(data = ggplot_data) +
      geom_sf(aes(fill = Happiness.Score, geometry = geometry, text = paste("Country:", name, "<br>Happiness Score:", Happiness.Score, "<br>Happiness Rank:", Happiness.Rank,"<br>", Emoji)), color = NA) + 
      scale_fill_viridis_c() +
      theme_minimal() +
      labs(fill = "Happiness Score", title = paste("World Happiness Map", data_year$year))
    
    # Convert to plotly and customize hover information
    ggplotly(p, tooltip = "text")
  })
  
  output$timeSeriesPlot1 <- renderPlotly({
    # Plot for the selected Y-variable
    selected_var <- input$variable_1  
    
    plot_data <- covid_years_data %>%
      filter(Country %in% input$country) %>%
      select(Year, Country, !!sym(selected_var), Happiness.Rank)
    
    p <- ggplot(plot_data, aes(x = Year, y = !!sym(selected_var), color = Country)) +
      geom_line() +
      geom_point(aes(text = paste("Year:", Year, "<br>",
                                  "Country:", Country, "<br>",
                                  selected_var, ":", !!sym(selected_var), "<br>",
                                  "Happiness Rank:", Happiness.Rank))) +
      scale_x_continuous(breaks = unique(plot_data$Year)) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = paste("Time Series of", selected_var))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$timeSeriesPlot2 <- renderPlotly({
    # Plot for the happiness rank
    plot_data <- covid_years_data %>%
      filter(Country %in% input$country) %>%
      select(Year, Country, Happiness.Score, Happiness.Rank)
    
    p <- ggplot(plot_data, aes(x = Year, y = Happiness.Score, color = Country)) +
      geom_line() +
      geom_point(aes(text = paste("Year:", Year, "<br>",
                                  "Country:", Country, "<br>",
                                  "Happiness Score:", Happiness.Score, "<br>",
                                  "Happiness Rank:", Happiness.Rank))) +
      scale_x_continuous(breaks = unique(plot_data$Year)) + 
      theme_minimal() + 
      theme(legend.position = "none") +
      labs(title = "Time Series of Happiness Score")
    
    ggplotly(p, tooltip = "text")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

