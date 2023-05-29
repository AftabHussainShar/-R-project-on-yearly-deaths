library(shiny)
library(ggplot2)
library(tidyr)
library(plotly)

# Read the data
data <- read.csv("C:/Users/Aftab Hussain/Desktop/test/country_wise.csv")

# Set options to display numbers without scientific notation
options(scipen = 999)

# Convert GDP values from scientific notation to character format without scientific notation
gdp_values <- format(filtered_data$GDP, scientific = FALSE)

# Convert GDP values from character to numeric format
gdp_values <- as.numeric(gdp_values)

population_values <- filtered_data$Population

# Set the maximum size for the bubbles
max_size <- 100

# Calculate the adjusted bubble sizes
gdp_size_adjusted <- gdp_size * max_size / max(gdp_values)
population_size_adjusted <- population_size * max_size / max(population_values)


# Define the UI
ui <- fluidPage(
  titlePanel("Deaths by Category"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(data$Entity)),
      selectInput("year", "Select Year:", choices = unique(data$Year)),
      textOutput("gdp_output"),
      textOutput("population_output")
    ),
    mainPanel(
      plotOutput("bar_plot"),
      plotlyOutput("bubble_plot")
    )
  )
)

# Define the server
server <- function(input, output) {
  # Filter the data based on selected country and year
  filtered_data <- reactive({
    subset(data, Entity == input$country & Year == input$year)
  })
  
  # Perform data transformation and plot the bar chart
  output$bar_plot <- renderPlot({
    country_data <- filtered_data()
    
    columns_of_interest <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)  # Column indices of the columns of interest
    
    column_names <- colnames(country_data)[columns_of_interest]  # Get the column names based on the indices
    
    short_column_names <- c("Drowning", "Interpersonal violence", "Drug use disorders", "Self-harm",
                            "Exposure to forces of nature", "Environmental heat and cold exposure",
                            "Conflict and terrorism", "Poisonings", "Road injuries", "Fire substances")
    
    country_data_long <- gather(country_data, key = "Category", value = "Number of Deaths", columns_of_interest)
    
    country_data_long$Category <- factor(country_data_long$Category,
                                         levels = column_names,
                                         labels = short_column_names)
    
    p <- ggplot(country_data_long, aes(x = Category, y = `Number of Deaths`, fill = Category)) +
      geom_bar(stat = "identity") +
      labs(x = "Categories", y = "Number of Deaths", title = paste("Deaths in", input$country, input$year)) +
      theme_minimal() +
      theme(axis.text.x = element_blank())
    
    print(p)
  })
  
  # Perform data transformation for bubble chart and plot using plotly
  output$bubble_plot <- renderPlotly({
    country_data <- filtered_data()
    gdp <- country_data$GDP
    population <- country_data$Population
    
    # Calculate the range of GDP and Population values from the filtered data
    gdp_range <- range(data$GDP)
    population_range <- range(data$Population)
    
    # Calculate the relative size of the bubbles based on the GDP and Population values
    gdp_size <- (gdp - gdp_range[1]) / (gdp_range[2] - gdp_range[1])
    population_size <- (population - population_range[1]) / (population_range[2] - population_range[1])
    
    # Create data for the two bubbles
    bubble_df <- data.frame(
      label = c("GDP", "Population"),
      size = c(gdp_size_adjusted, population_size_adjusted),
      position = c(-0.2, 0.2)  # Adjusted positioning to center the bubbles
    )
    
    # Define the colors and labels for the bubbles
    bubble_colors <- c("blue", "red")
    bubble_labels <- c("GDP", "Population")
    
    p <- ggplot(bubble_df, aes(x = position, y = 1, size = size, fill = label,
                               text = ifelse(label == "GDP",
                                             paste("GDP:", format(gdp, big.mark = ",")),
                                             paste("Population:", format(population, big.mark = ","))))) +
      geom_point(shape = 21, color = "black", alpha = 0.8) +
      scale_x_continuous(limits = c(-1, 1), expand = c(0, 0), breaks = NULL) +
      scale_y_continuous(limits = c(0, 2), expand = c(0, 0), breaks = NULL, labels = NULL) +
      theme_void() +
      guides(size = FALSE, fill = FALSE) +
      theme(plot.background = element_rect(fill = "white")) +
      labs(title = "Bubble Chart: GDP and Population") +
      scale_fill_manual(values = bubble_colors, labels = bubble_labels)
    
    # Set the bubble size range
    p <- p + scale_size(range = c(10, 20))
    
    # Convert the bubble chart to a plotly object
    p <- ggplotly(p, tooltip = "text")
    
    # Remove the arrow annotations
    p$x$data[[2]]$xend <- NULL
    p$x$data[[2]]$yend <- NULL
    
   
    # Add custom legend annotation and legend boxes
    p <- p %>% layout(
      annotations = list(
        list(
          x = 0.9,
          y = 1.05,
          xref = "paper",
          yref = "paper",
          text = "GDP",
          showarrow = FALSE,
          font = list(size = 10, color = "blue")
        ),
        list(
          x = 0.9,
          y = 0.95,
          xref = "paper",
          yref = "paper",
          text = "Population",
          showarrow = FALSE,
          font = list(size = 10, color = "red")
        )
      ),
      legend = list(
        x = 1.1,
        y = 1.02,
        bgcolor = "white",
        bordercolor = "black",
        borderwidth = 1,
        itemsizing = "constant",
        itemwidth = 80,
        itemheight = 20,
        title = list(
          text = "",
          font = list(size = 12)
        ),
        tracegroupgap = 5,
        traceorder = "normal",
        orientation = "h"
      )
    )
  })
  
  # Display GDP value
  output$gdp_output <- renderText({
    country_data <- filtered_data()
    gdp_value <- country_data$GDP
    paste("GDP:", format(gdp_value, big.mark = ","))
  })
  
  # Display Population value
  output$population_output <- renderText({
    country_data <- filtered_data()
    population_value <- country_data$Population
    paste("Population:", format(population_value, big.mark = ","))
  })
}

# Run the Shiny app
shinyApp(ui, server)
