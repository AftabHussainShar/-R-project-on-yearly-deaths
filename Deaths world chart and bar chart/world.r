library(shiny)
library(plotly)
library(dplyr)

data <- read.csv("C:/Users/Aftab Hussain/Desktop/test/country_wise.csv")

# Create a new column for total deaths
data <- data %>% mutate(TotalDeaths = rowSums(.[3:12], na.rm = TRUE))

# Shiny app UI
ui <- fluidPage(
  titlePanel("World Map - Category-wise Deaths"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year", choices = unique(data$Year)),
      width = 2
    ),
    mainPanel(
      plotlyOutput("worldmap"),
      width = 10
    )
  )
)

# Shiny app server
server <- function(input, output) {
  filteredData <- reactive({
    subset(data, Year == input$year)
  })
  
  output$worldmap <- renderPlotly({
    column_names <- colnames(filteredData())[3:12]
    short_column_names <- c("Drowning", "Interpersonal violence", "Drug use disorders", "Self-harm",
                            "Exposure to forces of nature", "Environmental heat and cold exposure",
                            "Conflict and terrorism", "Poisonings", "Road injuries", "Fire substances")
    
    text_info <- lapply(1:nrow(filteredData()), function(i) {
      paste(
        "<b>Country:</b>", filteredData()$Entity[i],
        "<br><b>Total Deaths:</b>", filteredData()$TotalDeaths[i],
        "<br><b>Total GDP:</b>", filteredData()$GDP[i],
        "<br><b>Total Population:</b>", filteredData()$Population[i],
        "<br><b>Category-wise Deaths:</b><br>",
        paste(short_column_names, ": ", filteredData()[i, 3:14], "<br>", sep = "", collapse = "")
      )
    })
    
    plot_ly(
      data = filteredData(),
      type = "choropleth",
      locations = ~Entity,
      locationmode = "country names",
      z = ~TotalDeaths,
      colorscale = "Reds",
      text = text_info,
      hoverinfo = "text"
    ) %>% layout(
      title = "World Map - Category-wise Deaths",
      geo = list(showframe = FALSE, showcoastlines = FALSE, projection = list(type = "natural earth")),
      margin = list(l = 0, r = 0, t = 50, b = 0)
    )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
