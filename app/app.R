if(!require(shiny)){install.packages('shiny', dependencies = TRUE)}
if(!require(DT)){install.packages('DT', dependencies = TRUE)}
if(!require(ggplot2)){install.packages('ggplot2', dependencies = TRUE)}
if(!require(dplyr)){install.packages('dplyr', dependencies = TRUE)}
library(shiny)
library(dplyr)
library(ggplot2)

# Read the dataset
auschwitz_data <- read.csv("data.csv")
auschwitz_data 
# Define UI for application
ui <- fluidPage(
  titlePanel("Holocaust Victims at Auschwitz"),
  sidebarLayout(
    sidebarPanel(
      selectInput("nationality", "Select Nationality/Category:",
                  choices = unique(auschwitz_data$Religion),
                  selected = NULL, multiple = TRUE)
    ),
    mainPanel(
      plotOutput("victim_plot"),
      dataTableOutput("victim_table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data based on user selection
  filtered_data <- reactive({
    req(input$nationality)
    auschwitz_data %>%
      filter(Religion %in% input$nationality)
  })
  
  # Render the interactive graph
  output$victim_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Religion, fill = Religion)) +
      geom_bar() +
      labs(title = "Number of Holocaust Victims at Auschwitz by Religion",
           x = "Religion", y = "Number of Victims") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the interactive table
  output$victim_table <- renderDataTable({
    filtered_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

