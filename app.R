# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)

# Read the dataset
auschwitz_data <- read.csv("data.csv")

# Define UI for application
ui <- fluidPage(
  titlePanel("Holocaust Victims at Auschwitz"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select Category:",
                  choices = c("Religion", "Birthplace", "Residence")),
      uiOutput("groupInput")
    ),
    mainPanel(
      plotOutput("victim_plot"),
      dataTableOutput("victim_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Dynamic input options based on category selection
  output$groupInput <- renderUI({
    if (input$category == "Religion") {
      selectInput("group", "Select Religion:",
                  choices = unique(auschwitz_data$Religion),
                  selected = NULL, multiple = TRUE)
    } else if (input$category == "Birthplace") {
      selectInput("group", "Select Birthplace:",
                  choices = unique(auschwitz_data$Birthplace),
                  selected = NULL, multiple = TRUE)
    } else if (input$category == "Residence") {
      selectInput("group", "Select Residence:",
                  choices = unique(auschwitz_data$Residence),
                  selected = NULL, multiple = TRUE)
    }
  })
  
  # Filter data based on user selection
  filtered_data <- reactive({
    req(input$group)
    if (input$category == "Religion") {
      auschwitz_data %>%
        filter(Religion %in% input$group)
    } else if (input$category == "Birthplace") {
      auschwitz_data %>%
        filter(Birthplace %in% input$group)
    } else if (input$category == "Residence") {
      auschwitz_data %>%
        filter(Residence %in% input$group)
    }
  })
  
  # Render the interactive graph
  output$victim_plot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$category, fill = input$category)) +
      geom_bar() +
      labs(title = paste("Number of Holocaust Victims at Auschwitz by", input$category),
           x = input$category, y = "Number of Victims") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the interactive table
  output$victim_table <- renderDataTable({
    filtered_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

           
