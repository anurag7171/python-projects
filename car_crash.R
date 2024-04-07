#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Grouped Bar Chart"),
  
  sidebarLayout(
    sidebarPanel(
      # Input for the dataset
      selectInput("category1", "Select Category 1:", choices = NULL),
      selectInput("category2", "Select Category 2:", choices = NULL),
      selectInput("category3", "Select Category 3:", choices = NULL),
      selectInput("y_variable", "Select Y-axis Category:", choices = NULL),
      selectInput("summary_function", "Select Summary Function:", choices = c("Sum", "Mean", "Count", "Median")),
      textInput("graph_title", "Graph Title:", value = ""),
      textInput("graph_source", "Graph Source:", value = ""),
      
      # Input for axis limits
      numericInput("x_limit_min", "X-axis Minimum:", value = NA),
      numericInput("x_limit_max", "X-axis Maximum:", value = NA),
      numericInput("y_limit_min", "Y-axis Minimum:", value = NA),
      numericInput("y_limit_max", "Y-axis Maximum:", value = NA),
      
      actionButton("plotBtn", "Plot")
    ),
    
    mainPanel(
      # Output for the bar graph
      plotOutput("barPlot")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Read the dataset
  dataset <- reactive({
    read.csv("/Users/anuragrawat/monash_university/5147/dataset/new_crash_data.csv", stringsAsFactors = FALSE)
  })
  
  # Update the category choices based on the dataset
  observe({
    req(dataset())
    
    # Save selected category choices
    category1_selected <- input$category1
    category2_selected <- input$category2
    category3_selected <- input$category3
    y_variable_selected <- input$y_variable
    
    updateSelectInput(session, "category1", choices = colnames(dataset()), selected = category1_selected)
    updateSelectInput(session, "category2", choices = colnames(dataset()), selected = category2_selected)
    updateSelectInput(session, "category3", choices = colnames(dataset()), selected = category3_selected)
    updateSelectInput(session, "y_variable", choices = colnames(dataset()), selected = y_variable_selected)
  })
  
  # Render the bar graph
  output$barPlot <- renderPlot({
    req(input$plotBtn, input$category1, input$category2, input$category3, input$y_variable,
        input$summary_function, dataset())
    
    # Create the grouped bar chart
    grouped_bar_chart <- dataset() %>%
      group_by_at(vars(input$category1, input$category2, input$category3)) %>%
      summarise(value = case_when(
        input$summary_function == "Sum" ~ sum(!!sym(input$y_variable)),
        input$summary_function == "Mean" ~ mean(!!sym(input$y_variable)),
        input$summary_function == "Count" ~ n(),
        input$summary_function == "Median" ~ median(!!sym(input$y_variable))
      )) %>%
      mutate(value = round(value)) %>%  # Round the values to the nearest integer
      ggplot(aes(x = get(input$category1), y = value,
                 fill = interaction(get(input$category2), get(input$category3)))) +
      geom_bar(stat = "identity", position = "dodge", width = 0.8) +
      labs(x = input$category1, y = input$y_variable, title = input$graph_title) +
      theme_minimal() +
      annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -1,
               label = paste("Source:", input$graph_source)) +  # Add the graph source as annotation
      coord_cartesian(xlim = c(input$x_limit_min, input$x_limit_max), ylim = c(input$y_limit_min, input$y_limit_max))  # Set axis limits
    
    # Add labels for highest values
    grouped_bar_chart <- grouped_bar_chart +
      geom_text(aes(label = value, group = interaction(get(input$category2), get(input$category3))),
                position = position_dodge(width = 0.8), vjust = -0.5)
    
    # Render the plot
    print(grouped_bar_chart)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
