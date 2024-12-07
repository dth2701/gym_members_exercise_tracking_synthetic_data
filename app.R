# app.R
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(cluster)
library(readr)

# Read the data and handle NA values
gym_data <- read_csv("gym_members_exercise_tracking_synthetic_data.csv") %>%
  # Remove rows with any NA values
  na.omit() %>%
  # Replace infinite values with NA and then remove those rows
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.), NA, .))) %>%
  na.omit()

# UI portion of the app
ui <- fluidPage(
  titlePanel("Gym Members Clustering Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Input controls
      sliderInput("clusters", "Number of Clusters:",
                  min = 2, max = 6, value = 3),
      selectInput("vars", "Variables for Clustering:",
                  choices = c("Age", "Weight (kg)", "Height (m)", "BMI", 
                              "Fat_Percentage", "Workout_Frequency (days/week)"),
                  multiple = TRUE,
                  selected = c("Age", "BMI"))
    ),
    
    mainPanel(
      # Output plots
      plotOutput("structurePlot", height = "400px"),
      plotOutput("membershipPlot", height = "400px"),
      
      # Text output for analysis
      h3("Cluster Analysis"),
      verbatimTextOutput("clusterSummary")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Reactive clustering function
  clusters <- reactive({
    req(input$vars)
    validate(
      need(length(input$vars) >= 2, "Please select at least 2 variables for clustering")
    )
    
    # Select variables for clustering and handle any remaining NA/Inf values
    cluster_data <- gym_data %>%
      select(all_of(input$vars)) %>%
      mutate(across(everything(), ~as.numeric(as.character(.)))) %>%  # Ensure numeric
      na.omit()  # Remove any remaining NA values
    
    # Check if we have valid data for clustering
    validate(
      need(nrow(cluster_data) > 0, "No valid data for clustering after removing NA/Inf values"),
      need(all(sapply(cluster_data, function(x) length(unique(x)) > 1)), 
           "Each variable must have at least 2 unique values")
    )
    
    # Scale the data
    scaled_data <- scale(cluster_data)
    
    # Perform k-means clustering with error handling
    tryCatch({
      kmeans(scaled_data, centers = input$clusters)
    }, error = function(e) {
      validate(need(FALSE, paste("Clustering error:", e$message)))
    })
  })
  
  # Structure plot
  output$structurePlot <- renderPlot({
    cluster_results <- clusters()
    
    # Convert cluster probabilities to long format
    cluster_probs <- cluster_results$cluster %>%
      as.factor() %>%
      table() %>%
      prop.table()
    
    prob_df <- data.frame(
      Cluster = factor(names(cluster_probs)),
      Probability = as.numeric(cluster_probs)
    )
    
    # Create structure plot
    ggplot(prob_df, aes(x = 1, y = Probability, fill = Cluster)) +
      geom_col(position = "stack", width = 0.5) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank()) +
      labs(title = "Cluster Structure Plot",
           y = "Proportion of Members") +
      scale_y_continuous(labels = scales::percent)
  })
  
  # Membership distribution boxplot
  output$membershipPlot <- renderPlot({
    cluster_results <- clusters()
    
    # Create data frame with cluster assignments and selected variables
    plot_data <- gym_data %>%
      select(all_of(input$vars)) %>%
      mutate(Cluster = as.factor(cluster_results$cluster)) %>%
      gather(Variable, Value, -Cluster)
    
    # Create jittered boxplot
    ggplot(plot_data, aes(x = Cluster, y = Value, fill = Cluster)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.3) +
      facet_wrap(~Variable, scales = "free_y") +
      theme_minimal() +
      labs(title = "Distribution of Variables by Cluster",
           y = "Value") +
      theme(axis.text.x = element_text(angle = 0))
  })
  
  # Cluster summary statistics
  output$clusterSummary <- renderPrint({
    cluster_results <- clusters()
    
    # Add cluster assignments to data and calculate summaries
    analysis_data <- gym_data %>%
      mutate(Cluster = cluster_results$cluster)
    
    analysis_data %>%
      group_by(Cluster) %>%
      summarise(
        n = n(),
        across(all_of(input$vars), 
               list(mean = ~mean(., na.rm = TRUE),
                    sd = ~sd(., na.rm = TRUE)))
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)