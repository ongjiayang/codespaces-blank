install.packages("shiny")
install.packages("data.table")
library(shiny)
library(data.table)

# Define UI
ui <- fluidPage(
  titlePanel("Cumulative Paid Claims"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c(".csv")),
      sliderInput("tail_factor", "Tail Factor:", min = 1, max = 2, value = 1.1, step = 0.1)
    ),
    mainPanel(
      plotOutput("cumulative_plot"),
      dataTableOutput("data_table") 
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive function to read data
  data <- reactive({
    req(input$file)  # Require file input
    fread(input$file$datapath)  # Read CSV file
  })
  
  # Reactive function to calculate cumulative triangle
  calculate_cumulative_triangle <- reactive({
    data <- data()  # Get data
    tail_factor <- input$tail_factor  # Get tail factor
    
    # Form the claims triangle into a matrix
    claims_triangle <- matrix(data = NA, 
                              nrow = length(unique(data$LossYear)) + 1L, 
                              ncol = length(unique(data$DevelopmentYear)) + 1L)
    claims_triangle[1,1] <- "Loss Year"
    claims_triangle[-1,1] <- unique(data$LossYear)
    claims_triangle[1,2:ncol(claims_triangle)] <- unique(data$DevelopmentYear)
    
    # Form the data into incremental claims triangle
    for(i in 2:nrow(claims_triangle)) {
      for(j in 2:ncol(claims_triangle)) {
        claims_triangle[i,j] <- sum(data$PaidClaims[data$LossYear == claims_triangle[i,1] & data$DevelopmentYear == claims_triangle[1,j]])
      }
    }
    
    # Transform the incremental claims triangle into data frame
    dataframe <- as.data.frame(claims_triangle)
    dataframe <- dataframe[-1,-1]
    dataframe <- as.data.frame(sapply(dataframe, as.numeric))
    colnames(dataframe) <- claims_triangle[1,-1]
    rownames(dataframe) <- claims_triangle[-1,1]
    
    # Transform the incremental claims triangle into cumulative claims triangle
    cumulative_triangle <- t(apply(dataframe, 1, cumsum))
    
    # Remove the lower right triangle values
    cumulative_triangle[lower.tri(cumulative_triangle)[, ncol(cumulative_triangle):1L]] <- NA
    
    # Calculate development factors for each development year
    development_factors <- data.frame(matrix(NA, nrow = 1, ncol = ncol(cumulative_triangle)))
    for(i in 2:ncol(cumulative_triangle)) {
      development_factors[1,i] <- sum(cumulative_triangle[1:i,i], na.rm = TRUE) / sum(cumulative_triangle[1:i,i-1][!is.na(cumulative_triangle[,i])], na.rm = TRUE)
    }
    
    # Multiply the claims in cumulative triangle to complete the triangle
    for(i in 2:nrow(cumulative_triangle)) {
      for(j in 2:ncol(cumulative_triangle)) {
        if(is.na(cumulative_triangle[i,j])) {
          cumulative_triangle[i,j] <- cumulative_triangle[i,j-1] * development_factors[,j]
        }
      }
    }
    
    # Extend one more development year by using tail factor
    new_development_year <- cumulative_triangle[, ncol(cumulative_triangle)] * tail_factor
    
    # Combine the new development year column with the cumulative claim triangle
    cumulative_triangle <- cbind(cumulative_triangle, new_development_year)
    
    # Rename column name for new development year
    colnames(cumulative_triangle)[ncol(cumulative_triangle)] <- as.numeric(colnames(cumulative_triangle)[ncol(cumulative_triangle)-1]) + 1
    
    # Return the cumulative triangle
    cumulative_triangle
  })
  
  # Render the plot
  output$cumulative_plot <- renderPlot({
    cumulative_triangle <- calculate_cumulative_triangle()
    
    # Plot graph
    plot(1:ncol(cumulative_triangle), cumulative_triangle[1,], type = "l", col = 1, 
         ylim = c(min(cumulative_triangle, na.rm = TRUE), max(cumulative_triangle, na.rm = TRUE)), 
         xlab = "Development Year", ylab = "Claims Amount ($)", main = "Cumulative Paid Claims ($)", xaxt = "n")
    
    # Add integer ticks on the x-axis
    axis(1, at = 1:ncol(cumulative_triangle), labels = colnames(cumulative_triangle))
    
    # Plot additional lines for the other rows
    for (i in 2:nrow(cumulative_triangle)) {
      lines(1:ncol(cumulative_triangle), cumulative_triangle[i,], col = i)
    }

    # Add legend
    legend("topright", legend = rownames(cumulative_triangle), col = 1:nrow(cumulative_triangle), lty = 1)
  })
  
  # Render the data table
  output$data_table <- renderDataTable({
    data()  # Display the data table received from the CSV file
  })
}

# Run the application
shinyApp(ui = ui, server = server)
