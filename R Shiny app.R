# Load necessary libraries
library(shiny)
library(ggplot2)
library(rpart)
library(randomForest)
library(caret)
library(Metrics)

# Comparing the model performance
model_comparison <- function(test_data, linear_regression, decision_tree, random_forest) {
  # Preparing an empty data frame to store results
  results <- data.frame(Model = character(),
                        RMSE = numeric(),
                        MAE = numeric(),
                        stringsAsFactors = FALSE)
  
  # Models storing in a list
  list_models <- list("Linear Regression" = linear_regression,
                 "Decision Tree" = decision_tree,
                 "Random Forest" = random_forest)
  
  # Loop through each model in the 'models' list and get the current model
  for (model_name in names(list_models)) {
    model <- list_models[[model_name]]
    
    # Make predictions on the test data using the current model
    pred_test <- predict(model, test_data)
    
    # Calculate Root Mean Squared Error (RMSE) and Mean Absolute Error between actual and predicted prices
    rmse_val <- rmse(test_data$Price, pred_test)
    mae_val <- mae(test_data$Price, pred_test)
    
    # Append the model results to the 'results' data frame
    results <- rbind(results, data.frame(Model = model_name, RMSE = rmse_val, MAE = mae_val))
  }
  
  return(results)
}


# Load your data
data <- read.csv("Vehicle_Information.csv")

# Set a random seed
set.seed(5555)
# Split the data into training and testing sets (80% train, 20% test)
split_data <- createDataPartition(data$Price, p = .80, list = FALSE, times = 1)
train_data <- data[split_data,]
test_data <- data[-split_data,]

# Training the Linear Regression, Decision Tree and Random Forest models
linear_regression <- lm(Price ~ Age_08_04 + KM + Fuel_Type + Mfg_Year, data = train_data)

decision_tree <- rpart(Price ~ Age_08_04 + KM + Fuel_Type + Mfg_Year, data = train_data)

random_forest <- randomForest(Price ~ Age_08_04 + KM + Fuel_Type + Mfg_Year, data = train_data)

# Define UI
ui <- fluidPage(
  h1("R-Shiny Presentation, Group-7", style = "font-size: 24px;"),
  h3("Predicting the prices for the vehicles", style = "font-size: 16px;"),
  
  tabsetPanel(
    tabPanel("Predictor",
             sidebarLayout(
               sidebarPanel(
                 
                 numericInput("age", "Age(in months)", 20),
                 numericInput("km", "Distance travelled(in kms)", 40000),
                 numericInput("year", "Vehicle Manufactured year", value = 2004, min = 1998),
                 selectInput("fuel", "Choose the type of Fuel", choices = c("Petrol", "Diesel", "CNG")),
                 numericInput('randomseed', label='Set the random seed', value=5555, min = 1, max = 9999),
                 selectInput("modelType", "Choose the type of Model", choices = c("Linear Regression", "Decision Tree", "Random Forest")),
                 actionButton("predict", "Predict Price")
               ),
               mainPanel(
                 tags$style(type="text/css", "#predictedPrice {font-size: 15px; text-align: center; width: 100%;}"),
                 textOutput("predictedPrice")
               )
             )
    ),
    
        tabPanel("Comparing the models",
             fluidRow(
               plotOutput("comparingModel"),
               #textOutput("rmse_val")
             )
    ),
    
    tabPanel("EDA for the selected variables",
             sidebarPanel(
               selectInput("variable", "Select a variable for analysis:",
                           choices = colnames(data), selected = colnames(data)[1]),
               selectInput("x_var", "Select X-axis variable:", choices = colnames(data)),
               selectInput("y_var", "Select Y-axis variable:", choices = colnames(data)),
             ),
             fluidRow(
               plotOutput("plot"),
               plotOutput("bar_plot")
             )
    ),
    
    
    tabPanel("Exploratory Data Analysis(EDA)",
             fluidRow(
               column(6, plotOutput("price_age", height = 400, width = 600)),
               column(6, plotOutput("fuel_count", height = 400, width = 600)),
               column(6, plotOutput("price_km", height = 400, width = 600)),
               #column(6, plotOutput("correlationHeatmap", height = 400, width = 500))
             )
    )
    
  )
)

# Define server logic
server <- function(input, output) {
  # Prediction logic
  pred_event <- eventReactive(input$predict, {
    df <- data.frame(Age_08_04 = input$age,
                          KM = input$km,
                          Fuel_Type = input$fuel,
                     Mfg_Year = input$year)
    
    if (input$modelType == "Linear Regression") {
      return(predict(linear_regression, df))
    } else if (input$modelType == "Decision Tree") {
      return(predict(decision_tree, df))
    } else if (input$modelType == "Random Forest") {
      return(predict(random_forest, df))
    } else {
      return(NA)
    }
  })
  
  output$predictedPrice <- renderText({
    req(input$predict)
    pred <- pred_event()
    if (is.na(pred)) {
      "Invalid Model Selection"
    } else {
      paste("Predicted Price: $", round(pred, 2))
    }
  })
  
  # Reactive data for EDA and model comparison based on inputs
  reactiveData <- reactive({
    data[which(data$Age_08_04 <= input$age & data$KM <= input$km & data$Mfg_Year <= input$year), ]
  })
  
  # EDA Plots
  
  # Create bar plot
  output$bar_plot <- renderPlot({
    ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = paste("Bar Plot of", input$x_var, "vs", input$y_var),
           x = input$x_var, y = input$y_var)
  })
  
  output$plot <- renderPlot({
    ggplot(reactiveData(), aes_string(x = input$variable)) +
      geom_bar(fill = "steelblue") +
      theme_minimal() +
      labs(title = paste("Distribution of", input$variable),
           x = input$variable, y = "Count")
  })
  
  output$price_age <- renderPlot({
    ggplot(reactiveData(), aes(x = Age_08_04, y = Price)) +
      geom_line(color = "darkgreen") +
      geom_point(color = "orange") +
      theme_minimal() +
      labs(title = "Price Distribution by Age Group", x = "Months(age)", y = "Vehicle Price")
  })
  
  
  
  # Calculate the correlation matrix
  correlation_matrix <- cor(data[, c("Price", "Age_08_04", "KM", "HP")])
  
  # Plot the correlation heatmap
  output$correlationHeatmap <- renderPlot({
    corrplot::corrplot(correlation_matrix, method = "color", type = "lower", order = "hclust", tl.cex = 0.8, tl.col = "black", tl.srt = 40, addCoef.col = "black", number.cex = 0.6, 
                       title = "Correlation Heatmap")
  })
  
  
  output$price_km <- renderPlot({
    ggplot(reactiveData(), aes(x = KM, y = Price)) +
      geom_point(alpha = 0.5, color = "blue") +
      geom_smooth(method = "lm", color = "red", fill = "lightblue", alpha = 0.2) +
      theme_minimal() +
      labs(title = "Price distributed by Kilometers Driven", x = "Distance travelled(in kms)", y = "Vehicle Price")
  })
  
  
  
  output$fuel_count <- renderPlot({
    ggplot(reactiveData(), aes(x = Fuel_Type)) +
      geom_bar(fill = "steelblue") +
      theme_minimal() +
      labs(title = "Distribution of Fuel Types", x = "Fuel Type", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  })
  
  
  # Comparing the models
  output$comparingModel <- renderPlot({
    data_compare <- model_comparison(reactiveData(), linear_regression, decision_tree, random_forest)
    
    # Print RMSE and MAE values
    #print(data_compare)
    
    ggplot(data_compare, aes(x = Model)) +
      geom_bar(aes(y = RMSE), stat = "identity", fill = "coral", alpha = 0.8, position = position_dodge(width = 0.9)) +
      geom_bar(aes(y = MAE), stat = "identity", fill = "steelblue", alpha = 0.8, position = position_dodge(width = 0.9)) +
      theme_minimal() +
      labs(title = "Comparison between three models by checking Root Mean Squared Error (RMSE) and Mean Absolute Error(MAE)", x = "Models", y = "Metric Value") +
      scale_y_continuous(sec.axis = sec_axis(~., name = "MAE (steelblue) / RMSE (coral)"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
