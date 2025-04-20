library(shiny)
library(plotly)
library(rpart)

# Load model and support files
tree1 <- readRDS("tree1_verified.rds")
tree2 <- readRDS("tree2_verified.rds")
combined_tree <- readRDS("combined_tree_verified.rds")


borough_avg_price <- readRDS("borough_avg_price.rds")
borough_avg_footage <- readRDS("borough_avg_footage.rds")
borough_avg_review <- readRDS("borough_avg_review.rds")

value_quantiles <- readRDS("value_quantiles.rds")
price_quantiles <- readRDS("price_quantiles.rds")
footage_quantiles <- readRDS("footage_quantiles.rds")
review_quantiles <- readRDS("review_quantiles.rds")
review_weight_median <- readRDS("review_weight_median.rds")

price_levels <- readRDS("price_levels.rds")
footage_levels <- readRDS("footage_levels.rds")
review_levels <- readRDS("review_levels.rds")
floor_levels <- readRDS("floor_levels.rds")

# Load test data and prediction output
test_data <- read.csv("Test.csv")
predictions <- read.csv("result_airbnb.csv")
borough_data <- read.csv("localities_NYC.csv")
test_data <- merge(test_data, borough_data, by.x = "neighbourhood", by.y = "neighborhood", all.x = TRUE)
test_data$Deal <- predictions$Deal

neighbourhoods <- unique(trimws(as.character(test_data$neighbourhood)))
boroughs <- c("Brooklyn", "Manhattan", "Queens", "Staten Island", "Bronx")

# ------------------------------
# UI
ui <- fluidPage(
  titlePanel("Airbnb Deal Predictor (Real-Time)"),
  
  fluidRow(
    column(4,
           wellPanel(
             selectInput("neighbourhood", "Neighbourhood", choices = neighbourhoods),
             selectInput("borough", "Borough", choices = boroughs),
             numericInput("price", "Price", value = 100, min = 10, max = 1000),
             numericInput("footage", "Footage", value = 250, min = 50, max = 1000),
             numericInput("avgreview", "Average Review (0-5)", value = 4.2, min = 0, max = 5, step = 0.1),
             numericInput("floor", "Floor", value = 3, min = 0, max = 50),
             actionButton("predict", "Predict Deal", class = "btn-primary")
           )
    ),
    
    column(8,
           h3("Prediction Result"),
           uiOutput("prediction_result"),
           br(),
           h4("Top 3 Similar Listings"),
           tableOutput("similar_listings"),
           br(),
           h4("Analytics"),
           wellPanel(
             sliderInput("price_range", "Filter by Price Range ($)",
                         min = 10, max = 1000, value = c(50, 300), step = 10),
             plotlyOutput("review_vs_price")
           )
    )
  )
)

# ------------------------------
# Server
server <- function(input, output) {
  observeEvent(input$predict, {
    # Capture user input
    df <- data.frame(
      neighbourhood = trimws(as.character(input$neighbourhood)),
      borough = trimws(as.character(input$borough)),
      price = input$price,
      footage = input$footage,
      avgreview = input$avgreview,
      floor = input$floor
    )
    # Force correct borough from mapping file (to ensure consistency with model training)
    borough_map <- read.csv("localities_NYC.csv")
    user_borough <- borough_map$borough[match(df$neighbourhood, borough_map$neighborhood)]
    
    # Overwrite borough to ensure exact match with model training
    df$borough <- user_borough
    
    
    # Feature Engineering (same as PredictionModel.R)
    df$price_per_foot <- df$price / df$footage
    df$value_score <- df$footage * df$avgreview / df$price
    df$is_manhattan <- df$borough == "Manhattan"
    df$high_floor <- df$floor > 10
    df$price_review_norm <- df$price / (df$avgreview + 1)
    df$log_price <- log(df$price)
    df$log_footage <- log(df$footage)
    df$price_sqft <- df$price / sqrt(df$footage)
    df$review_weight <- df$avgreview^2
    
    df$price_vs_borough <- df$price / borough_avg_price[[df$borough]]
    df$footage_vs_borough <- df$footage / borough_avg_footage[[df$borough]]
    df$review_vs_borough <- df$avgreview / borough_avg_review[[df$borough]]
    
    # Categoricals (with correct factor levels)
    # Use levels as stored in model training
    df$price_category <- factor(
      cut(df$price,
          breaks = c(0, 75, 100, 125, 150, 175, 200, 250, 300, 400, 1000),
          labels = price_levels,
          include.lowest = TRUE),
      levels = price_levels
    )
    
    df$footage_category <- factor(
      cut(df$footage,
          breaks = c(0, 150, 200, 250, 300, 350, 400, 500, 600, Inf),
          labels = footage_levels,
          include.lowest = TRUE),
      levels = footage_levels
    )
    
    df$review_category <- factor(
      cut(df$avgreview,
          breaks = c(0, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
          labels = review_levels,
          include.lowest = TRUE),
      levels = review_levels
    )
    
    df$floor_category <- factor(
      cut(df$floor,
          breaks = c(-Inf, 1, 3, 5, 8, 10, 15, 20, Inf),
          labels = floor_levels,
          include.lowest = TRUE),
      levels = floor_levels
    )
    
    
    # Rule logic
    rule <- "Neutral"
    if (df$value_score > value_quantiles[2] && df$price_vs_borough < 0.9 &&
        df$footage_vs_borough > 1.1 && df$review_vs_borough > 1.1) rule <- "Good"
    if (df$price > price_quantiles[4] && df$value_score < value_quantiles[1] &&
        df$review_weight < review_weight_median) rule <- "Bad"
    if (df$is_manhattan && df$price < price_quantiles[2] &&
        df$footage > footage_quantiles[3] && df$avgreview > review_quantiles[2]) rule <- "Good"
    if (df$footage < footage_quantiles[1] && df$price > price_quantiles[3] &&
        df$review_vs_borough < 0.9) rule <- "Bad"
    if (!df$is_manhattan && df$value_score > value_quantiles[2] &&
        df$review_weight > review_weight_median && df$footage_vs_borough > 1.2) rule <- "Good"
    if (df$price_vs_borough > 1.3 && df$review_vs_borough < 0.8 &&
        df$footage_vs_borough < 1.1) rule <- "Bad"
    
    df$model1 <- predict(tree1, df, type = "class")
    df$model2 <- predict(tree2, df, type = "class")
    df$rule_model <- rule
    
    final_pred <- predict(combined_tree, df, type = "class")
    print("------- DEBUG INFO -------")
    print(paste("Rule Model:", rule))
    print(paste("Model1 Prediction:", df$model1))
    print(paste("Model2 Prediction:", df$model2))
    print(paste("Final Combined Prediction:", final_pred))
    print("---------------------------")
    
    
    # Render prediction result
    output$prediction_result <- renderUI({
      if (final_pred == "Good") {
        HTML('<span style="color: green; font-size: 20px;">✔️ Good Deal</span>')
      } else if (final_pred == "Bad") {
        HTML('<span style="color: red; font-size: 20px;">❌ Bad Deal</span>')
      } else {
        HTML('<span style="color: orange; font-size: 20px;">⚖️ Neutral Deal</span>')
      }
    })
    
    # Similar Listings (unchanged)
    numeric_test <- test_data[, c("price", "footage", "avgreview", "floor")]
    numeric_user <- as.numeric(df[, c("price", "footage", "avgreview", "floor")])
    distances <- apply(numeric_test, 1, function(x) sqrt(sum((x - numeric_user)^2)))
    same_area <- which(test_data$borough == input$borough & test_data$neighbourhood == input$neighbourhood)
    if (length(same_area) > 0) distances[-same_area] <- Inf
    
    output$similar_listings <- renderTable({
      top_n <- 3
      ranked_indices <- order(distances)
      if (length(same_area) >= top_n) ranked_indices <- same_area[order(distances[same_area])]
      top_matches <- test_data[ranked_indices[1:min(top_n, length(ranked_indices))],
                               c("neighbourhood", "price", "footage", "avgreview", "floor", "Deal")]
      top_matches$Rank <- 1:nrow(top_matches)
      colnames(top_matches) <- c("Neighbourhood", "Price ($)", "Footage (sqft)",
                                 "Avg. Review", "Floor", "Deal", "Rank")
      top_matches
    })
    
    output$review_vs_price <- renderPlotly({
      borough_subset <- test_data[test_data$borough == input$borough &
                                    test_data$price >= input$price_range[1] &
                                    test_data$price <= input$price_range[2], ]
      borough_subset <- na.omit(borough_subset[, c("avgreview", "footage", "price", "Deal", "neighbourhood")])
      borough_subset$Deal <- factor(borough_subset$Deal, levels = c("Good", "Neutral", "Bad"))
      plot_ly(
        data = borough_subset,
        x = ~avgreview, y = ~footage, type = 'scatter', mode = 'markers',
        color = ~Deal, colors = c("Good" = "green", "Neutral" = "orange", "Bad" = "red"),
        text = ~paste("Neighbourhood:", neighbourhood,
                      "<br>Price: $", round(price, 2),
                      "<br>Review:", round(avgreview, 2),
                      "<br>Footage:", footage,
                      "<br>Deal:", Deal),
        hoverinfo = 'text',
        marker = list(size = 10, opacity = 0.7)
      ) %>% layout(
        title = paste("Review vs Footage -", input$borough),
        xaxis = list(title = "Average Review"),
        yaxis = list(title = "Footage (sqft)"),
        legend = list(title = list(text = "Deal Type"))
      )
    })
  })
}

# Launch app
shinyApp(ui = ui, server = server)
