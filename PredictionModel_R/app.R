library(shiny)
library(plotly)

# Load required data
test_data <- read.csv("Test.csv")
predictions <- read.csv("result_airbnb.csv")
borough_data <- read.csv("localities_NYC.csv")
test_data <- merge(test_data, borough_data, by.x = "neighbourhood", by.y = "neighborhood", all.x = TRUE)


# Combine for easy lookup
test_data$Deal <- predictions$Deal

# Neighbourhood options
neighbourhoods <- c(
  "Fort Greene", "West Village", "Sunset Park", "Midtown", "Bushwick",
  "Williamsburg", "Clinton Hill", "Astoria", "Upper East Side", "Upper West Side",
  "Bedford-Stuyvesant", "Crown Heights", "Flushing", "Greenpoint", "Prospect-Lefferts Gardens",
  "Chelsea", "Financial District", "Elmhurst", "Harlem", "Hell's Kitchen",
  "East Flatbush", "Ditmars Steinway", "Gramercy", "East Village", "Washington Heights",
  "Kips Bay", "Rosedale", "Two Bridges", "Lower East Side", "Long Island City",
  "South Slope", "Inwood", "Rego Park", "Park Slope", "Flatiron District",
  "East New York", "Murray Hill", "Tribeca", "NoHo", "Howard Beach",
  "Flatbush", "East Harlem", "Theater District", "Jackson Heights", "Jamaica",
  "Sunnyside", "Gravesend", "Forest Hills", "Morningside Heights", "Cypress Hills",
  "Fort Hamilton", "Canarsie", "Queens Village", "Sheepshead Bay", "Nolita",
  "Corona", "Prospect Heights", "East Elmhurst", "Chinatown", "Edgemere",
  "Bay Ridge", "Cobble Hill", "Woodside", "SoHo", "Windsor Terrace",
  "Woodhaven", "Dyker Heights", "Briarwood", "Carroll Gardens", "Kensington",
  "Boerum Hill", "Richmond Hill", "Borough Park", "Greenwich Village", "Glendale",
  "Cambria Heights", "Battery Park City", "Bensonhurst", "Little Italy", "Gowanus",
  "Ridgewood", "Middle Village", "Marble Hill", "Brooklyn Heights", "Ozone Park",
  "Douglaston", "Downtown Brooklyn", "Springfield Gardens", "Rockaway Beach", "Civic Center",
  "Maspeth", "Hollis", "DUMBO", "Fresh Meadows", "Manhattan Beach",
  "St. Albans", "Bay Terrace", "Arverne", "Coney Island", "Midwood",
  "Bayside", "Flatlands", "Roosevelt Island", "Far Rockaway", "Brownsville",
  "Red Hook", "Bergen Beach", "Bellerose", "Holliswood", "Bath Beach",
  "South Ozone Park", "Kew Gardens", "College Point", "Columbia St", "Kew Gardens Hills",
  "Laurelton", "Brighton Beach", "Stuyvesant Town", "Bayswater", "Jamaica Hills",
  "Sea Gate", "Jamaica Estates", "Belle Harbor", "Vinegar Hill", "Whitestone",
  "Neponsit", "Navy Yard", "Mill Basin", "Breezy Point", "Little Neck"
)

boroughs <- c("Brooklyn", "Manhattan", "Queens", "Staten Island", "Bronx")

# UI
ui <- fluidPage(
  titlePanel("Airbnb Deal Prediction"),
  
  fluidRow(
    column(4,
           wellPanel(
             selectInput("neighbourhood", "Neighbourhood", choices = neighbourhoods),
             selectInput("borough", "Borough", choices = boroughs),
             numericInput("price", "Price", value = 100, min = 10, max = 1000),
             numericInput("footage", "Footage", value = 250, min = 50, max = 1000),
             numericInput("avgreview", "Average Review (0-5)", value = 4.2, min = 0, max = 5, step = 0.1),
             numericInput("floor", "Floor", value = 3, min = 0, max = 50),
             actionButton("predict", "Predict Deal")
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

# Server
server <- function(input, output) {
  observeEvent(input$predict, {
    # User input
    user_input <- data.frame(
      neighbourhood = input$neighbourhood,
      borough = input$borough,
      price = input$price,
      footage = input$footage,
      avgreview = input$avgreview,
      floor = input$floor
    )
    
    # Compute distances
    numeric_test <- test_data[, c("price", "footage", "avgreview", "floor")]
    numeric_user <- as.numeric(user_input[, c("price", "footage", "avgreview", "floor")])
    
    distances <- apply(numeric_test, 1, function(x) sqrt(sum((x - numeric_user)^2)))
    
    # Prefer same area if available
    same_area <- which(test_data$borough == input$borough & test_data$neighbourhood == input$neighbourhood)
    if (length(same_area) > 0) {
      distances[-same_area] <- Inf
    }
    
    # Get predicted deal from closest match
    closest_index <- which.min(distances)
    predicted_deal <- test_data$Deal[closest_index]
    
    # Display styled prediction result
    output$prediction_result <- renderUI({
      if (predicted_deal == "Good") {
        HTML('<span style="color: green; font-size: 20px;">✔️ Good Deal</span>')
      } else if (predicted_deal == "Bad") {
        HTML('<span style="color: red; font-size: 20px;">❌ Bad Deal</span>')
      } else {
        HTML('<span style="color: orange; font-size: 20px;"> Neutral Deal</span>')
      }
    })
    
    # Show top 3 similar listings
    output$similar_listings <- renderTable({
      top_n <- 3
      ranked_indices <- order(distances)
      
      # Use same_area subset if it has enough data
      if (length(same_area) >= top_n) {
        ranked_indices <- same_area[order(distances[same_area])]
      }
      
      top_matches <- test_data[ranked_indices[1:min(top_n, length(ranked_indices))],
                               c("neighbourhood", "price", "footage", "avgreview", "floor", "Deal")]
      
      # Add Rank and nice column names
      top_matches$Rank <- 1:nrow(top_matches)
      colnames(top_matches) <- c("Neighbourhood", "Price ($)", "Footage (sqft)",
                                 "Avg. Review", "Floor", "Deal", "Rank")
      top_matches
    })
    
    
    # Scatter plot of price vs. review
    output$review_vs_price <- renderPlotly({
      if (!"borough" %in% colnames(test_data)) {
        showNotification("Borough column is missing. Please join with Boroughs.csv if needed.", type = "error")
        return(NULL)
      }
      
      borough_subset <- test_data[
        test_data$borough == input$borough &
          test_data$price >= input$price_range[1] &
          test_data$price <= input$price_range[2], 
      ]
      
      borough_subset <- na.omit(borough_subset[, c("avgreview", "footage", "price", "Deal", "neighbourhood")])
      borough_subset$Deal <- factor(borough_subset$Deal, levels = c("Good", "Neutral", "Bad"))
      
      deal_colors <- c("Good" = "green", "Neutral" = "orange", "Bad" = "red")
      
      plot_ly(
        data = borough_subset,
        x = ~avgreview,
        y = ~footage,
        type = 'scatter',
        mode = 'markers',
        color = ~Deal,
        colors = deal_colors,
        text = ~paste("Neighbourhood:", neighbourhood,
                      "<br>Price: $", round(price, 2),
                      "<br>Review:", round(avgreview, 2),
                      "<br>Footage:", footage,
                      "<br>Deal:", Deal),
        hoverinfo = 'text',
        marker = list(size = 10, opacity = 0.7)
      ) %>%
        layout(
          title = paste("Review vs Footage -", input$borough),
          xaxis = list(title = "Average Review"),
          yaxis = list(title = "Footage (sqft)"),
          legend = list(title = list(text = "Deal Type"))
        )
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
