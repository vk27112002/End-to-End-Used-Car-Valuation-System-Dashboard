library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(forcats)
library(scales)
library(DT)
library(tidyr)     



cars_df <- read.csv("cars_cleaned_v3.csv")

cars_df <- cars_df %>%
  mutate(
    Price = as.numeric(Price),
    Kilometers_Driven = as.numeric(Kilometers_Driven),
    Vehicle_Age =as.integer(Vehicle_Age), 
    Year = (as.numeric(format(Sys.Date(), "%Y")) - Vehicle_Age), # Calculate Year from Age
    Brand = as.character(Brand),
    City_of_Listing = as.character(City_of_Listing),
    Fuel_Type = as.character(Fuel_Type),
    Transmission = as.character(Transmission),
    Market_Segment = as.character(Market_Segment),
    Model = as.character(Model)
  )
#Making filters
city_choices <- c("All", unique(cars_df$City_of_Listing))
brand_choices <- c("All", unique(cars_df$Brand))
segment_choices <- c("All", unique(cars_df$Market_Segment))
fuel_choices <- c("All", unique(cars_df$Fuel_Type))

#finding the top 6 brands
top_6_brands <- cars_df %>%
  count(Brand, sort = TRUE) %>%
  slice_head(n = 6) %>%
  pull(Brand)

#THE UI
ui <- dashboardPage(
  
  skin = "blue", 
  dashboardHeader(title = "Used Car Insights"),
  
  #Pls fix error vaibhav
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Top Picks & Recommendations", tabName = "recommend", icon = icon("star")),
      menuItem("Market Overview", tabName = "overview", icon = icon("chart-pie")),
      menuItem("Price Drivers", tabName = "rq1", icon = icon("dollar-sign")),
      menuItem("Geographic Analysis", tabName = "rq2", icon = icon("map-marked-alt")),
      menuItem("Segment & Depreciation", tabName = "rq3", icon = icon("car-crash")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      
      hr(),
      h5(strong("  Global Filters"), style = "padding-left: 15px;"),
      selectInput("cityInput", "City:", choices = city_choices, selected = "All"),
      selectInput("brandInput", "Brand:", choices = brand_choices, selected = "All"),
      selectInput("segmentInput", "Market Segment:", choices = segment_choices, selected = "All"),
      selectInput("fuelInput", "Fuel Type:", choices = fuel_choices, selected = "All"),
      
      sliderInput("priceInput", "Price Range (INR):",
                  min = min(cars_df$Price, na.rm=T), max = max(cars_df$Price, na.rm=T),
                  value = c(min(cars_df$Price, na.rm=T), max(cars_df$Price, na.rm=T)),
                  step = 10000, pre = "₹", sep = ","),
      
      sliderInput("kmInput", "Kilometers Driven:",
                  min = min(cars_df$Kilometers_Driven, na.rm=T), max = max(cars_df$Kilometers_Driven, na.rm=T),
                  value = c(min(cars_df$Kilometers_Driven, na.rm=T), max(cars_df$Kilometers_Driven, na.rm=T)),
                  step = 1000, sep = ","),
      
      sliderInput("ageInput", "Vehicle Age (Years):",
                  min = min(cars_df$Vehicle_Age, na.rm=T), max = max(cars_df$Vehicle_Age, na.rm=T),
                  value = c(min(cars_df$Vehicle_Age, na.rm=T), max(cars_df$Vehicle_Age, na.rm=T)),
                  step = 1)
    )
  ),
  
  #Main body of dashboard
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "home",
              fluidRow(
                valueBoxOutput("totalListingsBox", width = 4),
                valueBoxOutput("avgPriceBox", width = 4),
                valueBoxOutput("avgAgeBox", width = 4)
              ),
              
              
              fluidRow(
                box(
                  title = "Dynamic Insights",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("dynamicInsightText") 
                )
              ),
              
      ),
      
      
      tabItem(tabName = "recommend",
              h2("Top Picks & Recommendations"),
              p("Find the best cars based on your selected filters. Click a model in the 'Top 5' list to see a detailed analysis."),
              
              fluidRow(
                column(width = 5,
                       box(
                         title = "Top 5 Most Listed Models",
                         solidHeader = TRUE, status = "primary",
                         width = NULL, # Auto-widthx
                         p("Click a row to analyze the model."),
                         DT::dataTableOutput("topModelsTable") ,
                         
                       ),
                       box(
                         title = "Best Value for Money",
                         solidHeader = TRUE, status = "primary",
                         width = NULL,
                         collapsible = TRUE, collapsed = TRUE,
                         p("These cars (under 5 years old) are priced at least 20% below the average for their specific model."),
                         DT::dataTableOutput("bestValueTable")
                       )
                ),
                
                
                column(width = 7,
                       box(
                         title = "Model vs. Segment Analysis",
                         solidHeader = TRUE, status = "primary",
                         width = NULL,
                         uiOutput("modelDetailTitle"), 
                         plotOutput("modelComparisonChart"), # <-- PLOT WITH BUG FIX
                         p(tags$small(HTML(" This chart compares the selected model (blue) against its entire market segment average (yellow) across three key stats. For Price and Mileage, a *lower* bar is better. For Age, a *lower* bar means the listed cars are newer.")))
                       ),
                       fluidRow(
                         valueBoxOutput("selectedModelPrice", width = 6),
                         valueBoxOutput("selectedModelAge", width = 6)
                       )
                )
              )
      ),
      
      
      tabItem(tabName = "overview",
              h2("Market Overview (Univariate Analysis)"),
              fluidRow(
                box(title = "Price Distribution", status = "primary", solidHeader = TRUE, width = 6, 
                    plotOutput("priceHist"),
                    p(tags$small("This shows the frequency of cars at different price points. A 'right-skew' (long tail on the right) is common, showing a few very expensive cars."))
                ),
                box(title = "Listings by Brand ", status = "primary", solidHeader = TRUE, width = 6, 
                    plotOutput("brandBar"),
                    p(tags$small(" This bar chart shows the most common brands in your filtered selection, indicating market dominance. Colors are assigned per brand."))
                )
              ),
              fluidRow(
                box(title = "Listings by Fuel Type", status = "primary", solidHeader = TRUE, width = 6, 
                    plotOutput("fuelBar"),
                    p(tags$small(" A simple breakdown of the most common fuel types (e.g., Petrol, Diesel, CNG) available, colored by type."))
                ),
                box(title = "Listings by Market Segment", status = "primary", solidHeader = TRUE, width = 6, 
                    plotOutput("segmentBar"),
                    p(tags$small(" Shows the popularity of different car types (e.g., Hatchback, SUV) in your selection, colored by segment."))
                )
              )
      ),
      
      #Research Question 1
      tabItem(tabName = "rq1",
              h2("What Determines a Car's Price?"),
              fluidRow(
                box(title = "Price vs. Vehicle Age", status = "primary", solidHeader = TRUE, width = 12, 
                    plotOutput("priceVsAgePlot"),
                    p(tags$small("This is a key plot. The red line shows the average trend."))
                ),
                box(title = "Price vs. Kilometers Driven", status = "primary", solidHeader = TRUE, width = 12, 
                    plotOutput("priceVsKmPlot"),
                    p(tags$small("This scatter plot shows that cars with higher mileage (more KMs driven) generally have a lower resale price."))
                ),
                
                box(title = "Price vs. Transmission (Faceted by Segment)", status = "primary", solidHeader = TRUE, width = 12, 
                    plotOutput("priceVsTransmissionPlot"),
                    p(tags$small(HTML(" This plot shows the price premium for Automatic vs. Manual cars.")))
                ),
                
                box(title = "Price Distribution by Brand", status = "primary", solidHeader = TRUE, width = 12, 
                    plotOutput("priceVsBrandPlot"),
                    p(tags$small("This box plot compares the price ranges for different brands, colored by brand. A 'box' shows the middle 50% of prices."))
                )
              )
      ),
      
      #Research Question 2
      tabItem(tabName = "rq2",
              h2("How Do Markets Differ by City?"),
              fluidRow(
                box(title = "Price Distribution by City", status = "primary", solidHeader = TRUE, width = 12, 
                    plotOutput("priceByCityPlot"),
                    p(tags$small("This box plot compares the median price and price range of used cars across different cities, colored by city."))
                ),
                box(title = "Brand Popularity by City (Top 15 Brands)", status = "primary", solidHeader = TRUE, width = 12, 
                    plotOutput("brandByCityPlot"),
                    p(tags$small("This plot shows if certain brands are more popular in one city than another."))
                ),
                box(title = "Fuel Type Preference by City", status = "primary", solidHeader = TRUE, width = 12, 
                    plotOutput("fuelByCityPlot"),
                    p(tags$small("This stacked bar chart shows the percentage of fuel types in each city. For example, Delhi NCR might have a higher percentage of CNG vehicles due to local policies."))
                )
              )
      ),
      
      # Research Question 3
      tabItem(tabName = "rq3",
              h2("Deep Dive: Segments & Depreciation "),
              fluidRow(
                box(
                  title = "Brand-Specific Depreciation Rates",
                  status = "primary", solidHeader = TRUE, width = 12,
                  p("This plot ignores global filters to compare the top 6 brands."),
                  checkboxGroupInput("depreciationBrands", "Select brands to compare:",
                                     choices = top_6_brands, selected = top_6_brands, inline = TRUE),
                  plotOutput("depreciationPlot"),
                  p(tags$small("A steeper line means the brand loses value faster. A flatter line means the brand holds its value well over time."))
                )
              ),
              fluidRow(
                box(title = "Mileage Impact by Market Segment ", status = "primary", solidHeader = TRUE, width = 6, 
                    plotOutput("segmentKmPlot"),
                    p(tags$small("This shows the Price vs. Mileage relationship, separated by car type."))
                ),
                box(title = "Owner Type Analysis: Age vs. Mileage ", status = "primary", solidHeader = TRUE, width = 6, 
                    plotOutput("ageKmPlot"),
                    p(tags$small("This helps find market anomalies. Cars in the 'top-left' are 'Heavy Users' (high km, low age). Cars in the 'bottom-right' are 'Light Users' (low km, high age) and can be great finds."))
                )
              ),
              fluidRow(
                box(title = "Depreciation Rate by Fuel Type", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("fuelDepreciationPlot"),
                    p(tags$small(HTML("This chart estimates the average drop in price (₹) for each additional year of age, grouped by fuel type. A more negative bar means faster depreciation.")))
                )
              )
      ),
      
      #Data explorer
      tabItem(tabName = "data",
              h2("Filtered Data Explorer"),
              fluidRow(
                box(width = 12, DT::dataTableOutput("dataTable"))
              )
      ),
      
      #About tab
      tabItem(tabName = "about",
              h2("About This Project"),
              box(
                title = "MTH-208 Course Project", status = "primary", solidHeader = TRUE, width = 12,
                h4(strong("Research Questions:")),
                tags$ol(
                  tags$li("What are the most significant predictors of a used car's price?"),
                  tags$li("How do pricing structures and brand popularity vary across cities?"),
                  tags$li("What is the relationship between age, mileage, and resale value for different segments?")
                ),
                br(),
                h4(strong("Data Source:")),
                p("We scraped data from CarDekho and also took data from a publicly available Kaggle dataset."),
                br(),
                h4(strong("User Features:")),
                p("We have also added a Top Models and Recommendations tab in the app for users to find the cars most often listed and see how they compare with the market."),
                br(),
                h4(strong("Team Members:")),
                tags$ul(
                  tags$li("Yatin Preetam Bhojwani (241211)"),
                  tags$li("Vaibhav Kalyan (241125)"),
                  tags$li("Vaibhav Khare (251080109)"),
                  tags$li("Kaustabh Roy (251080071)")
                )
              )
      )
    )
  )
)



server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    data <- cars_df
    
    if (input$cityInput != "All") {
      data <- data %>% filter(City_of_Listing == input$cityInput)
    }
    if (input$brandInput != "All") {
      data <- data %>% filter(Brand == input$brandInput)
    }
    if (input$segmentInput != "All") {
      data <- data %>% filter(Market_Segment == input$segmentInput)
    }
    if (input$fuelInput != "All") {
      data <- data %>% filter(Fuel_Type == input$fuelInput)
    }
    
    data <- data %>%
      filter(Price >= input$priceInput[1] & Price <= input$priceInput[2]) %>%
      filter(Kilometers_Driven >= input$kmInput[1] & Kilometers_Driven <= input$kmInput[2]) %>%
      filter(Vehicle_Age >= input$ageInput[1] & Vehicle_Age <= input$ageInput[2])
    
    validate(
      need(nrow(data) > 0, "No cars match your selected filters. Please widen your search.")
    )
    
    return(data)
  })
  
  
  output$totalListingsBox <- renderValueBox({
    valueBox(
      value = prettyNum(nrow(filtered_data()), big.mark = ","),
      subtitle = "Total Listings",
      icon = icon("car"),
      color = "blue" # Matched to theme
    )
  })
  
  output$avgPriceBox <- renderValueBox({
    avg_price <- mean(filtered_data()$Price, na.rm = T)
    valueBox(
      value = prettyNum(round(avg_price), big.mark = ","),
      subtitle = "Average Price (INR)",
      icon = icon("money-bill-wave"),
      color = "blue" 
    )
  })
  
  output$avgAgeBox <- renderValueBox({
    avg_age <- mean(filtered_data()$Vehicle_Age, na.rm = T)
    valueBox(
      value = paste(round(avg_age, 1), "Years"),
      subtitle = "Average Vehicle Age",
      icon = icon("calendar-alt"),
      color = "blue" 
    )
  })
  
  
  output$dynamicInsightText <- renderUI({
    
    data <- filtered_data() 
    
    #Calculate most popular model in the filtered data
    top_model <- data %>%
      count(Brand, Model, sort = TRUE) %>%
      unite("Full_Model", Brand, Model, sep = " ") %>%
      slice_head(n = 1)
    
    # Get average price from filtered data
    avg_price_insight <- mean(data$Price, na.rm = TRUE)
    
    # Check which city is selected
    city_selected <- input$cityInput
    
    
    # Part 1: The city
    if(city_selected == "All") {
      city_text <- "across all cities"
    } else {
      city_text <- paste("in", city_selected)
    }
    
    # Part 2: The model
    if(nrow(top_model) > 0) {
      model_text <- paste0("The most popular model is the <b>", top_model$Full_Model, "</b> (with ", top_model$n, " listings).")
    } else {
      model_text <- "There are no listings for a single model."
    }
    
    # Part 3: The price
    price_text <- paste0("The average price for this selection is <b>₹", prettyNum(round(avg_price_insight), big.mark = ","), "</b>.")
    
    
    HTML(paste(
      "<p style='font-size: 16px;'>", # Larger text
      "Based on your filters ", city_text, ":<br>",
      "&bull; ", model_text, "<br>",
      "&bull; ", price_text,
      "</p>"
    ))
  })
  
  
  selected_model <- reactiveVal()
  
  top_models_data <- reactive({
    filtered_data() %>%
      count(Brand, Model, sort = TRUE) %>%
      unite("Full_Model", Brand, Model, sep = " ") %>%
      slice_head(n = 5) %>%
      rename(`Top 5 Models` = Full_Model, Listings = n)
  })
  
  output$topModelsTable <- DT::renderDataTable({
    datatable(top_models_data(),
              selection = 'single', 
              options = list(pageLength = 5, dom = 't'), 
              rownames = FALSE
    )
  })
  
  observeEvent(input$topModelsTable_rows_selected, {
    selected_row_num <- input$topModelsTable_rows_selected
    if (length(selected_row_num)) {
      model_name <- top_models_data()[[1]][selected_row_num]
      selected_model(model_name) 
    }
  })
  
  output$modelDetailTitle <- renderUI({
    if (is.null(selected_model())) {
      h4(strong("Please select a model from the list to analyze."))
    } else {
      h4(strong(paste("Analysis for:", selected_model())))
    }
  })
  
  output$modelComparisonChart <- renderPlot({
    if (is.null(selected_model())) {
      return(NULL)
    }
    
    
    # Get stats for the selected model, ADD Type column
    model_stats <- cars_df %>%
      unite("Full_Model", Brand, Model, sep = " ", remove = FALSE) %>%
      filter(Full_Model == selected_model()) %>%
      summarise(
        Price = mean(Price, na.rm = T),
        Mileage = mean(Kilometers_Driven, na.rm = T),
        Age = mean(Vehicle_Age, na.rm = T),
        Segment = first(Market_Segment)
      ) %>%
      mutate(Type = "Selected Model") # <-- Add Type column
    
    
    segment_stats <- cars_df %>%
      filter(Market_Segment == model_stats$Segment) %>%
      summarise(
        Price = mean(Price, na.rm = T),
        Mileage = mean(Kilometers_Driven, na.rm = T),
        Age = mean(Vehicle_Age, na.rm = T)
      ) %>%
      mutate(Type = "Segment Average") # <-- Add Type column
    
    
    # Combine with a clean 'bind_rows' and pivot
    comparison_data <- bind_rows(model_stats, segment_stats) %>%
      select(Type, Price, Mileage, Age) %>%
      pivot_longer(cols = c("Price", "Mileage", "Age"), names_to = "Metric", values_to = "Value")
    
    # 4. Create the plot
    ggplot(comparison_data, aes(x = Type, y = Value, fill = Type)) +
      geom_col(position = "dodge") +
      facet_wrap(~ Metric, scales = "free_y") + 
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = c("Selected Model" = "#3c8dbc", "Segment Average" = "#f39c12")) +
      labs(
        x = "", y = "Average Value", fill = "Comparison"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold") 
      )
  })
  
  output$selectedModelPrice <- renderValueBox({
    if (is.null(selected_model())) {
      valueBox("-", "Avg. Model Price", icon = icon("dollar-sign"), color = "blue")
    } else {
      price <- cars_df %>%
        unite("Full_Model", Brand, Model, sep = " ") %>%
        filter(Full_Model == selected_model()) %>%
        pull(Price) %>%
        mean(na.rm = T)
      valueBox(prettyNum(round(price), big.mark = ","), "Avg. Model Price", icon = icon("dollar-sign"), color = "blue")
    }
  })
  
  output$selectedModelAge <- renderValueBox({
    if (is.null(selected_model())) {
      valueBox("-", "Avg. Model Age", icon = icon("calendar-alt"), color = "blue")
    } else {
      age <- cars_df %>%
        unite("Full_Model", Brand, Model, sep = " ") %>%
        filter(Full_Model == selected_model()) %>%
        pull(Vehicle_Age) %>%
        mean(na.rm = T)
      valueBox(paste(round(age, 1), "Yrs"), "Avg. Model Age", icon = icon("calendar-alt"), color = "blue")
    }
  })
  
  
  best_value_data <- reactive({
    filtered_data() %>%
      filter(Vehicle_Age <= 5) %>%
      group_by(Brand, Model) %>%
      filter(n() >= 2) %>% 
      mutate(Model_Avg_Price = mean(Price, na.rm = T)) %>%
      ungroup() %>%
      filter(Price <= (Model_Avg_Price * 0.8)) %>%
      select(Brand, Model, Vehicle_Age, Price, Kilometers_Driven, City_of_Listing, Model_Avg_Price) %>%
      arrange(Price) %>%
      mutate(Model_Avg_Price = round(Model_Avg_Price))
  })
  
  output$bestValueTable <- DT::renderDataTable({
    datatable(best_value_data(),
              options = list(pageLength = 3, scrollX = TRUE),
              rownames = FALSE,
              colnames = c("Brand", "Model", "Age", "Price", "KM", "City", "Model Avg."))
  })
  
  
  
  output$priceHist <- renderPlot({
    ggplot(filtered_data(), aes(x = Price)) +
      geom_histogram(bins = 30, fill = "#3c8dbc", alpha = 0.8) +
      scale_x_continuous(labels = scales::comma) +
      labs(x = "Price (INR)", y = "Frequency") + theme_minimal(base_size = 14)
  })
  
  output$brandBar <- renderPlot({
    data_to_plot <- filtered_data() %>% count(Brand) %>% top_n(15, n)
    ggplot(data_to_plot, aes(x = n, y = fct_reorder(Brand, n), fill = Brand)) +
      geom_col() +
      labs(x = "Count", y = "Brand") + 
      theme_minimal(base_size = 14) +
      theme(legend.position = "none") 
  })
  
  output$fuelBar <- renderPlot({
    ggplot(filtered_data(), aes(x = fct_infreq(Fuel_Type), fill = Fuel_Type)) +
      geom_bar() +
      labs(x = "Fuel Type", y = "Count") + 
      theme_minimal(base_size = 14) +
      theme(legend.position = "none") 
  })
  
  output$segmentBar <- renderPlot({
    ggplot(filtered_data(), aes(x = fct_infreq(Market_Segment), fill = Market_Segment)) +
      geom_bar() +
      labs(x = "Market Segment", y = "Count") + 
      theme_minimal(base_size = 14) +
      theme(legend.position = "none") 
  })
  
  
  output$priceVsAgePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Vehicle_Age, y = Price)) +
      geom_point(alpha = 0.4, color = "#3c8dbc") + 
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Vehicle Age (Years)", y = "Price (INR)") + theme_minimal(base_size = 14)
  })
  
  output$priceVsKmPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Kilometers_Driven, y = Price)) +
      geom_point(alpha = 0.4, color = "#3c8dbc") + 
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      labs(x = "Kilometers Driven", y = "Price (INR)") + theme_minimal(base_size = 14)
  })
  
  output$priceVsTransmissionPlot <- renderPlot({
    data_to_plot <- filtered_data() %>%
      filter(Transmission != "" & !is.na(Transmission))
    
    validate(
      need(nrow(data_to_plot) > 0, "No data with known transmission types for these filters.")
    )
    
    ggplot(data_to_plot, aes(x = Transmission, y = Price, fill = Transmission)) +
      geom_boxplot() +
      facet_wrap(~ Market_Segment, scales = "free_y") + 
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Transmission Type", y = "Price (INR)", fill = "Transmission") + 
      theme_minimal(base_size = 14) +
      theme(legend.position = "none") 
  })
  
  output$priceVsBrandPlot <- renderPlot({
    brands_to_plot <- filtered_data() %>% count(Brand) %>% top_n(15, n) %>% pull(Brand)
    data_to_plot <- filtered_data() %>% filter(Brand %in% brands_to_plot)
    ggplot(data_to_plot, aes(x = Price, y = fct_reorder(Brand, Price, .fun = median), fill = Brand)) +
      geom_boxplot() + 
      scale_x_continuous(labels = scales::comma) +
      labs(x = "Price (INR)", y = "Brand") + 
      theme_minimal(base_size = 14) +
      theme(legend.position = "none") 
  })
  
  
  
  output$priceByCityPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Price, y = fct_reorder(City_of_Listing, Price, .fun = median), fill = City_of_Listing)) +
      geom_boxplot() + 
      scale_x_continuous(labels = scales::comma) +
      labs(x = "Price (INR)", y = "City") + 
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  output$brandByCityPlot <- renderPlot({
    top_brands_filtered <- filtered_data() %>% count(Brand) %>% top_n(15, n) %>% pull(Brand)
    data_to_plot <- filtered_data() %>% filter(Brand %in% top_brands_filtered)
    ggplot(data_to_plot, aes(y = Brand, fill = Brand)) + 
      geom_bar() +
      facet_wrap(~ City_of_Listing) +
      labs(x = "Count", y = "Brand") +
      theme_minimal(base_size = 14) + 
      theme(axis.text.y = element_text(size = 8), legend.position = "none") 
  })
  
  output$fuelByCityPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = City_of_Listing, fill = Fuel_Type)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "City", y = "Proportion", fill = "Fuel Type") + 
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") 
  })
  
  #RQ3
  
  depreciation_data <- reactive({
    cars_df %>%
      filter(Brand %in% input$depreciationBrands)
  })
  
  output$depreciationPlot <- renderPlot({
    if (nrow(depreciation_data()) == 0) return(NULL)
    ggplot(depreciation_data(), aes(x = Vehicle_Age, y = Price, color = Brand)) +
      geom_point(alpha = 0.2) +
      geom_smooth(method = "lm", se = FALSE, size = 1.2) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Vehicle Age (Years)", y = "Price (INR)", color = "Brand") +
      theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  })
  
  output$segmentKmPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Kilometers_Driven, y = Price)) +
      geom_point(alpha = 0.4, color = "#3c8dbc") + 
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      facet_wrap(~ Market_Segment, scales = "free_y") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      labs(x = "Kilometers Driven", y = "Price (INR)") + theme_minimal(base_size = 14)
  })
  
  output$ageKmPlot <- renderPlot({
    plot_data <- filtered_data()
    ggplot(plot_data, aes(x = Vehicle_Age, y = Kilometers_Driven)) +
      geom_point(alpha = 0.5, color = "#3c8dbc") + 
      geom_smooth(color = "red", se = FALSE) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Vehicle Age (Years)", y = "Kilometers Driven") +
      annotate("text", x = max(plot_data$Vehicle_Age, na.rm=T) * 0.8, y = min(plot_data$Kilometers_Driven, na.rm=T) * 1.2, 
               label = "Light Users", fontface = "italic") +
      annotate("text", x = min(plot_data$Vehicle_Age, na.rm=T) * 1.2, y = max(plot_data$Kilometers_Driven, na.rm=T) * 0.8, 
               label = "Heavy Users", fontface = "italic") +
      theme_minimal(base_size = 14)
  })
  
  output$fuelDepreciationPlot <- renderPlot({
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 20, "Not enough data to calculate depreciation for these filters.")
    )
    
    depreciation <- data %>%
      filter(!is.na(Fuel_Type) & Fuel_Type != "") %>%
      group_by(Fuel_Type) %>%
      filter(n() > 5 & var(Vehicle_Age, na.rm = TRUE) > 0) %>% # Need variance and multiple points
      summarise(Depreciation_Rate = cov(Price, Vehicle_Age, use = "complete.obs") /
                  var(Vehicle_Age, na.rm = TRUE))
    
    validate(
      need(nrow(depreciation) > 0, "Could not calculate depreciation. Try widening your filters.")
    )
    
    ggplot(depreciation, aes(x = fct_reorder(Fuel_Type, Depreciation_Rate), y = Depreciation_Rate, fill = Fuel_Type)) +
      geom_col(show.legend = FALSE, alpha = 0.8) +
      scale_y_continuous(labels = scales::comma) + 
      labs(title = "Average Price Drop per Year by Fuel Type",
           x = "Fuel Type",
           y = "₹ Decrease per Year") +
      theme_minimal(base_size = 14)
  })
  
  
  output$dataTable <- DT::renderDataTable({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
}
shinyApp(ui = ui, server = server)

