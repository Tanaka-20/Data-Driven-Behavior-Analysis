<<<<<<< HEAD
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(lubridate)
library(treemapify)
library(scales)
library(data.table)
library(DT)
library(RColorBrewer)
library(future)
plan(multisession)
library(conflicted)
library(plotly)


# Preprocess the data at app startup
data <- fread("./data/bike.csv")
data <- data[
  !is.na(started_at) & !is.na(ended_at),
  .(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    ride_duration = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = factor(lubridate::wday(started_at), 
                         levels = 1:7, 
                         labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
    hour_of_day = lubridate::hour(as.POSIXct(started_at)),
    rideable_type,
    member_casual,
    start_station_name,
    end_station_name,
    start_lat,
    start_lng,
    end_lat,
    end_lng,
    ride_id,
    start_station_id,
    end_station_id
  )
][ride_duration > 0 & ride_duration <= 120]

# Precompute top stations at startup for a faster rendering
top_stations <- reactive({
  data[!is.na(start_station_name), .N, by = start_station_name][order(-N)][1:10]
})

# Precompute station density data at startup
station_density <- reactive({
  data[!is.na(start_lat) & !is.na(start_lng), 
       .(ride_count = .N), 
       by = .(start_station_name, start_lat, start_lng)]
})

# Precompute daily usage data at startup
daily_usage <- reactive({
  data[, .N, by = .(date = as.Date(started_at))]
})

# Convert to data.table for faster filtering
data <- as.data.table(data)

# Cache data for reuse across plots
top_stations_data <- reactive(top_stations())
station_density_data <- reactive(station_density())
daily_usage_data <- reactive(daily_usage())

# Define consistent theme for all plots
custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size =20, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "top"
  )

# Define UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body, html { margin: 0; padding: 0; overflow-x: hidden; }
      .burger-menu { position: fixed; top: 10px; left: 10px; z-index: 1000; cursor: pointer; }
      .burger-line { width: 30px; height: 4px; background-color: #000000; margin: 6px 0; }
      .menu-panel { position: fixed; top: 0; left: -250px; width: 250px; height: 100%; background-color: #FFF; z-index: 999;
                    box-shadow: 2px 0 5px rgba(0,0,0,0.5); transition: left 0.3s ease-in-out; overflow-y: auto; padding: 20px; padding-top: 60px; }
      .menu-panel.open { left: 0; }
      .menu-item { margin: 15px 0; font-size: 18px; cursor: pointer; color: #333; }
      .menu-item:hover { text-decoration: underline; }
      .content-container {
      /*background-color: #FFF !important;*/ /* Light blue background */
      margin: 0;
      height: 100%;
      overflow: hidden;
    }
      .content-container.shift { margin-left: 250px;}
      .fullscreen-image { position: absolute; top: 0; left: 0; width: 100%; height: 100vh; object-fit: cover; z-index: -1; }
    "))
  ),
  div(
    class = "burger-menu",
    onclick = "document.querySelector('.menu-panel').classList.toggle('open');
               document.querySelector('.content-container').classList.toggle('shift');",
    div(class = "burger-line"), div(class = "burger-line"), div(class = "burger-line")
  ),
  div(
    class = "menu-panel",
    div(class = "menu-item", onclick = "Shiny.setInputValue('menu', 'home', {priority: 'event'})", tags$span("Home")),
    div(class = "menu-item", onclick = "Shiny.setInputValue('menu', 'about', {priority: 'event'})", tags$span("About")),
    div(class = "menu-item", onclick = "Shiny.setInputValue('menu', 'user_behavior_analysis', {priority: 'event'})", tags$span("User Behavior Analysis")),
    div(class = "menu-item", onclick = "Shiny.setInputValue('menu', 'station_performance', {priority: 'event'})", tags$span("Station Performance")),
    div(class = "menu-item", onclick = "Shiny.setInputValue('menu', 'rides_dashboard', {priority: 'event'})", tags$span("Rides Dashboard")),
    div(class = "menu-item", onclick = "Shiny.setInputValue('menu', 'dynamic_dashboard', {priority: 'event'})", tags$span("Dynamic Dashboard"))
    
  ),
  div(class = "content-container", uiOutput("main_content"))
)

# Define Server
server <- function(input, output, session) {
  current_page <- reactiveVal("home")
  
  observeEvent(input$menu, {
    current_page(input$menu)
  })
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    req(input$timeline_slider)
    timeline_range <- input$timeline_slider
    bike_filter <- input$bike_type
    
    result <- data[started_at >= as.POSIXct(timeline_range[1]) & started_at <= as.POSIXct(timeline_range[2])]
    if (bike_filter != "All") {
      result <- result[rideable_type %in% bike_filter]
    }
    result
  })
  
  output$main_content <- renderUI({
    if (current_page() == "home") {
      tags$div(
        tags$img(src = "DDBA.jpg", class = "fullscreen-image"),
        tags$div(style = "position: relative; text-align: center; padding: 50px;")
      )
    } else if (current_page() == "about") {
      tags$div(
        style = "text-align: center; padding: 50px;",
        tags$h1("About This App"),
        tags$p("Details about the app.")
      )
    } else if (current_page() == "user_behavior_analysis") {
      tags$div(
        style = "text-align: center; padding: 20px;",
        tags$h1("User Behavior Analysis"),
        tags$div(
          style = "display: flex; justify-content: space-between; flex-wrap: wrap; gap: 20px;",
          tags$div(style = "flex: 1 1 calc(50% - 10px); max-width: 50%;", plotOutput("avgRidePlot")),
          tags$div(style = "flex: 1 1 calc(50% - 10px); max-width: 50%;", plotOutput("rideFrequencyPlot")),
          tags$div(style = "flex: 1 1 calc(50% - 10px); max-width: 50%;", plotOutput("diversityPlot")),
          tags$div(style = "flex: 1 1 calc(50% - 10px); max-width: 50%;", plotOutput("bikeUsagePlot"))
        )
      )
    } else if (current_page() == "station_performance") {
      tags$div(
        style = "text-align: center; padding: 20px;",
        tags$h1("Station Performance"),
        tags$div(
          style = "display: flex; justify-content: space-between; flex-wrap: wrap; gap: 20px;",
          # First plot
          tags$div(
            style = "flex: 1 1 calc(50% - 20px); max-width: 50%;",
            plotOutput("topStationsPlot")
          ),
          # Second plot
          tags$div(
            style = "flex: 1 1 calc(50% - 20px); max-width: 50%;",
            plotOutput("stationDensityPlot")
          ),
          # Third plot (starts new row due to flex-wrap)
          tags$div(
            style = "flex: 1 1 calc(50% - 20px); max-width: 50%;",
            plotOutput("areaChartPlot")
          )
        )
      )
    }
    else if (current_page() == "rides_dashboard") {
      tags$div(
        style = "text-align: center; padding: 20px;",
        tags$h1("Rides Dashboard"),
        
        # Timeline slider and filter side by side
        tags$div(
          style = "display: flex; justify-content: center; align-items: center; gap: 5%; margin-bottom: 20px;",
          
          # Timeline Slider
          sliderInput(
            "timeline_slider",
            "Select Timeline",
            min = min(data$started_at),
            max = max(data$started_at),
            value = c(min(data$started_at), max(data$started_at)),
            timeFormat = "%Y-%m-%d",
            step = 86400,
            width = "30%" # Proportional width for responsiveness
          ),
          
          # Bike Type Filter
          selectInput(
            "bike_type",
            "Filter by Bike Type",
            choices = c("All", unique(data$rideable_type)),
            selected = "All",
            multiple = TRUE,
            width = "28%" # Proportional width for responsiveness
          ),
          
          # Update Button
          actionButton(
            "update_button_1",
            "Update",
            style = "height: 38px; padding: 5px 10px; background-color: #007BFF; color: white; border: none; border-radius: 5px; cursor: pointer;"
          )
        ),
        
        # Scatter plot and summary table side by side
        tags$div(
          style = "display: flex; justify-content: space-between; flex-wrap: wrap; gap: 20px;",
          
          # Scatter plot
          tags$div(
            style = "flex: 1 1 50%; max-width: 50%;", # Increased width for scatter plot
            plotOutput("scatterPlot", height = "500px") # Adjust height if needed
          ),
          
          # Summary table with a name and border
          tags$div(
            style = "flex: 1 1 48%; max-width: 48%; border: 2px solid #000; border-radius: 5px; padding: 10px; height: 55vh; overflow-y: auto;", # Reduced height and added scrolling
            tags$h3("Summary Table", style = "text-align: center; margin-bottom: 10px;"),
            DTOutput("summaryTable")
          )
        )
        
      )
    }
    else if (current_page() == "dynamic_dashboard") {
      tags$div(
        style = "text-align: center; padding: 20px;",
        tags$h1("Dynamic Dashboard"),
        fluidRow(
          column(3, shinyWidgets::pickerInput(
            inputId = "row_var", 
            label = "Select Row Variables", 
            choices = names(data), 
            options = list(`actions-box` = TRUE, `live-search` = TRUE), 
            multiple = TRUE
          )),
          column(3, shinyWidgets::pickerInput(
            inputId = "col_var", 
            label = "Select Column Variables", 
            choices = names(data), 
            options = list(`actions-box` = TRUE, `live-search` = TRUE), 
            multiple = TRUE
          )),
          column(3, selectInput("chart_type", "Select Chart Type", 
                                choices = c("Bar Chart", "Line Chart", "Scatter Plot", "Histogram", "Stacked Bar Chart", 
                                            "Box Plot", "Density Plot", "Heatmap"))),
          column(3, sliderInput("time_range", "Select Time Range", 
                                min = as.Date(min(data$started_at)), 
                                max = as.Date(max(data$started_at)), 
                                value = c(as.Date(min(data$started_at)), as.Date(max(data$started_at))),
                                timeFormat = "%Y-%m-%d"))
        ),
        actionButton("update_chart", "Update Chart"),
        plotOutput("dynamic_chart")
      )
    }
  })
  
  observeEvent(input$menu, {
    if (input$menu == "user_behavior_analysis") {
      
      # User Behavior Analysis Outputs
      output$avgRidePlot <- renderPlot({
        avg_data <- data[, .(avg_duration = mean(ride_duration)), by = member_casual]
        ggplot(avg_data, aes(x = member_casual, y = avg_duration, fill = member_casual)) +
          geom_bar(stat = "identity") +
          labs(title = "Average Ride Duration by User Type", x = "User Type", y = "Duration (mins)", fill = "User Type") +
          custom_theme
      })
      
      output$rideFrequencyPlot <- renderPlot({
        freq_data <- data[, .N, by = .(day_of_week, member_casual)]
        ggplot(freq_data, aes(x = day_of_week, y = N, fill = member_casual)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Ride Frequency by Day of Week", x = "Day of Week", y = "Number of Rides", fill = "User Type") +
          custom_theme +
          scale_y_continuous(labels = scales::comma)
      })
      
      output$diversityPlot <- renderPlot({
        # Summarize data and filter out NA values
        diversity_data <- data[!is.na(start_station_name) & !is.na(member_casual), 
                               .N, by = .(start_station_name, member_casual)][order(-N)][1:20]
        
        # Create the bar chart
        ggplot(diversity_data, aes(x = reorder(start_station_name, -N), y = N, fill = member_casual)) +
          geom_bar(stat = "identity", position = "dodge") +
          coord_flip() +
          labs(
            title = "Top 20 Stations by User Type",
            x = "Station Name",
            y = "Number of Rides",
            fill = "User Type"
          ) +
          custom_theme +
          theme(axis.text.y = element_text(size = 8))
      })
      
      output$bikeUsagePlot <- renderPlot({
        bike_data <- data[, .N, by = .(rideable_type, member_casual)]
        ggplot(bike_data, aes(x = member_casual, y = N, fill = rideable_type)) +
          geom_bar(stat = "identity", position = "stack") +
          labs(title = "Bike Type Usage by User Group", x = "User Type", y = "Number of Rides", fill = "Bike Type") +
          custom_theme +
          scale_y_continuous(labels = scales::comma)
      })
    }
    
    
    if (input$menu == "station_performance") {
      
      # Top Stations Plot
      output$topStationsPlot <- renderPlot({
        ggplot(top_stations_data(), aes(x = reorder(start_station_name, N), y = N)) +
          geom_col(fill = "steelblue") +  # Use geom_col instead of geom_bar(stat = "identity") for simplicity
          coord_flip() +
          labs(title = "Top 10 Stations by Number of Rides", x = "Station Name", y = "Number of Rides") +
          custom_theme
      })
      
      # Station Density Plot
      output$stationDensityPlot <- renderPlot({
        density_data <- station_density_data()
        
        ggplot(density_data, aes(x = start_lng, y = start_lat)) +
          geom_jitter(aes(size = ride_count, color = ride_count), alpha = 0.6, width = 0.005, height = 0.005) +
          geom_density2d_filled(data = density_data, alpha = 0.4, bins = 10) +  # Use precomputed data
          scale_size_continuous(range = c(2, 10), name = "Ride Volume") +
          scale_color_viridis_c(option = "plasma", name = "Ride Volume") +
          labs(
            title = "Station Density and Ride Volume",
            x = "Longitude",
            y = "Latitude"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.position = "right"
          )
      })
      
      # Daily Ride Volume Area Chart
      output$areaChartPlot <- renderPlot({
        ggplot(daily_usage_data(), aes(x = date, y = N)) +
          geom_area(fill = "steelblue", alpha = 0.7) +
          geom_line(color = "darkblue", size = 1) +
          labs(title = "Daily Ride Volume", x = "Date", y = "Number of Rides") +
          custom_theme
      })
    }
    
    
    
    if (input$menu == "rides_dashboard") {
      # Rides Dashboard Outputs
      observeEvent(input$update_button_1, { # Ensure button ID matches the UI
        # Scatter Plot Output
        output$scatterPlot <- renderPlot({
          req(input$menu == "rides_dashboard") # Ensure the user is on the Rides Dashboard
          
          plot_data <- filtered_data() # Reactive filtered dataset
          
          ggplot(plot_data, aes(x = ride_duration, y = hour_of_day, color = rideable_type)) +
            geom_point(alpha = 0.6) +
            facet_wrap(~member_casual) +
            labs(
              title = "Ride Patterns by User Type",
              x = "Ride Duration (mins)",
              y = "Start Time (Hour)",
              color = "Bike Type"
            ) +
            custom_theme
        })
        
        # Summary Table Output
        output$summaryTable <- renderDT({
          req(input$menu == "rides_dashboard") # Ensure the user is on the Rides Dashboard
          
          filtered_data()[
            , .(
              avg_duration = round(mean(ride_duration, na.rm = TRUE), 2),
              ride_count = .N,
              bike_types = paste(unique(rideable_type), collapse = ", ")
            ), by = .(day_of_week, member_casual)
          ] %>%
            datatable(options = list(pageLength = 10, scrollX = TRUE, server = TRUE, processing = TRUE))
        })
      })
    }
    
    if (input$menu == "dynamic_dashboard") {
      # Dynamic Chart Rendering
      observeEvent(input$update_chart, {
        # Debounced and isolated inputs
        isolate({
          # Ensure at least one variable is selected for rows or columns
          if (length(input$row_var) == 0 && length(input$col_var) == 0) {
            output$dynamic_chart <- renderPlot({
              ggplot() + 
                theme_void() + 
                labs(title = "Please select at least one row or column variable to render a chart.")
            })
            return()
          }
          
          row_vars <- input$row_var
          col_vars <- input$col_var
          chart_type <- input$chart_type
          time_range <- input$time_range
          
          # Preprocess data
          selected_vars <- c(row_vars, col_vars, "started_at", "start_lat", "start_lng")
          plot_data <- data[, ..selected_vars, with = FALSE]
          
          # Filter data by time range
          if (!is.null(time_range)) {
            plot_data <- plot_data[as.Date(started_at) >= time_range[1] & as.Date(started_at) <= time_range[2]]
          }
          
          # Ensure variables are factors if needed
          lapply(row_vars, function(var) plot_data[[var]] <<- as.factor(plot_data[[var]]))
          lapply(col_vars, function(var) plot_data[[var]] <<- as.factor(plot_data[[var]]))
          
          # Reset previous chart
          output$dynamic_chart <- NULL
          
          # Render the new chart
          output$dynamic_chart <- renderPlot({
            if (chart_type == "Bar Chart") {
              # Check if the column used for y-axis (count) is numeric or missing
              if (!is.null(col_vars[1]) && is.numeric(plot_data[[col_vars[1]]])) {
                # If the data is already aggregated (numeric values), use stat = "identity"
                ggplot(plot_data, aes_string(x = row_vars[1], y = col_vars[1], fill = row_vars[1])) +
                  geom_bar(stat = "identity", position = "dodge") +
                  labs(
                    title = paste("Dynamic Bar Chart - Summarized", row_vars[1]),
                    x = row_vars[1],
                    y = col_vars[1],
                    fill = col_vars[1]
                  ) +
                  custom_theme +
                  scale_y_continuous(labels = scales::comma)
              } else {
                # Otherwise, use stat = "count" for raw counts
                ggplot(plot_data, aes_string(x = row_vars[1], fill = col_vars[1])) +
                  geom_bar(stat = "count", position = "dodge") +
                  labs(
                    title = paste("Dynamic Bar Chart - Count", row_vars[1]),
                    x = row_vars[1],
                    y = "Count",
                    fill = col_vars[1]
                  ) +
                  custom_theme +
                  scale_y_continuous(labels = scales::comma)
              }
            }
            else if (chart_type == "Line Chart") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                group_by_column <- row_vars[1]
                y_value_column <- col_vars[1]
                
                # Check if col_var is numeric
                if (!is.numeric(plot_data[[y_value_column]])) {
                  # Try to convert to numeric
                  plot_data[[y_value_column]] <- as.numeric(as.character(plot_data[[y_value_column]]))
                  
                  # If conversion results in NA, default to count
                  if (any(is.na(plot_data[[y_value_column]]))) {
                    aggregation_type <- "count"
                    plot_data[, y_value := .N, by = group_by_column]
                  } else {
                    aggregation_type <- "average"
                    plot_data[, y_value := mean(get(y_value_column), na.rm = TRUE), by = group_by_column]
                  }
                } else {
                  # Numeric column: Calculate average
                  aggregation_type <- "average"
                  plot_data[, y_value := mean(get(y_value_column), na.rm = TRUE), by = group_by_column]
                }
                
                # Filter data for plotting
                line_chart_data <- unique(plot_data[, .(x_value = get(group_by_column), y_value)])
                
                # Ensure the aggregated data has rows to plot
                if (nrow(line_chart_data) == 0) {
                  output$dynamic_chart <- renderPlot({
                    ggplot() + 
                      theme_void() + 
                      labs(title = "No data available for the selected inputs.")
                  })
                  return()
                }
                
                # Render the Line Chart
                output$dynamic_chart <- renderPlot({
                  ggplot(line_chart_data, aes(x = x_value, y = y_value, group = 1)) +
                    geom_line(color = "steelblue", size = 1) +
                    geom_point(color = "darkblue", size = 2) +
                    labs(
                      title = ifelse(aggregation_type == "count",
                                     "Dynamic Line Chart",
                                     paste("Dynamic Line Chart (Average of", y_value_column, ")")),
                      x = group_by_column,
                      y = ifelse(aggregation_type == "count", "Count", paste("Average of", y_value_column))
                    ) +
                    custom_theme +
                    scale_y_continuous(labels = scales::comma) +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
                })
              } else {
                # Handle case where row_vars or col_vars are not selected
                output$dynamic_chart <- renderPlot({
                  ggplot() + 
                    theme_void() + 
                    labs(title = "Line Chart requires both row and column variables.")
                })
              }
            }
            else if (chart_type == "Scatter Plot") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                # Dynamically extract x and y-axis variables
                x_axis <- row_vars[1]
                y_axis <- col_vars[1]
                
                # Check if the variables exist in the data
                if (!(x_axis %in% names(plot_data))) {
                  stop(paste("The column selected for the x-axis (", x_axis, ") does not exist in the data.", sep = ""))
                }
                if (!(y_axis %in% names(plot_data))) {
                  stop(paste("The column selected for the y-axis (", y_axis, ") does not exist in the data.", sep = ""))
                }
                
                # Dynamically process x-axis: Convert to numeric if possible or keep as categorical
                if (!is.numeric(plot_data[[x_axis]])) {
                  plot_data[[x_axis]] <- factor(plot_data[[x_axis]])  # Treat as categorical if not numeric
                }
                
                # Dynamically process y-axis: Aggregate counts for non-numeric columns
                if (!is.numeric(plot_data[[y_axis]])) {
                  y_axis_agg <- paste0(y_axis, "_count")  # Create a count column name
                  plot_data <- plot_data[, .N, by = .(x_value = get(x_axis), y_value = get(y_axis))]
                  setnames(plot_data, c("x_value", "y_value", "N"), c(x_axis, y_axis, y_axis_agg))
                  y_axis <- y_axis_agg  # Use the aggregated column for y-axis
                }
                
                # Filter out rows with NA values for x or y
                valid_data <- plot_data[!is.na(plot_data[[x_axis]]) & !is.na(plot_data[[y_axis]])]
                
                # Check if the dataset is empty after filtering
                if (nrow(valid_data) == 0) {
                  output$dynamic_chart <- renderPlot({
                    ggplot() + 
                      theme_void() + 
                      labs(title = "No valid data available for the selected inputs.")
                  })
                  return()
                }
                
                # Render the Scatter Plot
                output$dynamic_chart <- renderPlot({
                  ggplot(valid_data, aes_string(x = x_axis, y = y_axis)) +
                    geom_point(color = "steelblue", alpha = 0.6, size = 2) +
                    labs(
                      title = "Dynamic Scatter Plot",
                      x = x_axis,
                      y = y_axis
                    ) +
                    custom_theme +
                    scale_y_continuous(labels = scales::comma) +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if necessary
                })
              } else {
                output$dynamic_chart <- renderPlot({
                  ggplot() + 
                    theme_void() + 
                    labs(title = "Scatter Plot requires both row and column variables.")
                })
              }
            }
            
            else if (chart_type == "Histogram") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                x_axis <- col_vars[1]
                fill_var <- row_vars[1]
                
                # Check if the x-axis variable is numeric
                if (!is.numeric(plot_data[[x_axis]])) {
                  plot_data[[x_axis]] <- as.numeric(as.character(plot_data[[x_axis]]))
                  if (any(is.na(plot_data[[x_axis]]))) {
                    stop(paste("The column selected for the histogram (", x_axis, ") must be numeric. Non-numeric values detected.", sep = ""))
                  }
                }
                
                # Handle NA values for the fill variable
                if (!is.null(fill_var)) {
                  plot_data[[fill_var]] <- as.factor(plot_data[[fill_var]])  # Ensure fill_var is treated as a factor
                } else {
                  fill_var <- NULL
                }
                
                # Ensure the dataset is not empty after filtering
                valid_data <- plot_data[!is.na(plot_data[[x_axis]])]
                if (nrow(valid_data) == 0) {
                  output$dynamic_chart <- renderPlot({
                    ggplot() +
                      theme_void() +
                      labs(title = "No valid data available for the selected inputs.")
                  })
                  return()
                }
                
                # Render the histogram
                output$dynamic_chart <- renderPlot({
                  ggplot(valid_data, aes_string(x = x_axis, fill = fill_var)) +
                    geom_histogram(binwidth = 5, color = "white", alpha = 0.7, position = "identity") +
                    labs(
                      title = "Dynamic Histogram",
                      x = x_axis,
                      y = "Count",
                      fill = fill_var
                    ) +
                    custom_theme +
                    scale_y_continuous(labels = scales::comma) +
                    scale_fill_brewer(palette = "Set2")  # Adjust color palette if needed
                })
              } else {
                output$dynamic_chart <- renderPlot({
                  ggplot() +
                    theme_void() +
                    labs(title = "Histogram requires both row and column variables.")
                })
              }
            } else if (chart_type == "Stacked Bar Chart") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                x_axis <- row_vars[1]
                fill_var <- col_vars[1]
                
                # Check if the x-axis variable exists
                if (!(x_axis %in% names(plot_data))) {
                  stop(paste("The column selected for the x-axis (", x_axis, ") does not exist in the data.", sep = ""))
                }
                
                # Check if the fill variable exists
                if (!(fill_var %in% names(plot_data))) {
                  stop(paste("The column selected for the fill variable (", fill_var, ") does not exist in the data.", sep = ""))
                }
                
                # Ensure the x-axis variable is treated as a factor (categorical variable)
                plot_data[[x_axis]] <- as.factor(plot_data[[x_axis]])
                
                # Ensure the fill variable is treated as a factor
                plot_data[[fill_var]] <- as.factor(plot_data[[fill_var]])
                
                # Remove NA values in the x-axis and fill variable
                valid_data <- plot_data[!is.na(plot_data[[x_axis]]) & !is.na(plot_data[[fill_var]])]
                
                # Check if the dataset is empty after filtering
                if (nrow(valid_data) == 0) {
                  output$dynamic_chart <- renderPlot({
                    ggplot() +
                      theme_void() +
                      labs(title = "No valid data available for the selected inputs.")
                  })
                  return()
                }
                
                # Render the stacked bar chart
                output$dynamic_chart <- renderPlot({
                  ggplot(valid_data, aes_string(x = x_axis, fill = fill_var)) +
                    geom_bar(position = "stack") +
                    labs(
                      title = "Dynamic Stacked Bar Chart",
                      x = x_axis,
                      y = "Count",
                      fill = fill_var
                    ) +
                    custom_theme +
                    scale_y_continuous(labels = scales::comma) +
                    scale_fill_brewer(palette = "Set2")  # Optional: Customize color palette
                })
              } else {
                output$dynamic_chart <- renderPlot({
                  ggplot() +
                    theme_void() +
                    labs(title = "Stacked Bar Chart requires both row and column variables.")
                })
              }
            } else if (chart_type == "Box Plot") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                x_axis <- row_vars[1]
                y_axis <- col_vars[1]
                
                # Check if the x-axis and y-axis variables exist
                if (!(x_axis %in% names(plot_data))) {
                  stop(paste("The column selected for the x-axis (", x_axis, ") does not exist in the data.", sep = ""))
                }
                if (!(y_axis %in% names(plot_data))) {
                  stop(paste("The column selected for the y-axis (", y_axis, ") does not exist in the data.", sep = ""))
                }
                
                # Ensure the x-axis variable is treated as a factor (categorical variable)
                plot_data[[x_axis]] <- as.factor(plot_data[[x_axis]])
                
                # Ensure the y-axis variable is numeric
                if (!is.numeric(plot_data[[y_axis]])) {
                  plot_data[[y_axis]] <- as.numeric(as.character(plot_data[[y_axis]]))
                  if (any(is.na(plot_data[[y_axis]]))) {
                    stop(paste("The column selected for the y-axis (", y_axis, ") must be numeric. Non-numeric values detected.", sep = ""))
                  }
                }
                
                # Remove rows with NA values in x-axis or y-axis
                valid_data <- plot_data[!is.na(plot_data[[x_axis]]) & !is.na(plot_data[[y_axis]])]
                
                # Check if the dataset is empty after filtering
                if (nrow(valid_data) == 0) {
                  output$dynamic_chart <- renderPlot({
                    ggplot() +
                      theme_void() +
                      labs(title = "No valid data available for the selected inputs.")
                  })
                  return()
                }
                
                # Render the box plot
                output$dynamic_chart <- renderPlot({
                  ggplot(valid_data, aes_string(x = x_axis, y = y_axis, fill = x_axis)) +
                    geom_boxplot(outlier.colour = "red", outlier.size = 2, outlier.shape = 16) +  # Customize outliers
                    labs(
                      title = "Dynamic Box Plot",
                      x = x_axis,
                      y = y_axis,
                      fill = x_axis
                    ) +
                    custom_theme +
                    scale_fill_brewer(palette = "Set2")  # Optional: Customize color palette
                })
              } else {
                output$dynamic_chart <- renderPlot({
                  ggplot() +
                    theme_void() +
                    labs(title = "Box Plot requires both row and column variables.")
                })
              }
            } else if (chart_type == "Density Plot") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                x_axis <- col_vars[1]
                fill_var <- row_vars[1]
                
                # Check if the x-axis variable exists
                if (!(x_axis %in% names(plot_data))) {
                  stop(paste("The column selected for the x-axis (", x_axis, ") does not exist in the data.", sep = ""))
                }
                
                # Check if the fill variable exists
                if (!(fill_var %in% names(plot_data))) {
                  stop(paste("The column selected for the fill variable (", fill_var, ") does not exist in the data.", sep = ""))
                }
                
                # Handle non-numeric x_axis
                if (!is.numeric(plot_data[[x_axis]])) {
                  # Convert to numeric if possible
                  plot_data[[x_axis]] <- as.numeric(as.character(plot_data[[x_axis]]))
                  if (any(is.na(plot_data[[x_axis]]))) {
                    warning(paste("The column selected for the x-axis (", x_axis, ") contains non-numeric values. Switching to counts.", sep = ""))
                    # Aggregate counts for categorical x_axis
                    x_axis_agg <- paste0(x_axis, "_count")
                    plot_data <- plot_data[, .(count = .N), by = .(fill_var = get(fill_var), x_axis = get(x_axis))]
                    names(plot_data) <- c(fill_var, x_axis, x_axis_agg)
                    x_axis <- x_axis_agg
                  }
                }
                
                # Ensure the fill variable is treated as a factor
                plot_data[[fill_var]] <- as.factor(plot_data[[fill_var]])
                
                # Remove rows with NA values in the x-axis or fill variable
                valid_data <- plot_data[!is.na(plot_data[[x_axis]]) & !is.na(plot_data[[fill_var]])]
                
                # Check if the dataset is empty after filtering
                if (nrow(valid_data) == 0) {
                  output$dynamic_chart <- renderPlot({
                    ggplot() +
                      theme_void() +
                      labs(title = "No valid data available for the selected inputs.")
                  })
                  return()
                }
                
                # Render the density plot
                output$dynamic_chart <- renderPlot({
                  ggplot(valid_data, aes_string(x = x_axis, fill = fill_var)) +
                    geom_density(alpha = 0.7, adjust = 1.5) +  # Smoothing adjustment for density
                    labs(
                      title = "Dynamic Density Plot",
                      x = x_axis,
                      y = "Density",
                      fill = fill_var
                    ) +
                    custom_theme +
                    scale_fill_brewer(palette = "Set2")  # Optional: Customize color palette
                })
              } else {
                # Handle case where required variables are missing
                output$dynamic_chart <- renderPlot({
                  ggplot() +
                    theme_void() +
                    labs(title = "Density Plot requires both row and column variables.")
                })
              }
            }
            else if (chart_type == "Heatmap") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                # Ensure variables are factors if they are categorical
                if (!is.numeric(plot_data[[row_vars[1]]])) {
                  plot_data[[row_vars[1]]] <- as.factor(plot_data[[row_vars[1]]])
                }
                if (!is.numeric(plot_data[[col_vars[1]]])) {
                  plot_data[[col_vars[1]]] <- as.factor(plot_data[[col_vars[1]]])
                }
                
                # Precompute counts for categorical variables
                if (is.factor(plot_data[[row_vars[1]]]) && is.factor(plot_data[[col_vars[1]]])) {
                  heatmap_data <- plot_data[, .N, by = c(row_vars[1], col_vars[1])]
                  ggplot(heatmap_data, aes_string(x = row_vars[1], y = col_vars[1], fill = "N")) +
                    geom_tile() +
                    labs(title = "Dynamic Heatmap", x = row_vars[1], y = col_vars[1], fill = "Count") +
                    custom_theme +
                    scale_fill_gradient(low = "blue", high = "red")  # Adjust colors as needed
                  
                } else {
                  # Handle numeric variables by binning
                  heatmap_data <- plot_data[, .(x_bin = cut(get(row_vars[1]), breaks = 10),
                                                y_bin = cut(get(col_vars[1]), breaks = 10)), 
                                            by = 1:nrow(plot_data)]
                  ggplot(heatmap_data, aes(x = x_bin, y = y_bin)) +
                    geom_tile(aes(fill = ..count..), stat = "bin2d") +
                    labs(title = "Dynamic Heatmap (Binned)", x = row_vars[1], y = col_vars[1], fill = "Count") +
                    custom_theme +
                    scale_fill_gradient(low = "blue", high = "red")
                }
              } else {
                ggplot() + 
                  theme_void() + 
                  labs(title = "Heatmap requires both row and column variables.")
              }
            } else {
              ggplot() + 
                theme_void() + 
                labs(title = "Unsupported combination of chart type and selected variables.")
            }
          })
        })
      })
    }
  })
}

# Run App
shinyApp(ui = ui, server = server)
=======
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(lubridate)
library(treemapify)
library(scales)
library(data.table)
library(DT)
library(RColorBrewer)
library(future)
plan(multisession)
library(conflicted)
library(plotly)


# Preprocess the data at app startup
data <- fread("./data/bike.csv")
data <- data[
  !is.na(started_at) & !is.na(ended_at),
  .(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    ride_duration = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = factor(lubridate::wday(started_at), 
                         levels = 1:7, 
                         labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
    hour_of_day = lubridate::hour(as.POSIXct(started_at)),
    rideable_type,
    member_casual,
    start_station_name,
    end_station_name,
    start_lat,
    start_lng,
    end_lat,
    end_lng,
    ride_id,
    start_station_id,
    end_station_id
  )
][ride_duration > 0 & ride_duration <= 120]

# Precompute top stations at startup for faster rendering
top_stations <- reactive({
  data[!is.na(start_station_name), .N, by = start_station_name][order(-N)][1:10]
})

# Precompute station density data at startup
station_density <- reactive({
  data[!is.na(start_lat) & !is.na(start_lng), 
       .(ride_count = .N), 
       by = .(start_station_name, start_lat, start_lng)]
})

# Precompute daily usage data at startup
daily_usage <- reactive({
  data[, .N, by = .(date = as.Date(started_at))]
})

# Convert to data.table for faster filtering
data <- as.data.table(data)

# Cache data for reuse across plots
top_stations_data <- reactive(top_stations())
station_density_data <- reactive(station_density())
daily_usage_data <- reactive(daily_usage())

# Define consistent theme for all plots
custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size =20, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "top"
  )

# Define UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body, html { margin: 0; padding: 0; overflow-x: hidden; }
      .burger-menu { position: fixed; top: 10px; left: 10px; z-index: 1000; cursor: pointer; }
      .burger-line { width: 30px; height: 4px; background-color: #000000; margin: 6px 0; }
      .menu-panel { position: fixed; top: 0; left: -250px; width: 250px; height: 100%; background-color: #FFF; z-index: 999;
                    box-shadow: 2px 0 5px rgba(0,0,0,0.5); transition: left 0.3s ease-in-out; overflow-y: auto; padding: 20px; padding-top: 60px; }
      .menu-panel.open { left: 0; }
      .menu-item { margin: 15px 0; font-size: 18px; cursor: pointer; color: #333; }
      .menu-item:hover { text-decoration: underline; }
      .content-container {
      /*background-color: #FFF !important;*/ /* Light blue background */
      margin: 0;
      height: 100%;
      overflow: hidden;
    }
      .content-container.shift { margin-left: 250px;}
      .fullscreen-image { position: absolute; top: 0; left: 0; width: 100%; height: 100vh; object-fit: cover; z-index: -1; }
    "))
  ),
  div(
    class = "burger-menu",
    onclick = "document.querySelector('.menu-panel').classList.toggle('open');
               document.querySelector('.content-container').classList.toggle('shift');",
    div(class = "burger-line"), div(class = "burger-line"), div(class = "burger-line")
  ),
  div(
    class = "menu-panel",
    div(class = "menu-item", onclick = "Shiny.setInputValue('menu', 'home', {priority: 'event'})", tags$span("Home")),
    div(class = "menu-item", onclick = "Shiny.setInputValue('menu', 'about', {priority: 'event'})", tags$span("About")),
    div(class = "menu-item", onclick = "Shiny.setInputValue('menu', 'user_behavior_analysis', {priority: 'event'})", tags$span("User Behavior Analysis")),
    div(class = "menu-item", onclick = "Shiny.setInputValue('menu', 'station_performance', {priority: 'event'})", tags$span("Station Performance")),
    div(class = "menu-item", onclick = "Shiny.setInputValue('menu', 'rides_dashboard', {priority: 'event'})", tags$span("Rides Dashboard")),
    div(class = "menu-item", onclick = "Shiny.setInputValue('menu', 'dynamic_dashboard', {priority: 'event'})", tags$span("Dynamic Dashboard"))
    
  ),
  div(class = "content-container", uiOutput("main_content"))
)

# Define Server
server <- function(input, output, session) {
  current_page <- reactiveVal("home")
  
  observeEvent(input$menu, {
    current_page(input$menu)
  })
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    req(input$timeline_slider)
    timeline_range <- input$timeline_slider
    bike_filter <- input$bike_type
    
    result <- data[started_at >= as.POSIXct(timeline_range[1]) & started_at <= as.POSIXct(timeline_range[2])]
    if (bike_filter != "All") {
      result <- result[rideable_type %in% bike_filter]
    }
    result
  })
  
  output$main_content <- renderUI({
    if (current_page() == "home") {
      tags$div(
        tags$img(src = "DDBA.jpg", class = "fullscreen-image"),
        tags$div(style = "position: relative; text-align: center; padding: 50px;")
      )
    } else if (current_page() == "about") {
      tags$div(
        style = "text-align: center; padding: 50px;",
        tags$h1("About This App"),
        tags$p("Details about the app.")
      )
    } else if (current_page() == "user_behavior_analysis") {
      tags$div(
        style = "text-align: center; padding: 20px;",
        tags$h1("User Behavior Analysis"),
        tags$div(
          style = "display: flex; justify-content: space-between; flex-wrap: wrap; gap: 20px;",
          tags$div(style = "flex: 1 1 calc(50% - 10px); max-width: 50%;", plotOutput("avgRidePlot")),
          tags$div(style = "flex: 1 1 calc(50% - 10px); max-width: 50%;", plotOutput("rideFrequencyPlot")),
          tags$div(style = "flex: 1 1 calc(50% - 10px); max-width: 50%;", plotOutput("diversityPlot")),
          tags$div(style = "flex: 1 1 calc(50% - 10px); max-width: 50%;", plotOutput("bikeUsagePlot"))
        )
      )
    } else if (current_page() == "station_performance") {
      tags$div(
        style = "text-align: center; padding: 20px;",
        tags$h1("Station Performance"),
        tags$div(
          style = "display: flex; justify-content: space-between; flex-wrap: wrap; gap: 20px;",
          # First plot
          tags$div(
            style = "flex: 1 1 calc(50% - 20px); max-width: 50%;",
            plotOutput("topStationsPlot")
          ),
          # Second plot
          tags$div(
            style = "flex: 1 1 calc(50% - 20px); max-width: 50%;",
            plotOutput("stationDensityPlot")
          ),
          # Third plot (starts new row due to flex-wrap)
          tags$div(
            style = "flex: 1 1 calc(50% - 20px); max-width: 50%;",
            plotOutput("areaChartPlot")
          )
        )
      )
    }
    else if (current_page() == "rides_dashboard") {
      tags$div(
        style = "text-align: center; padding: 20px;",
        tags$h1("Rides Dashboard"),
        
        # Timeline slider and filter side by side
        tags$div(
          style = "display: flex; justify-content: center; align-items: center; gap: 5%; margin-bottom: 20px;",
          
          # Timeline Slider
          sliderInput(
            "timeline_slider",
            "Select Timeline",
            min = min(data$started_at),
            max = max(data$started_at),
            value = c(min(data$started_at), max(data$started_at)),
            timeFormat = "%Y-%m-%d",
            step = 86400,
            width = "30%" # Proportional width for responsiveness
          ),
          
          # Bike Type Filter
          selectInput(
            "bike_type",
            "Filter by Bike Type",
            choices = c("All", unique(data$rideable_type)),
            selected = "All",
            multiple = TRUE,
            width = "28%" # Proportional width for responsiveness
          ),
          
          # Update Button
          actionButton(
            "update_button_1",
            "Update",
            style = "height: 38px; padding: 5px 10px; background-color: #007BFF; color: white; border: none; border-radius: 5px; cursor: pointer;"
          )
        ),
        
        # Scatter plot and summary table side by side
        tags$div(
          style = "display: flex; justify-content: space-between; flex-wrap: wrap; gap: 20px;",
          
          # Scatter plot
          tags$div(
            style = "flex: 1 1 50%; max-width: 50%;", # Increased width for scatter plot
            plotOutput("scatterPlot", height = "500px") # Adjust height if needed
          ),
          
          # Summary table with a name and border
          tags$div(
            style = "flex: 1 1 48%; max-width: 48%; border: 2px solid #000; border-radius: 5px; padding: 10px; height: 55vh; overflow-y: auto;", # Reduced height and added scrolling
            tags$h3("Summary Table", style = "text-align: center; margin-bottom: 10px;"),
            DTOutput("summaryTable")
          )
        )
        
      )
    }
    else if (current_page() == "dynamic_dashboard") {
      tags$div(
        style = "text-align: center; padding: 20px;",
        tags$h1("Dynamic Dashboard"),
        fluidRow(
          column(3, shinyWidgets::pickerInput(
            inputId = "row_var", 
            label = "Select Row Variables", 
            choices = names(data), 
            options = list(`actions-box` = TRUE, `live-search` = TRUE), 
            multiple = TRUE
          )),
          column(3, shinyWidgets::pickerInput(
            inputId = "col_var", 
            label = "Select Column Variables", 
            choices = names(data), 
            options = list(`actions-box` = TRUE, `live-search` = TRUE), 
            multiple = TRUE
          )),
          column(3, selectInput("chart_type", "Select Chart Type", 
                                choices = c("Bar Chart", "Line Chart", "Scatter Plot", "Histogram", "Stacked Bar Chart", 
                                            "Box Plot", "Density Plot", "Heatmap"))),
          column(3, sliderInput("time_range", "Select Time Range", 
                                min = as.Date(min(data$started_at)), 
                                max = as.Date(max(data$started_at)), 
                                value = c(as.Date(min(data$started_at)), as.Date(max(data$started_at))),
                                timeFormat = "%Y-%m-%d"))
        ),
        actionButton("update_chart", "Update Chart"),
        plotOutput("dynamic_chart")
      )
    }
  })
  
  observeEvent(input$menu, {
    if (input$menu == "user_behavior_analysis") {
      
      # User Behavior Analysis Outputs
      output$avgRidePlot <- renderPlot({
        avg_data <- data[, .(avg_duration = mean(ride_duration)), by = member_casual]
        ggplot(avg_data, aes(x = member_casual, y = avg_duration, fill = member_casual)) +
          geom_bar(stat = "identity") +
          labs(title = "Average Ride Duration by User Type", x = "User Type", y = "Duration (mins)", fill = "User Type") +
          custom_theme
      })
      
      output$rideFrequencyPlot <- renderPlot({
        freq_data <- data[, .N, by = .(day_of_week, member_casual)]
        ggplot(freq_data, aes(x = day_of_week, y = N, fill = member_casual)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Ride Frequency by Day of Week", x = "Day of Week", y = "Number of Rides", fill = "User Type") +
          custom_theme +
          scale_y_continuous(labels = scales::comma)
      })
      
      output$diversityPlot <- renderPlot({
        # Summarize data and filter out NA values
        diversity_data <- data[!is.na(start_station_name) & !is.na(member_casual), 
                               .N, by = .(start_station_name, member_casual)][order(-N)][1:20]
        
        # Create the bar chart
        ggplot(diversity_data, aes(x = reorder(start_station_name, -N), y = N, fill = member_casual)) +
          geom_bar(stat = "identity", position = "dodge") +
          coord_flip() +
          labs(
            title = "Top 20 Stations by User Type",
            x = "Station Name",
            y = "Number of Rides",
            fill = "User Type"
          ) +
          custom_theme +
          theme(axis.text.y = element_text(size = 8))
      })
      
      output$bikeUsagePlot <- renderPlot({
        bike_data <- data[, .N, by = .(rideable_type, member_casual)]
        ggplot(bike_data, aes(x = member_casual, y = N, fill = rideable_type)) +
          geom_bar(stat = "identity", position = "stack") +
          labs(title = "Bike Type Usage by User Group", x = "User Type", y = "Number of Rides", fill = "Bike Type") +
          custom_theme +
          scale_y_continuous(labels = scales::comma)
      })
    }
    
    
    if (input$menu == "station_performance") {
      
      # Top Stations Plot
      output$topStationsPlot <- renderPlot({
        ggplot(top_stations_data(), aes(x = reorder(start_station_name, N), y = N)) +
          geom_col(fill = "steelblue") +  # Use geom_col instead of geom_bar(stat = "identity") for simplicity
          coord_flip() +
          labs(title = "Top 10 Stations by Number of Rides", x = "Station Name", y = "Number of Rides") +
          custom_theme
      })
      
      # Station Density Plot
      output$stationDensityPlot <- renderPlot({
        density_data <- station_density_data()
        
        ggplot(density_data, aes(x = start_lng, y = start_lat)) +
          geom_jitter(aes(size = ride_count, color = ride_count), alpha = 0.6, width = 0.005, height = 0.005) +
          geom_density2d_filled(data = density_data, alpha = 0.4, bins = 10) +  # Use precomputed data
          scale_size_continuous(range = c(2, 10), name = "Ride Volume") +
          scale_color_viridis_c(option = "plasma", name = "Ride Volume") +
          labs(
            title = "Station Density and Ride Volume",
            x = "Longitude",
            y = "Latitude"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.position = "right"
          )
      })
      
      # Daily Ride Volume Area Chart
      output$areaChartPlot <- renderPlot({
        ggplot(daily_usage_data(), aes(x = date, y = N)) +
          geom_area(fill = "steelblue", alpha = 0.7) +
          geom_line(color = "darkblue", size = 1) +
          labs(title = "Daily Ride Volume", x = "Date", y = "Number of Rides") +
          custom_theme
      })
    }
    
    
    
    if (input$menu == "rides_dashboard") {
      # Rides Dashboard Outputs
      observeEvent(input$update_button_1, { # Ensure button ID matches the UI
        # Scatter Plot Output
        output$scatterPlot <- renderPlot({
          req(input$menu == "rides_dashboard") # Ensure the user is on the Rides Dashboard
          
          plot_data <- filtered_data() # Reactive filtered dataset
          
          ggplot(plot_data, aes(x = ride_duration, y = hour_of_day, color = rideable_type)) +
            geom_point(alpha = 0.6) +
            facet_wrap(~member_casual) +
            labs(
              title = "Ride Patterns by User Type",
              x = "Ride Duration (mins)",
              y = "Start Time (Hour)",
              color = "Bike Type"
            ) +
            custom_theme
        })
        
        # Summary Table Output
        output$summaryTable <- renderDT({
          req(input$menu == "rides_dashboard") # Ensure the user is on the Rides Dashboard
          
          filtered_data()[
            , .(
              avg_duration = round(mean(ride_duration, na.rm = TRUE), 2),
              ride_count = .N,
              bike_types = paste(unique(rideable_type), collapse = ", ")
            ), by = .(day_of_week, member_casual)
          ] %>%
            datatable(options = list(pageLength = 10, scrollX = TRUE, server = TRUE, processing = TRUE))
        })
      })
    }
    
    if (input$menu == "dynamic_dashboard") {
      # Dynamic Chart Rendering
      observeEvent(input$update_chart, {
        # Debounced and isolated inputs
        isolate({
          # Ensure at least one variable is selected for rows or columns
          if (length(input$row_var) == 0 && length(input$col_var) == 0) {
            output$dynamic_chart <- renderPlot({
              ggplot() + 
                theme_void() + 
                labs(title = "Please select at least one row or column variable to render a chart.")
            })
            return()
          }
          
          row_vars <- input$row_var
          col_vars <- input$col_var
          chart_type <- input$chart_type
          time_range <- input$time_range
          
          # Preprocess data
          selected_vars <- c(row_vars, col_vars, "started_at", "start_lat", "start_lng")
          plot_data <- data[, ..selected_vars, with = FALSE]
          
          # Filter data by time range
          if (!is.null(time_range)) {
            plot_data <- plot_data[as.Date(started_at) >= time_range[1] & as.Date(started_at) <= time_range[2]]
          }
          
          # Ensure variables are factors if needed
          lapply(row_vars, function(var) plot_data[[var]] <<- as.factor(plot_data[[var]]))
          lapply(col_vars, function(var) plot_data[[var]] <<- as.factor(plot_data[[var]]))
          
          # Reset previous chart
          output$dynamic_chart <- NULL
          
          # Render the new chart
          output$dynamic_chart <- renderPlot({
            if (chart_type == "Bar Chart") {
              # Check if the column used for y-axis (count) is numeric or missing
              if (!is.null(col_vars[1]) && is.numeric(plot_data[[col_vars[1]]])) {
                # If the data is already aggregated (numeric values), use stat = "identity"
                ggplot(plot_data, aes_string(x = row_vars[1], y = col_vars[1], fill = row_vars[1])) +
                  geom_bar(stat = "identity", position = "dodge") +
                  labs(
                    title = paste("Dynamic Bar Chart - Summarized", row_vars[1]),
                    x = row_vars[1],
                    y = col_vars[1],
                    fill = col_vars[1]
                  ) +
                  custom_theme +
                  scale_y_continuous(labels = scales::comma)
              } else {
                # Otherwise, use stat = "count" for raw counts
                ggplot(plot_data, aes_string(x = row_vars[1], fill = col_vars[1])) +
                  geom_bar(stat = "count", position = "dodge") +
                  labs(
                    title = paste("Dynamic Bar Chart - Count", row_vars[1]),
                    x = row_vars[1],
                    y = "Count",
                    fill = col_vars[1]
                  ) +
                  custom_theme +
                  scale_y_continuous(labels = scales::comma)
              }
            }
            else if (chart_type == "Line Chart") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                group_by_column <- row_vars[1]
                y_value_column <- col_vars[1]
                
                # Check if col_var is numeric
                if (!is.numeric(plot_data[[y_value_column]])) {
                  # Try to convert to numeric
                  plot_data[[y_value_column]] <- as.numeric(as.character(plot_data[[y_value_column]]))
                  
                  # If conversion results in NA, default to count
                  if (any(is.na(plot_data[[y_value_column]]))) {
                    aggregation_type <- "count"
                    plot_data[, y_value := .N, by = group_by_column]
                  } else {
                    aggregation_type <- "average"
                    plot_data[, y_value := mean(get(y_value_column), na.rm = TRUE), by = group_by_column]
                  }
                } else {
                  # Numeric column: Calculate average
                  aggregation_type <- "average"
                  plot_data[, y_value := mean(get(y_value_column), na.rm = TRUE), by = group_by_column]
                }
                
                # Filter data for plotting
                line_chart_data <- unique(plot_data[, .(x_value = get(group_by_column), y_value)])
                
                # Ensure the aggregated data has rows to plot
                if (nrow(line_chart_data) == 0) {
                  output$dynamic_chart <- renderPlot({
                    ggplot() + 
                      theme_void() + 
                      labs(title = "No data available for the selected inputs.")
                  })
                  return()
                }
                
                # Render the Line Chart
                output$dynamic_chart <- renderPlot({
                  ggplot(line_chart_data, aes(x = x_value, y = y_value, group = 1)) +
                    geom_line(color = "steelblue", size = 1) +
                    geom_point(color = "darkblue", size = 2) +
                    labs(
                      title = ifelse(aggregation_type == "count",
                                     "Dynamic Line Chart",
                                     paste("Dynamic Line Chart (Average of", y_value_column, ")")),
                      x = group_by_column,
                      y = ifelse(aggregation_type == "count", "Count", paste("Average of", y_value_column))
                    ) +
                    custom_theme +
                    scale_y_continuous(labels = scales::comma) +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
                })
              } else {
                # Handle case where row_vars or col_vars are not selected
                output$dynamic_chart <- renderPlot({
                  ggplot() + 
                    theme_void() + 
                    labs(title = "Line Chart requires both row and column variables.")
                })
              }
            }
            else if (chart_type == "Scatter Plot") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                # Dynamically extract x and y-axis variables
                x_axis <- row_vars[1]
                y_axis <- col_vars[1]
                
                # Check if the variables exist in the data
                if (!(x_axis %in% names(plot_data))) {
                  stop(paste("The column selected for the x-axis (", x_axis, ") does not exist in the data.", sep = ""))
                }
                if (!(y_axis %in% names(plot_data))) {
                  stop(paste("The column selected for the y-axis (", y_axis, ") does not exist in the data.", sep = ""))
                }
                
                # Dynamically process x-axis: Convert to numeric if possible or keep as categorical
                if (!is.numeric(plot_data[[x_axis]])) {
                  plot_data[[x_axis]] <- factor(plot_data[[x_axis]])  # Treat as categorical if not numeric
                }
                
                # Dynamically process y-axis: Aggregate counts for non-numeric columns
                if (!is.numeric(plot_data[[y_axis]])) {
                  y_axis_agg <- paste0(y_axis, "_count")  # Create a count column name
                  plot_data <- plot_data[, .N, by = .(x_value = get(x_axis), y_value = get(y_axis))]
                  setnames(plot_data, c("x_value", "y_value", "N"), c(x_axis, y_axis, y_axis_agg))
                  y_axis <- y_axis_agg  # Use the aggregated column for y-axis
                }
                
                # Filter out rows with NA values for x or y
                valid_data <- plot_data[!is.na(plot_data[[x_axis]]) & !is.na(plot_data[[y_axis]])]
                
                # Check if the dataset is empty after filtering
                if (nrow(valid_data) == 0) {
                  output$dynamic_chart <- renderPlot({
                    ggplot() + 
                      theme_void() + 
                      labs(title = "No valid data available for the selected inputs.")
                  })
                  return()
                }
                
                # Render the Scatter Plot
                output$dynamic_chart <- renderPlot({
                  ggplot(valid_data, aes_string(x = x_axis, y = y_axis)) +
                    geom_point(color = "steelblue", alpha = 0.6, size = 2) +
                    labs(
                      title = "Dynamic Scatter Plot",
                      x = x_axis,
                      y = y_axis
                    ) +
                    custom_theme +
                    scale_y_continuous(labels = scales::comma) +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if necessary
                })
              } else {
                output$dynamic_chart <- renderPlot({
                  ggplot() + 
                    theme_void() + 
                    labs(title = "Scatter Plot requires both row and column variables.")
                })
              }
            }
            
            else if (chart_type == "Histogram") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                x_axis <- col_vars[1]
                fill_var <- row_vars[1]
                
                # Check if the x-axis variable is numeric
                if (!is.numeric(plot_data[[x_axis]])) {
                  plot_data[[x_axis]] <- as.numeric(as.character(plot_data[[x_axis]]))
                  if (any(is.na(plot_data[[x_axis]]))) {
                    stop(paste("The column selected for the histogram (", x_axis, ") must be numeric. Non-numeric values detected.", sep = ""))
                  }
                }
                
                # Handle NA values for the fill variable
                if (!is.null(fill_var)) {
                  plot_data[[fill_var]] <- as.factor(plot_data[[fill_var]])  # Ensure fill_var is treated as a factor
                } else {
                  fill_var <- NULL
                }
                
                # Ensure the dataset is not empty after filtering
                valid_data <- plot_data[!is.na(plot_data[[x_axis]])]
                if (nrow(valid_data) == 0) {
                  output$dynamic_chart <- renderPlot({
                    ggplot() +
                      theme_void() +
                      labs(title = "No valid data available for the selected inputs.")
                  })
                  return()
                }
                
                # Render the histogram
                output$dynamic_chart <- renderPlot({
                  ggplot(valid_data, aes_string(x = x_axis, fill = fill_var)) +
                    geom_histogram(binwidth = 5, color = "white", alpha = 0.7, position = "identity") +
                    labs(
                      title = "Dynamic Histogram",
                      x = x_axis,
                      y = "Count",
                      fill = fill_var
                    ) +
                    custom_theme +
                    scale_y_continuous(labels = scales::comma) +
                    scale_fill_brewer(palette = "Set2")  # Adjust color palette if needed
                })
              } else {
                output$dynamic_chart <- renderPlot({
                  ggplot() +
                    theme_void() +
                    labs(title = "Histogram requires both row and column variables.")
                })
              }
            } else if (chart_type == "Stacked Bar Chart") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                x_axis <- row_vars[1]
                fill_var <- col_vars[1]
                
                # Check if the x-axis variable exists
                if (!(x_axis %in% names(plot_data))) {
                  stop(paste("The column selected for the x-axis (", x_axis, ") does not exist in the data.", sep = ""))
                }
                
                # Check if the fill variable exists
                if (!(fill_var %in% names(plot_data))) {
                  stop(paste("The column selected for the fill variable (", fill_var, ") does not exist in the data.", sep = ""))
                }
                
                # Ensure the x-axis variable is treated as a factor (categorical variable)
                plot_data[[x_axis]] <- as.factor(plot_data[[x_axis]])
                
                # Ensure the fill variable is treated as a factor
                plot_data[[fill_var]] <- as.factor(plot_data[[fill_var]])
                
                # Remove NA values in the x-axis and fill variable
                valid_data <- plot_data[!is.na(plot_data[[x_axis]]) & !is.na(plot_data[[fill_var]])]
                
                # Check if the dataset is empty after filtering
                if (nrow(valid_data) == 0) {
                  output$dynamic_chart <- renderPlot({
                    ggplot() +
                      theme_void() +
                      labs(title = "No valid data available for the selected inputs.")
                  })
                  return()
                }
                
                # Render the stacked bar chart
                output$dynamic_chart <- renderPlot({
                  ggplot(valid_data, aes_string(x = x_axis, fill = fill_var)) +
                    geom_bar(position = "stack") +
                    labs(
                      title = "Dynamic Stacked Bar Chart",
                      x = x_axis,
                      y = "Count",
                      fill = fill_var
                    ) +
                    custom_theme +
                    scale_y_continuous(labels = scales::comma) +
                    scale_fill_brewer(palette = "Set2")  # Optional: Customize color palette
                })
              } else {
                output$dynamic_chart <- renderPlot({
                  ggplot() +
                    theme_void() +
                    labs(title = "Stacked Bar Chart requires both row and column variables.")
                })
              }
            } else if (chart_type == "Box Plot") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                x_axis <- row_vars[1]
                y_axis <- col_vars[1]
                
                # Check if the x-axis and y-axis variables exist
                if (!(x_axis %in% names(plot_data))) {
                  stop(paste("The column selected for the x-axis (", x_axis, ") does not exist in the data.", sep = ""))
                }
                if (!(y_axis %in% names(plot_data))) {
                  stop(paste("The column selected for the y-axis (", y_axis, ") does not exist in the data.", sep = ""))
                }
                
                # Ensure the x-axis variable is treated as a factor (categorical variable)
                plot_data[[x_axis]] <- as.factor(plot_data[[x_axis]])
                
                # Ensure the y-axis variable is numeric
                if (!is.numeric(plot_data[[y_axis]])) {
                  plot_data[[y_axis]] <- as.numeric(as.character(plot_data[[y_axis]]))
                  if (any(is.na(plot_data[[y_axis]]))) {
                    stop(paste("The column selected for the y-axis (", y_axis, ") must be numeric. Non-numeric values detected.", sep = ""))
                  }
                }
                
                # Remove rows with NA values in x-axis or y-axis
                valid_data <- plot_data[!is.na(plot_data[[x_axis]]) & !is.na(plot_data[[y_axis]])]
                
                # Check if the dataset is empty after filtering
                if (nrow(valid_data) == 0) {
                  output$dynamic_chart <- renderPlot({
                    ggplot() +
                      theme_void() +
                      labs(title = "No valid data available for the selected inputs.")
                  })
                  return()
                }
                
                # Render the box plot
                output$dynamic_chart <- renderPlot({
                  ggplot(valid_data, aes_string(x = x_axis, y = y_axis, fill = x_axis)) +
                    geom_boxplot(outlier.colour = "red", outlier.size = 2, outlier.shape = 16) +  # Customize outliers
                    labs(
                      title = "Dynamic Box Plot",
                      x = x_axis,
                      y = y_axis,
                      fill = x_axis
                    ) +
                    custom_theme +
                    scale_fill_brewer(palette = "Set2")  # Optional: Customize color palette
                })
              } else {
                output$dynamic_chart <- renderPlot({
                  ggplot() +
                    theme_void() +
                    labs(title = "Box Plot requires both row and column variables.")
                })
              }
            } else if (chart_type == "Density Plot") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                x_axis <- col_vars[1]
                fill_var <- row_vars[1]
                
                # Check if the x-axis variable exists
                if (!(x_axis %in% names(plot_data))) {
                  stop(paste("The column selected for the x-axis (", x_axis, ") does not exist in the data.", sep = ""))
                }
                
                # Check if the fill variable exists
                if (!(fill_var %in% names(plot_data))) {
                  stop(paste("The column selected for the fill variable (", fill_var, ") does not exist in the data.", sep = ""))
                }
                
                # Handle non-numeric x_axis
                if (!is.numeric(plot_data[[x_axis]])) {
                  # Convert to numeric if possible
                  plot_data[[x_axis]] <- as.numeric(as.character(plot_data[[x_axis]]))
                  if (any(is.na(plot_data[[x_axis]]))) {
                    warning(paste("The column selected for the x-axis (", x_axis, ") contains non-numeric values. Switching to counts.", sep = ""))
                    # Aggregate counts for categorical x_axis
                    x_axis_agg <- paste0(x_axis, "_count")
                    plot_data <- plot_data[, .(count = .N), by = .(fill_var = get(fill_var), x_axis = get(x_axis))]
                    names(plot_data) <- c(fill_var, x_axis, x_axis_agg)
                    x_axis <- x_axis_agg
                  }
                }
                
                # Ensure the fill variable is treated as a factor
                plot_data[[fill_var]] <- as.factor(plot_data[[fill_var]])
                
                # Remove rows with NA values in the x-axis or fill variable
                valid_data <- plot_data[!is.na(plot_data[[x_axis]]) & !is.na(plot_data[[fill_var]])]
                
                # Check if the dataset is empty after filtering
                if (nrow(valid_data) == 0) {
                  output$dynamic_chart <- renderPlot({
                    ggplot() +
                      theme_void() +
                      labs(title = "No valid data available for the selected inputs.")
                  })
                  return()
                }
                
                # Render the density plot
                output$dynamic_chart <- renderPlot({
                  ggplot(valid_data, aes_string(x = x_axis, fill = fill_var)) +
                    geom_density(alpha = 0.7, adjust = 1.5) +  # Smoothing adjustment for density
                    labs(
                      title = "Dynamic Density Plot",
                      x = x_axis,
                      y = "Density",
                      fill = fill_var
                    ) +
                    custom_theme +
                    scale_fill_brewer(palette = "Set2")  # Optional: Customize color palette
                })
              } else {
                # Handle case where required variables are missing
                output$dynamic_chart <- renderPlot({
                  ggplot() +
                    theme_void() +
                    labs(title = "Density Plot requires both row and column variables.")
                })
              }
            }
            else if (chart_type == "Heatmap") {
              if (length(row_vars) > 0 && length(col_vars) > 0) {
                # Ensure variables are factors if they are categorical
                if (!is.numeric(plot_data[[row_vars[1]]])) {
                  plot_data[[row_vars[1]]] <- as.factor(plot_data[[row_vars[1]]])
                }
                if (!is.numeric(plot_data[[col_vars[1]]])) {
                  plot_data[[col_vars[1]]] <- as.factor(plot_data[[col_vars[1]]])
                }
                
                # Precompute counts for categorical variables
                if (is.factor(plot_data[[row_vars[1]]]) && is.factor(plot_data[[col_vars[1]]])) {
                  heatmap_data <- plot_data[, .N, by = c(row_vars[1], col_vars[1])]
                  ggplot(heatmap_data, aes_string(x = row_vars[1], y = col_vars[1], fill = "N")) +
                    geom_tile() +
                    labs(title = "Dynamic Heatmap", x = row_vars[1], y = col_vars[1], fill = "Count") +
                    custom_theme +
                    scale_fill_gradient(low = "blue", high = "red")  # Adjust colors as needed
                  
                } else {
                  # Handle numeric variables by binning
                  heatmap_data <- plot_data[, .(x_bin = cut(get(row_vars[1]), breaks = 10),
                                                y_bin = cut(get(col_vars[1]), breaks = 10)), 
                                            by = 1:nrow(plot_data)]
                  ggplot(heatmap_data, aes(x = x_bin, y = y_bin)) +
                    geom_tile(aes(fill = ..count..), stat = "bin2d") +
                    labs(title = "Dynamic Heatmap (Binned)", x = row_vars[1], y = col_vars[1], fill = "Count") +
                    custom_theme +
                    scale_fill_gradient(low = "blue", high = "red")
                }
              } else {
                ggplot() + 
                  theme_void() + 
                  labs(title = "Heatmap requires both row and column variables.")
              }
            } else {
              ggplot() + 
                theme_void() + 
                labs(title = "Unsupported combination of chart type and selected variables.")
            }
          })
        })
      })
    }
  })
}

# Run App
shinyApp(ui = ui, server = server)
>>>>>>> 4287deaea94441a1b2318efe0bc4acb09a3dbf7a
