# Libraries----
library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(factoextra)
library(raster)
library(tmap)
library(png)
library(gridExtra)
library(dendextend)
library(DescTools)
library(imageryML)
library(plotly)
library(ggmap)
theme_set(theme_light())


# Data----
df_raw <- read_csv("../data/erddap_2000-01-01_2021-06-01_lat_40.625_50.625_lon_229.875_236.625.csv")


# UI----
ui <- fluidPage(

    # Application title
    titlePanel("Automatic Detection of Upwelling"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput(inputId = "custom_date",
                      label = "Date:",
                      value = "2020-09-20",
                      min = "2000-01-01",
                      max = "2021-06-01")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("auto_detection_plot", width = 650, height = 650)
        )
    )
)

# Server----
server <- function(input, output) {
    
    final_df <- reactive({
        
        # Process data
        df_processed <- df_raw %>% 
            # Get rid of miscellaneous zlev in first row
            slice(-1) %>% 
            # zlev is a column of zeroes, so get rid of that
            dplyr::select(-zlev) %>% 
            # Convert into date
            mutate(time = ymd_hms(time)) %>% 
            # Set column names
            rename(date = time,
                   lat = latitude,
                   lon = longitude) %>% 
            # Convert date column to Date type
            mutate(date = as.Date(date),
                   lat = as.numeric(lat),
                   lon = as.numeric(lon),
                   sst = as.numeric(sst))
        
        # Mask out Puget Sound, Strait of Juan de Fuca and Georgia Strait
        masks <- list(c(235.4488, 236.884, 47.87651, 50.13138),
                      c(232.2913, 233.8987, 50.28689, 51.60871),
                      c(234.4154, 235.9654, 49.04283, 50.09251))
        
        for (m1 in masks) {
            # index of Puget Sound, Strait of Juan de Fuca or Georgia Strait
            mask_loc <- df_processed$lat <= m1[4] & df_processed$lat >= m1[3] &
                df_processed$lon <= m1[2] & df_processed$lon >= m1[1]
            # Change to NA
            df_processed$sst[mask_loc] <- NA
        }
        
        
        # Create df for specific date
        df_processed_date <- df_processed %>% 
            filter(date == as.character(input$custom_date)) %>% 
            mutate(lon = lon - 360)
        
        # Automatic Detection Method
        # Find SST values next to land (sst_coast_X) and check if upwelling
        is_upwelling <- df_processed_date %>% 
            group_by(lat) %>% 
            # 1 longitude tick away from land
            mutate(sst_coast_1 = last(na.omit(sst)),
                   # 8 longitude ticks away from land (2 degrees in longitude)
                   sst_coast_2 = nth(na.omit(sst), -9),
                   # 12 longitude ticks away from land (3 degrees in longitude)
                   sst_coast_3 = nth(na.omit(sst), -13)) %>% 
            # Find difference between pixels
            # Threshold: 0.15
            summarize(is_upwelling_1_2 = sst_coast_2 - sst_coast_1 > 2.5,
                      is_upwelling_1_3 = sst_coast_3 - sst_coast_1 > 2.5) %>% 
            ungroup() %>% 
            group_by(lat) %>% 
            # Check if any upwelling in each latitude
            # first() because I want to return one row for each lat
            summarise(is_upwelling_total = first(is_upwelling_1_2 | is_upwelling_1_3)) %>% 
            ungroup()
        
        
        final_df <- df_processed_date %>% 
            left_join(is_upwelling) %>%
            ungroup() %>% 
            group_by(lat) %>% 
            # Find  (coast) longitude corresponding to row preceding last Na/NaN value in SST
            mutate(last_lon = across(sst, ~ tail(lon[!is.na(.)], 1))) %>% 
            ungroup() %>% 
            # last_lon: longitude off coast
            mutate(last_lon = last_lon$sst,
                   # For blue dots
                   last_lon_minus_two = last_lon - 2,
                   last_lon_minus_three = last_lon - 3)
        
        final_df
        
    })
    
    # Automatic Detection Plot
    output$auto_detection_plot <- renderPlot({
        
        # final_df for upwelling
        final_df_upwelling <- final_df() %>% 
            filter(is_upwelling_total)
       
        # Plot
        ggplot() +
            geom_tile(aes(lon, lat, fill = sst),
                      alpha = 0.9,
                      data = final_df()) +
            geom_point(data = final_df_upwelling,
                       mapping = aes(x = last_lon, y = lat),
                       size = 1.5,
                       shape = 8,
                       color = "red") +
            geom_point(data = final_df_upwelling,
                       mapping = aes(x = last_lon_minus_two, y = lat),
                       size = 1.5,
                       shape = 8,
                       color = "blue") +
            geom_point(data = final_df_upwelling,
                       mapping = aes(x = last_lon_minus_three, y = lat),
                       size = 1.5,
                       shape = 8,
                       color = "blue") +
            scale_x_continuous(labels = label_number(suffix ="\u00b0")) +
            scale_y_continuous(labels = label_number(suffix ="\u00b0")) +
            scale_fill_gradient2(midpoint = mean(final_df()$sst, na.rm = TRUE),
                                 low = "blue",
                                 mid = "white",
                                 high = "red",
                                 na.value = "grey20") +
            coord_quickmap() +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  legend.position = "bottom",
                  legend.background=element_blank()) +
            labs(x = NULL,
                 y = NULL,
                 fill = "Sea Surface Temperature (\u00B0C)",
                 title = paste0("Date: ", as.character(input$custom_date),
                                " Threshold: ", 2.5, " \u00B0C"),
                 subtitle = paste0("Latitude (",
                                   40.625,
                                   "\u00B0,",
                                   50.625,
                                   "\u00B0) ",
                                   " Longitude (",
                                   130.125,
                                   "\u00B0,",
                                   123.375,
                                   "\u00B0)",
                                   "\n\nRed dots are location of upwelling \nBlue dots are location of comparison to red dots")
            )
        
        
       
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
