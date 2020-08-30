# IE48A Group Assignment: Airbnb Kaggle Data

# Group 2: Can AYTORE, Taha BAYAZ, Ebru GECICI, Nezihe Nazli GUL, Talha UNLU, Mustafa KESER


library(shiny)
library(tidyverse)
library(ggplot2)
airbnb <- read.csv("C:/Users/can_a/Desktop/IE48A/AB_NYC_2019.csv")


quant <- quantile(airbnb$price, seq(0, 1, 0.2))
airbnb_price_group <- airbnb %>%
    mutate(price_group = case_when(
        price < quant[2] ~ "Very Low",
        price < quant[3] ~ "Low",
        price < quant[4] ~ "Medium",
        price < quant[5] ~ "High",
        TRUE ~ "Very High"
    )) %>%
    mutate(price_group = factor(price_group, levels = c("Very Low", "Low", "Medium", "High", "Very High")))


# Get genre list
neighbourhood <- airbnb %>% 
    distinct(neighbourhood) %>% 
    unlist(.)
names(neighbourhood) <- NULL
neighbourhood <- as.character(neighbourhood)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Compare Airbnb Prices"),
    
    h5("Group 2: Can AYTORE, Taha BAYAZ, Ebru GECICI, Nezihe Nazli GUL, Talha UNLU, Mustafa KESER\n"),
    
    sidebarLayout(
        sidebarPanel(
            
            numericInput("minimum_nights", 
                         label = "Minimum Nights:", 1),
            
            sliderInput("price", 
                label = "Price:",
                min = min(airbnb$price), 
                max = max(airbnb$price), 
                value = c(min(airbnb$price), median(airbnb$price)),
                ticks = TRUE,
                sep = ""),
   
            selectInput("room_type", 
                label = "Room Type:", 
                choices = unique(airbnb$room_type), 
                selected = "Private room", 
                multiple = TRUE),
    
            selectInput("neighbourhood", 
                label = "Neighbourhood:", 
                choices = c("All", neighbourhood), 
                selected = "All", 
                multiple = TRUE),
            
            sliderInput("listing_counts", 
                label = "Minimum Host Listing:",
                min = min(airbnb$calculated_host_listings_count),
                max = max(airbnb$calculated_host_listings_count),
                value = 1,
                ticks = FALSE,
                sep = "")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("AirbnbPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$AirbnbPlot <- renderPlot({
        
        plot_df <- airbnb_price_group %>% 
            filter(calculated_host_listings_count >= input$listing_counts) %>%
            filter(minimum_nights >= input$minimum_nights) %>%
            filter(price >= input$price[1] & price <= input$price[2]) %>%
            filter(room_type %in% input$room_type)
        
        if(!("All" %in% input$neighbourhood)){
            plot_df <- plot_df %>% filter(neighbourhood %in% input$neighbourhood)
        }
        
        ggplot(plot_df, aes(x = latitude, y = longitude, color = price_group)) +
            geom_point() +
            theme_minimal() +
            theme(legend.position = "bottom", 
                  legend.title = element_text(colour="red", size=13, face="bold"),
                  legend.text = element_text(size=12, face="italic"),
                  plot.title = element_text(hjust = 0.5, size=24),
                  axis.title=element_text(size=13,face="bold"),
                  strip.text.x = element_text(size = 12, colour = "blue3")) +
            facet_wrap(vars(neighbourhood_group), scales = "free") +
            labs(title = "Airbnb Locations by Neighbourhood Group", x = "Latitute", y = "Longitude", color = "Price Ranges:")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
