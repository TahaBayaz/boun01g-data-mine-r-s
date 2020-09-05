pti <- c("data.table", "tidyverse", "shiny")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
    install.packages(pti)
}

library(shiny)
library(tidyverse)
library(data.table)

carmarket = readRDS(gzcon(url("https://github.com/pjournal/boun01g-data-mine-r-s/blob/gh-pages/Project/turkey_car_market_EDA?raw=true")))

#Price Quantiles
quant <- quantile(carmarket$Price, seq(0, 1, 0.2))
carmarket <- carmarket %>%
    mutate(Price_Group = case_when(
        Price < quant[2] ~ "Very Low",
        Price < quant[3] ~ "Low",
        Price < quant[4] ~ "Medium",
        Price < quant[5] ~ "High",
        TRUE ~ "Very High"
    )) %>%
    mutate(Price_Group = factor(Price_Group, levels = c("Very Low", "Low", "Medium", "High", "Very High")))

#Get Price_Group list
Price_Group <- carmarket %>% 
    distinct(Price_Group) %>% 
    unlist(.)
names(Price_Group) <- NULL
Price_Group <- as.character(Price_Group)

#Get CCM list
# CCM <- carmarket %>% 
#     distinct(CCM) %>% 
#     unlist(.)
# names(CCM) <- NULL
# CCM <- as.character(CCM)
CCM = as.character(sort(unique(carmarket$CCM), decreasing = FALSE))

#Get Horse_Power list
# Horse_Power <- carmarket %>% 
#     distinct(Horse_Power) %>% 
#     unlist(.)
# names(Horse_Power) <- NULL
# Horse_Power <- as.character(Horse_Power)
Horse_Power = as.character(sort(unique(carmarket$Horse_Power), decreasing = FALSE))

#Get brand list
brand <- carmarket %>% 
    distinct(Brand) %>% 
    unlist(.)
names(brand) <- NULL
brand <- as.character(brand)

#Get color list
color <- carmarket %>% 
    distinct(Color) %>% 
    unlist(.)
names(color) <- NULL
color <- as.character(color)

#Get gear list
gear <- carmarket %>% 
    distinct(Gear) %>% 
    unlist(.)
names(gear) <- NULL
gear <- as.character(gear)

#Get fuel list
fuel <- carmarket %>% 
    distinct(Fuel_Type) %>% 
    unlist(.)
names(fuel) <- NULL
fuel <- as.character(fuel)

#Get body list
body <- carmarket %>% 
    distinct(Body_Type) %>% 
    unlist(.)
names(body) <- NULL
body <- as.character(body)

#Get status list
status <- carmarket %>% 
    distinct(Seller_Status) %>% 
    unlist(.)
names(status) <- NULL
status <- as.character(status)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Overview of Car Market 2020"),
    
    h5("Group 2: Can AYTORE, Taha BAYAZ, Ebru GECICI, Nezihe Nazli GUL, Talha UNLU, Mustafa KESER\n"),
    
    sidebarLayout(
        sidebarPanel(

            selectInput("brand", 
                        label = "Car Brand:", 
                        choices = c("All", brand), 
                        selected = c("Audi","BMW","Mercedes"),
                        multiple = TRUE),            
            
            sliderInput("price", 
                        label = "Price:",
                        min = min(carmarket$Price), 
                        max = max(carmarket$Price), 
                        value = c(min,max),
                        ticks = TRUE,
                        sep = ""),
            
            selectInput("Price_Group", 
                        label = "Price Group:", 
                        choices = c("All", Price_Group), 
                        selected = "All", 
                        multiple = TRUE), 
            
            sliderInput("age", 
                        label = "Model Year:",
                        min = min(carmarket$Model_Year), 
                        max = max(carmarket$Model_Year), 
                        value = c(2000,2020),
                        ticks = FALSE,
                        sep = ""),
            
            selectInput("color", 
                        label = "Color:", 
                        choices = c("All", color), 
                        selected = "All", 
                        multiple = TRUE),      
            
            selectInput("gear", 
                        label = "Gear Type:", 
                        choices = c("All", gear), 
                        selected = "All", 
                        multiple = TRUE), 
            
            selectInput("fuel", 
                        label = "Fuel Type:", 
                        choices = c("All", fuel), 
                        selected = "All", 
                        multiple = TRUE), 
            
            selectInput("body", 
                        label = "Body Type:", 
                        choices = c("All", body), 
                        selected = "All", 
                        multiple = TRUE), 
            
            selectInput("CCM", 
                        label = "CCM:", 
                        choices = c("All", CCM), 
                        selected = "All", 
                        multiple = TRUE), 
            
            selectInput("Horse_Power", 
                        label = "Horse Power:", 
                        choices = c("All", Horse_Power), 
                        selected = "All", 
                        multiple = TRUE), 
            
            sliderInput("Kilometers", 
                        label = "Kilometers:",
                        min = min(carmarket$Kilometers), 
                        max = max(carmarket$Kilometers), 
                        value = c(min(carmarket$Kilometers), median(carmarket$Kilometers)),
                        ticks = FALSE,
                        sep = ""),
            
            selectInput("status", 
                        label = "Status:", 
                        choices = c("All", status), 
                        selected = "All", 
                        multiple = TRUE),
            
            actionButton("show_options", 
                         label = "Show/Update Number of Options")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Graph", plotOutput("plot"), textOutput("options")),
                tabPanel("Table", dataTableOutput("table")),
                tabPanel("Density", plotOutput("density"))
            )
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    rv_options <- eventReactive(input$show_options, {
        
        option <- carmarket %>% 
            filter(Model_Year >= input$age[1] & Model_Year <= input$age[2]) %>%
            filter(Kilometers >= input$Kilometers[1] & Kilometers <= input$Kilometers[2]) %>%
            filter(Price >= input$price[1] & Price <= input$price[2])
        
        if(!("All" %in% input$Price_Group)){
            option <- option %>% filter(Price_Group %in% input$Price_Group)
        }
        if(!("All" %in% input$CCM)){
            option <- option %>% filter(CCM %in% input$CCM)
        }
        if(!("All" %in% input$Horse_Power)){
            option <- option %>% filter(Horse_Power %in% input$Horse_Power)
        }
        if(!("All" %in% input$brand)){
            option <- option %>% filter(Brand %in% input$brand)
        }
        if(!("All" %in% input$color)){
            option <- option %>% filter(Color %in% input$color)
        }
        if(!("All" %in% input$gear)){
            option <- option %>% filter(Gear %in% input$gear)
        }
        if(!("All" %in% input$fuel)){
            option <- option %>% filter(Fuel_Type %in% input$fuel)
        }
        if(!("All" %in% input$body)){
            option <- option %>% filter(Body_Type %in% input$body)
        }
        if(!("All" %in% input$status)){
            option <- option %>% filter(Seller_Status %in% input$status)
        }
        
        paste("You have", option %>% summarise(count=n()), "options.")
    })
    
    output$options <- renderText({
        rv_options() 
    })
    
    output$plot <- renderPlot({
        
        plot_df <- carmarket %>% 
            filter(Model_Year >= input$age[1] & Model_Year <= input$age[2]) %>%
            filter(Kilometers >= input$Kilometers[1] & Kilometers <= input$Kilometers[2]) %>%
            filter(Price >= input$price[1] & Price <= input$price[2])
        
        if(!("All" %in% input$Price_Group)){
            plot_df <- plot_df %>% filter(Price_Group %in% input$Price_Group)
        }
        if(!("All" %in% input$CCM)){
            plot_df <- plot_df %>% filter(CCM %in% input$CCM)
        }
        if(!("All" %in% input$Horse_Power)){
            plot_df <- plot_df %>% filter(Horse_Power %in% input$Horse_Power)
        }
        if(!("All" %in% input$brand)){
            plot_df <- plot_df %>% filter(Brand %in% input$brand)
        }
        if(!("All" %in% input$color)){
            plot_df <- plot_df %>% filter(Color %in% input$color)
        }
        if(!("All" %in% input$gear)){
            plot_df <- plot_df %>% filter(Gear %in% input$gear)
        }
        if(!("All" %in% input$fuel)){
            plot_df <- plot_df %>% filter(Fuel_Type %in% input$fuel)
        }
        if(!("All" %in% input$body)){
            plot_df <- plot_df %>% filter(Body_Type %in% input$body)
        }
        if(!("All" %in% input$status)){
            plot_df <- plot_df %>% filter(Seller_Status %in% input$status)
        }
        
        plot_df <- plot_df %>%
            mutate(avg_price = mean(Price))
        
        ggplot(plot_df, aes(x = Seller, y = avg_price, fill = Seller)) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            theme(legend.position = "bottom", 
                  legend.title = element_text(colour="red", size=13, face="bold"),
                  legend.text = element_text(size=12, face="italic"),
                  plot.title = element_text(hjust = 0.5, size=24),
                  axis.title=element_text(size=13,face="bold"),
                  strip.text.x = element_text(size = 12, colour = "blue3")) +
            scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ".", decimal.mark = ",")) +
            facet_wrap(vars(Brand), scales = "free") +
            labs(title = "Average Price by Seller", x = "Seller", y = "Average Price", fill = "Seller:") 
    })
    
    output$table <- renderDataTable({
        
        table_df <- carmarket %>% 
            filter(Model_Year >= input$age[1] & Model_Year <= input$age[2]) %>%
            filter(Kilometers >= input$Kilometers[1] & Kilometers <= input$Kilometers[2]) %>%
            filter(Price >= input$price[1] & Price <= input$price[2])
        
        if(!("All" %in% input$Price_Group)){
            table_df <- table_df %>% filter(Price_Group %in% input$Price_Group)
        }
        if(!("All" %in% input$CCM)){
            table_df <- table_df %>% filter(CCM %in% input$CCM)
        }
        if(!("All" %in% input$Horse_Power)){
            table_df <- table_df %>% filter(Horse_Power %in% input$Horse_Power)
        }
        if(!("All" %in% input$brand)){
            table_df <- table_df %>% filter(Brand %in% input$brand)
        }
        if(!("All" %in% input$color)){
            table_df <- table_df %>% filter(Color %in% input$color)
        }
        if(!("All" %in% input$gear)){
            table_df <- table_df %>% filter(Gear %in% input$gear)
        }
        if(!("All" %in% input$fuel)){
            table_df <- table_df %>% filter(Fuel_Type %in% input$fuel)
        }
        if(!("All" %in% input$body)){
            table_df <- table_df %>% filter(Body_Type %in% input$body)
        }
        if(!("All" %in% input$status)){
            table_df <- table_df %>% filter(Seller_Status %in% input$status)
        }
        
        table_df %>%
            arrange(Price)
    })
    
    output$density <- renderPlot({
        
        price_density <- carmarket %>% 
            filter(Model_Year >= input$age[1] & Model_Year <= input$age[2]) %>%
            filter(Kilometers >= input$Kilometers[1] & Kilometers <= input$Kilometers[2]) %>%
            filter(Price >= input$price[1] & Price <= input$price[2])
        
        if(!("All" %in% input$Price_Group)){
            price_density <- price_density %>% filter(Price_Group %in% input$Price_Group)
        }
        if(!("All" %in% input$CCM)){
            price_density <- price_density %>% filter(CCM %in% input$CCM)
        }
        if(!("All" %in% input$Horse_Power)){
            price_density <- price_density %>% filter(Horse_Power %in% input$Horse_Power)
        }
        if(!("All" %in% input$brand)){
            price_density <- price_density %>% filter(Brand %in% input$brand)
        }
        if(!("All" %in% input$color)){
            price_density <- price_density %>% filter(Color %in% input$color)
        }
        if(!("All" %in% input$gear)){
            price_density <- price_density %>% filter(Gear %in% input$gear)
        }
        if(!("All" %in% input$fuel)){
            price_density <- price_density %>% filter(Fuel_Type %in% input$fuel)
        }
        if(!("All" %in% input$body)){
            price_density <- price_density %>% filter(Body_Type %in% input$body)
        }
        if(!("All" %in% input$status)){
            price_density <- price_density %>% filter(Seller_Status %in% input$status)
        }
        
        ggplot(price_density, aes(x = Price, fill = Brand)) +
            geom_density(alpha=0.5) +
            theme_minimal() +
            theme(legend.position = "bottom", 
                  legend.title = element_text(colour="red", size=13, face="bold"),
                  legend.text = element_text(size=12, face="italic"),
                  plot.title = element_text(hjust = 0.5, size=24),
                  axis.title=element_text(size=13,face="bold"),
                  strip.text.x = element_text(size = 12, colour = "blue3")) +
            labs(title = "Price Density", x = "Price", y = "Density", fill = "Brand") +
            scale_x_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ".", decimal.mark = ","))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

