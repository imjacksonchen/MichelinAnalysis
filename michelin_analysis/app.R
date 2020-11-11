library(shiny)
library(tidyverse)
library(leaflet)

one_star <- read_csv("one-star-michelin-restaurants.csv")
two_star <- read_csv("two-stars-michelin-restaurants.csv")
three_star <- read_csv("three-stars-michelin-restaurants.csv")

# columns.to.keep <- c("name", "latitude", "longitude", "city", "region", "cuisine", "price")
# 
# one_star <- one_star[columns.to.keep]
# two_star <- two_star[columns.to.keep]
# three_star <- three_star[columns.to.keep]

one_star <- one_star %>%
    mutate(stars = "1")
two_star <- two_star %>%
    mutate(stars = "2")
three_star <- three_star %>%
    mutate(stars = "3")

all_restaurants <- full_join(one_star, two_star)
all_restaurants <- full_join(all_restaurants, three_star)
all_restaurants$city <- str_remove_all(all_restaurants$city, "\\-")
all_restaurants$city <- str_remove_all(all_restaurants$city, "[:digit:]")
all_restaurants$city[153] <- "Hong Kong"
all_restaurants$city[167] <- "Hong Kong"

# credit for image to Freepik at flaticon.com
cutleryIcon <- makeIcon(iconUrl = "cutlery.png",
                        iconWidth = 30, 
                        iconHeight = 30)

ui <- fluidPage(
    titlePanel("Exploring Michelin Star Restaurants"),
    h5("NOTE: This dataset doesn't contain data from these cities:
       Belgium, France, Germany, Italy, Japan, Luxembourg, Netherlands, 
       Portugal, China, Spain, and Switzerland."),
    
    h5("In this first graph, we want to explore the distribution of cuisines at 
       Michelin Star restaurants in a particular city, based either price or stars."),
    
    h6("Note: Certain cities such as San Francisco, New York, Hong Kong, and Singapore
       has more Michelin Star restaurants than other cities."),
    
    selectInput(inputId = "var1",
                label = "Choose a city for plot 1",
                choices = all_restaurants$city),
    
    selectInput(inputId = "var2",
                label = "Choose a variable plot 1",
                choices = c("stars", "price")),
    
    plotOutput(outputId = "plot2"),
    
    
    h5("In this second graph, we want to explore the distribution of cuisines at 
       Michelin Star restaurants in a particular region, based either price or stars."),
    h5("By looking broadly at regions and not cities, we can see popular cuisine
       trends in a larger area."),
    
    selectInput(inputId = "var4",
                label = "Choose a region for plot 2",
                choices = all_restaurants$region),
    
    selectInput(inputId = "var5",
                label = "Choose a variable plot 2",
                choices = c("stars", "price")),
    
    plotOutput(outputId = "plot3"),
    
    h5("In this third graph, we want to explore the distribution of cuisines based on stars.
       We can see that contemporary cuisine dominates in 2-star and 3-star restaurants while
       modern cuisine dominates 1-star restaurants."),
    
    selectInput(inputId = "var6",
                label = "Choose amount of star(s)",
                choices = c("1", "2", "3")),
    
    plotOutput(outputId = "plot4"),
    
    h3("Explore the different Michelin restaurants across the world!"),
    
    selectInput(inputId = "var3",
                label = "Choose amount of star(s)",
                choices = c("1", "2", "3")),
    
    h6("Click on a marker to view more info."),
    leafletOutput(outputId = "leaflet1")
)

server <- function(input, output, session) {
    
    # Plotting a geom bar with amount cuisines with how many stars
    output$plot2 <- renderPlot({
        all_restaurants %>% 
            filter(city == input$var1) %>% 
            ggplot(aes_string(fill = input$var2, x = "cuisine")) +
            geom_bar(position = "dodge") +
            scale_fill_manual(values=c("#CD7F32", "#C0C0C0", "gold", "deepskyblue", "firebrick2")) +
            scale_y_continuous(breaks=seq(1,50,1)) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            ggtitle(paste("Distribution of cuisines and", toString(input$var2), "in", toString(input$var1)))
    })
    
    output$plot3 <- renderPlot({
        all_restaurants %>% 
            filter(region == input$var4) %>% 
            ggplot(aes_string(fill = input$var5, x = "cuisine")) +
            geom_bar(position = "dodge") +
            scale_fill_manual(values=c("#CD7F32", "#C0C0C0", "gold", "deepskyblue", "firebrick2")) +
            scale_y_continuous(breaks=seq(1,50,1)) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            ggtitle(paste("Distribution of cuisines and", toString(input$var2), "in", toString(input$var4)))
    })
    
    ### Amount of stars based on cuisine
    output$plot4 <- renderPlot({
        all_restaurants %>% 
            filter(stars == input$var6) %>% 
            ggplot(aes(x = cuisine)) +
            geom_bar(position = "dodge") +
            coord_flip() +
            theme(axis.text.y = element_text(size = 7)) +
            ggtitle(paste("Distribution of cusines with", input$var6, "stars"))
    })    
    
    # Creating Map with restaurants filtered by # of stars
    output$leaflet1 <- renderLeaflet({
        all_restaurants %>%
            filter(stars == input$var3) %>%
            leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
            htmlwidgets::onRender("function(el, x) {
                L.control.zoom({ position: 'topright' }).addTo(this)
            }") %>%
            addTiles() %>%
            addMarkers(lng = ~longitude, 
                       lat = ~latitude,
                       label = ~name,
                       popup = paste("<b>", filter(all_restaurants, stars == input$var3)$name, "</b>", "<br>", 
                                     filter(all_restaurants, stars == input$var3)$city, ",",
                                     filter(all_restaurants, stars == input$var3)$region, "<br>",
                                     "Price:", filter(all_restaurants, stars == input$var3)$price, "<br>",
                                     "Cuisine:", filter(all_restaurants, stars == input$var3)$cuisine, "<br>",
                                     "<a href='", filter(all_restaurants, stars == input$var3)$url, "' target='_blank'>",
                                     "Link to Michelin website</a>"),
                       icon = cutleryIcon)
    })
}

shinyApp(ui, server)