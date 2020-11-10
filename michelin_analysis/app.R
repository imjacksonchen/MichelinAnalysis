library(shiny)
library(tidyverse)
library(leaflet)

one_star <- read_csv("one-star-michelin-restaurants.csv")
two_star <- read_csv("two-stars-michelin-restaurants.csv")
three_star <- read_csv("three-stars-michelin-restaurants.csv")

columns.to.keep <- c("name", "latitude", "longitude", "city", "region", "cuisine", "price")

one_star <- one_star[columns.to.keep]
two_star <- two_star[columns.to.keep]
three_star <- three_star[columns.to.keep]

one_star <- one_star %>%
    mutate(stars = "one")
two_star <- two_star %>%
    mutate(stars = "two")
three_star <- three_star %>%
    mutate(stars = "three")

all_restaurants <- full_join(one_star, two_star)
all_restaurants <- full_join(all_restaurants, three_star)
all_restaurants$city <- str_remove_all(all_restaurants$city, "\\-")
all_restaurants$city <- str_remove_all(all_restaurants$city, "[:digit:]")
all_restaurants$city[153] <- "Hong Kong"
all_restaurants$city[167] <- "Hong Kong"

ui <- fluidPage(

    selectInput(inputId = "var1",
                label = "Choose a city",
                choices = all_restaurants$city),

    selectInput(inputId = "var2",
                label = "Choose a variable",
                choices = c("stars", "price")),
    
    plotOutput(outputId =  "plot2"),
    
    selectInput(inputId = "var3",
                label = "Choose amount of stars",
                choices = c("one", "two", "three")),
    
    leafletOutput(outputId = "leaflet1")
)

server <- function(input, output, session) {
    
    # Plotting a geom bar with amount cuisines with how many stars
    output$plot2 <- renderPlot({
        all_restaurants %>% 
            filter(city == input$var1) %>% 
            ggplot(aes_string(fill = input$var2, x = "cuisine")) +
            geom_bar(position = "dodge") +
            scale_fill_manual(values=c("brown", "grey", "gold", "blue", "red")) +
            scale_y_continuous(breaks=seq(1,20,1)) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
                       popup = ~name)
    })
}

shinyApp(ui, server)