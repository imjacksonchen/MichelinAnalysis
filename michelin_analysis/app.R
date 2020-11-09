library(shiny)
library(tidyverse)
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

ui <- fluidPage(

    # selectizeInput(inputId = "var2",
    #                label = "Select your first variable",
    #                choices = c("city", "region", "cuisine", "stars")),

    selectInput(inputId = "var4",
                label = "Choose a city",
                choices = sort(all_restaurants$city)),
    
    selectInput(inputId = "var5",
                label = "Choose a variable",
                choices = c("stars", "price")),
    
    plotOutput(outputId =  "plot2"),
    
    plotOutput(outputId =  "plot3")
)

server <- function(input, output, session) {

    # Plotting a geom bar with city vs stars
    output$plot2 <- renderPlot({
        all_restaurants %>% 
            filter(city == input$var4) %>% 
            ggplot(aes_string(x = input$var5)) +
            geom_bar() 
    })
    
    # PLotting a geom bar with amount cuisines with how many stars
    output$plot3 <- renderPlot({
        all_restaurants %>% 
            filter(city == input$var4) %>% 
            ggplot(aes(fill = stars, x = cuisine)) +
            geom_bar(position = "dodge") +
            scale_y_continuous(breaks=seq(1,20,1))
    })
}

shinyApp(ui, server)