library(shiny)

library(tidyverse)
one_star <- read.csv("one-star-michelin-restaurants.csv")
two_star <- read.csv("two-stars-michelin-restaurants.csv")
three_star <- read.csv("three-stars-michelin-restaurants.csv")

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
    selectInput(inputId = "var3",
                label = "Choose a city",
                choices = all_restaurants$city),
    selectInput(inputId = "var1",
                label = "Select your first variable",
                #choices = c("city", "region", "cuisine", "stars")),
                choices = colnames(all_restaurants)),
    selectInput(inputId = "var2",
                label = "Select your second variable",
                #choices = c("city", "region", "cuisine", "stars")),
                choices = colnames(all_restaurants)),
    plotOutput(outputId = "plot1"), 
    plotOutput(outputId = "plot2")
)

server <- function(input, output, session) {
    output$plot1 <- renderPlot({
        all_restaurants %>%
            filter(city == input$var3) %>%
            ggplot(aes(x = city,
                        y = input$var1)) +
            geom_point() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        output$plot2 <- renderPlot({
            all_restaurants %>%
                filter(city %in% input$var3) %>%
                ggplot(aes(x = city, 
                           y = input$var1))+ 
                geom_point()

        })
    })
}

shinyApp(ui, server)