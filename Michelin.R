# R Program to analyze different Michelin starred restaurants for our HW 4 for Data Science
# Created by: Eric Leung, Bryan Martinez, Jackson Chen

library(tidyverse)
# one_star <- read_csv("/Users/ericleung/Desktop/Midd Year 4/Semester 1/MATH 216/MichelinAnalysis/michelin_analysis/one-star-michelin-restaurants.csv")
# two_star <- read_csv("/Users/ericleung/Desktop/Midd Year 4/Semester 1/MATH 216/MichelinAnalysis/michelin_analysis/two-stars-michelin-restaurants.csv")
# three_star <- read_csv("/Users/ericleung/Desktop/Midd Year 4/Semester 1/MATH 216/MichelinAnalysis/michelin_analysis/three-stars-michelin-restaurants.csv")

one_star <- read_csv("/Users/jacksonchen/Desktop/MATH216 Data Sci/Homework/HW4/MichelinAnalysis/michelin_analysis/one-star-michelin-restaurants.csv")
two_star <- read_csv("/Users/jacksonchen/Desktop/MATH216 Data Sci/Homework/HW4/MichelinAnalysis/michelin_analysis/two-stars-michelin-restaurants.csv")
three_star <- read_csv("/Users/jacksonchen/Desktop/MATH216 Data Sci/Homework/HW4/MichelinAnalysis/michelin_analysis/three-stars-michelin-restaurants.csv")

# columns.to.keep <- c("name", "latitude", "longitude", "city", "region", "cuisine", "price")
# 
# one_star <- one_star[columns.to.keep]
# two_star <- two_star[columns.to.keep]
# three_star <- three_star[columns.to.keep]

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

all_restaurants %>%
  ggplot(aes(x = cuisine)) +
  geom_bar() +
  coord_flip()

all_restaurants %>%
  filter(city == "Wien") %>%
  ggplot(aes(x = city,
            y = cuisine)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Bar Graph to show Cuisines vs price and stars
# Show for the city of Wien
all_restaurants %>% 
  filter(city == "Wien") %>% 
  ggplot(aes(x = stars)) +
  geom_bar()

### Grouped bar graph
all_restaurants %>% 
  filter(city == "Wien") %>% 
  ggplot(aes(fill = stars, x = cuisine)) +
  geom_bar(position = "dodge")


### Amount of stars based on cuisine
all_restaurants %>% 
  filter(stars == "one") %>% 
  ggplot(aes(x = cuisine)) +
  geom_bar(position = "dodge") +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))
  coord_flip()





