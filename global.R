library(tidyverse)
library(plotly)
library(threejs)
library(dygraphs)
library(timeSeries)
library(xts)
library(ggmap)
library(shinydashboard)
library(gsheet)
library(wordcloud)
library(d3heatmap)
library(shiny)
library(ggcorrplot)
library(RColorBrewer)
library(GGally)
library(tm)
library(dendextend)
library(idendr0)

full_user_data <- read.csv("userprofile.csv")
rating_data <- read.csv("rating_final.csv")

cuisine <- read.csv("chefmozcuisine.csv")
geo <- read.csv("geoplaces2.csv")
ratings <- read.csv("rating_final.csv")
hours <- read.csv("chefmozhours4.csv")
data <- left_join(cuisine, geo, by = "placeID")
data <- left_join(data, ratings, by = "placeID")
data <- data[complete.cases(data),]
hours <- hours[which(hours$hours != "00:00-00:00;"),]
data$state <- toupper(data$state)
data$state <- gsub("S.L.P.", "SLP", data$state)
data$state <- gsub("SAN LUIS POTOSI", "SLP", data$state)
data$state <- gsub("SAN LUIS POTOS", "SLP", data$state)
data$state <- gsub("SLP", "SAN LUIS POTOSI", data$state)
data$state <- gsub("MEXICO", "SAN LUIS POTOSI", data$state)

slp_labels = c("American", "Bakery","Bar", "Brewery", "Breakfast",
               "Burgers", "Coffee Shop", "Cafeteria", "Chinese",
               "Contemporary", "Fast Food", "Game", 
               "International", "Italian", "Japanese", 
               "Mexican", "Pizzeria", "Seafood")

morelos_labels <- rep(NA, length(slp_labels))
morelos_labels[1:10] <- c("American", "Bar", "Brewery",
                          "Cafeteria", "Family", "Fast Food",
                          "International", "Japanese",
                          "Mexican", "Vietnamese")

tamaulipas_labels <- rep(NA, length(slp_labels))
tamaulipas_labels[1:6] <- c("Armenian", "Fast Food","Italian",
                            "Mexican", "Pizzeria", "Regional")

all_colors <- c("lightpink", "deeppink", "chocolate4",
                "chartreuse4", "darkgoldenrod1", "chartreuse",
                "deepskyblue", "darkblue", "darkmagenta", 
                "darkorchid1", "chocolate", "lightsalmon",
                "yellow1", "tan1", "yellowgreen",
                "slategray4", "turquoise1", "plum2")

all_labels <- data.frame("SAN LUIS POTOSI" = slp_labels, 
                         "MORELOS" = morelos_labels, 
                         "TAMAULIPAS" = tamaulipas_labels)


coords <- data.frame("MORELOS" = c(-99.27, 18.84, 
                                   -99.14, 18.96),
                     "SAN LUIS POTOSI" = c(-100.9855883 - .1, 22.0895261, 
                                           -100.9855883 + .1, 22.0895261 + .15), 
                     "TAMAULIPAS"  = c(-99.20, 23.71, 
                                       -99.10, 23.775))

new_data <- left_join(data, hours, by = "placeID")
new_hours <- gsub(":", "",new_data$hours)
new_hours <- gsub(";", "", new_hours)
new_data <- new_data[complete.cases(new_hours) == T,]
new_hours <- new_hours[complete.cases(new_hours) == T]
for(ii in 1:length(new_hours)){
  
  new_string <- strsplit(new_hours[ii], "-")
  close <- new_string[[1]][2]
  open <- new_string[[1]][1]
  substr(close, 3, 4) <- ifelse(substr(close, 3, 3) == "3", ".5", ".0")
  substr(open, 3, 4) <- ifelse(substr(open, 3, 3) == "3", ".5", ".0")
  hours_open <- as.numeric(close) - as.numeric(open)
  hours_open <- ifelse(hours_open < 0, hours_open + 24, hours_open)
  new_hours[ii] <- as.numeric(hours_open)
}

new_data$total_hours <- new_hours



restaurant <- read.csv("geoplaces2.csv")
rating_final <- read.csv("rating_final.csv")
user <- read.csv("userprofile.csv")

placeID_list <- unique(rating_final$placeID)
my_data_final <- data.frame()
for (i in 1:length(placeID_list)){
  curr.placeID <- placeID_list[i]
  curr.placeID.data <- rating_final[which(rating_final$placeID == 
                                            curr.placeID),]
  curr.avg.rating <- mean(curr.placeID.data$rating)
  curr.avg.food_rating <- mean(curr.placeID.data$food_rating)
  curr.avg.service_rating <- round(median(curr.placeID.data$service_rating))
  curr.list <- data.frame(placeID = curr.placeID, rating = curr.avg.rating,
                          food_rating =  curr.avg.food_rating,
                          service_rating = curr.avg.service_rating)
  my_data_final <- rbind(my_data_final, curr.list)
}
total <- merge(restaurant,my_data_final,by="placeID")
my_data <- as_tibble(total)
my_data %>% select(placeID, price, city, rating, food_rating,service_rating)

total_user<- merge(user,rating_final,by="userID")
user_data <- as_tibble(total_user)


my_data %>% select(placeID, price, rating, food_rating,service_rating)

my_data$price <- recode(my_data$price, low = "0", medium = "1", high = "2")

my_data <- mutate(my_data, price = as.numeric(price),
                  rating = round(floor(rating)))

final <- my_data %>% select(rating, price)
colnames(final) <- c("Rating", "Price")
mat = as.matrix(final)


user_data <- dplyr::select(user_data, drink_level, marital_status, rating,
                           activity, religion, dress_preference, smoker)

user_data <- mutate(user_data,
                    activity = as.numeric(activity),
                    religion = as.numeric(religion),
                    dress_preference = as.numeric(dress_preference),
                    smoker = as.numeric(dress_preference),
                    marital_status	 = as.numeric(marital_status),
                    drink_level	 = as.numeric(drink_level))

colnames(user_data) <- c("Drink", "Married", "Rating", 
                         "Activity", "Religion", "Dress", "Smoker")

corr <- round(cor(user_data), 1)

user_mat = as.matrix(user_data)

p.mat <- cor_pmat(user_data)

interactive_315_theme <-  theme_bw() + # White background, black and white theme
  theme(axis.text = element_text(family = "Arial", size = 10, color = "black"),
        text = element_text(family = "Arial",size = 12, color = "black"))




Rating <- c(0, 1, 2)
Franchise <- c(30, 62, 65)
Not_Franchise <- c(224, 359, 421)
data_barchart <- data.frame(Rating, Franchise, Not_Franchise)




colorblind_palette <- c("deepskyblue", "darkorchid3", "firebrick1")


user_prof <- read.csv("userprofile.csv")
rating <- read_csv("rating_final.csv")
user_rating_df <- left_join(rating, user_prof, by = "userID")

user_rating_tabs <- names(table(user_rating_df$weight))

new_ratings <- rep(NA, length(user_rating_tabs))
for(ii in 1:length(new_ratings)){
  rows_weight <- which(user_rating_df$weight == user_rating_tabs[ii])
  new_ratings[ii] <- round(mean(user_rating_df[rows_weight,]$food_rating),0)
}


