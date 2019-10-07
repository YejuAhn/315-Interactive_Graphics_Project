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


function(input, output) {
  
  output$chloro_plot <- renderPlot({

    colName <- gsub(" ", ".", toupper(input$states))
    map <- get_stamenmap(coords[,colName], 
                         maptype = "toner-lite", zoom = 13)
    
    mapPoints <- ggmap(map) +
      geom_point(aes(x = longitude, y = latitude, color = factor(Rcuisine),
                     shape = factor(rating)), alpha = 0.5, size = 3, 
                 data = data[which(data$state == toupper(input$states)),]) + 
      scale_color_manual(name = "Cuisine",
                         labels = all_labels[!is.na(all_labels[,colName]),
                                             colName],
                         values = all_colors[1:sum(
                           !is.na(all_labels[,colName]))]) +
      labs( 
           x = "Longitude", 
           y = "Latitude", 
           shape = "Rating") + 
      interactive_315_theme +
      theme(legend.position = "bottom") 
    
    mapPoints
  })
  
  output$density_heat <- renderPlot({
    if(input$days == "Weekdays"){
      days <- "Mon;Tue;Wed;Thu;Fri;"
    } else if(input$days == "Saturday") {
      days <- "Sat;"
    } else {days <- "Sun;"}
    density_plot <- ggplot(new_data[which(new_data$days == days),],
           aes(x = as.numeric(service_rating), 
               y = as.numeric(total_hours))) + 
      stat_density2d(aes(fill = ..density..), geom = "tile", contour = F) +
      geom_density2d(color = "black") +
      scale_fill_gradient2( low = "blue", mid = "purple", 
                            high = "red", midpoint = .04) +
      labs(
           x = "Service Rating",
           y = "Hours Open",
           fill = "Density") + 
      interactive_315_theme +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
      ) 
    density_plot
  })
  
  data_plot1 <- c(as.character(full_user_data$religion), 
                  as.character(full_user_data$drink_level),
                  as.character(full_user_data$ambience),
                  as.character(full_user_data$transport))
  data_plot1 <- data_plot1[data_plot1!="none" & data_plot1!="?"]
  data_plot1 <- tolower(data_plot1)
  wordcloud_rep <- repeatable(wordcloud)
  count <- table(data_plot1)
  
  output$plot1 <- renderPlot({
  wordcloud_rep(sort(unique(data_plot1)), count, min.freq = input$freq,
                         colors=c("#FF0000FF", "#FF9900FF", "#CCFF00FF", "#33FF00FF",
                                  "#00FF66FF", "#00FFFFFF", "#0066FFFF", "#3300FFFF",
                                  "#CC00FFFF", "#FF0099FF"))
    
  })
  
  data_plot2 <- rating_data %>%
    group_by(userID) %>%
    summarize(sum(rating)/n()) %>%
    ungroup() %>%
    data.frame()
  data_plot2$weight <- full_user_data$weight
  
  colnames(data_plot2) <- c("id", "rating", "weight")
  data_plot2 <- data_plot2[which(data_plot2$weight!="?"),]
  
  data_plot2$budget <- full_user_data$budget
  data_plot2 <- data_plot2[data_plot2$budget!="?",]
  output$plot2 <- renderPlotly({
    my_plot <- ggplot(data_plot2) + 
      geom_density(aes(x=rating, fill=as.factor(budget)),
                   binwidth=.5, alpha=.5, position="identity", 
                   col = "black")+
      labs(
           y = "Density",
           x = "Rating",
           fill = "Budget") + 
      scale_fill_manual(values = c("magenta4", "limegreen", "deepskyblue")) +
      interactive_315_theme
    ggplotly(my_plot)
  })
  
  
  output$heatmap <- renderD3heatmap({
    if(input$palette == "Yellow and Red"){
      color_palette <- "YlOrRd"
    } else if(input$palette == "Red, Yellow, and Blue"){
      color_palette <- "RdYlBu"
    } else if (input$palette == "Greens"){
      color_palette <- "Greens"
    } else{
      color_palette <- "Blues"
    }
    d3heatmap(mat,
              colors = color_palette,
              env = parent.frame(),
              dendrogram = ifelse(input$dend, "both", "none")
    )
  })


  output$ggcor <- renderPlot({
    if(input$cor_palette == "Yellow And Red"){
      low_col <- "yellow"
      high_col <- "red3"
    } else if(input$cor_palette == "Blue and Purple"){
      low_col <- "deepskyblue"
      high_col <- "purple"
    } else{
      low_col <- "green3"
      high_col <- "orange"
    }
    
    p <- ggcorr(user_mat, low = low_col, mid = "grey", high = high_col) +
      interactive_315_theme

    if (input$corr) {
      p <- ggcorr(user_mat, low = low_col, mid = "grey", high = high_col, 
                  label = TRUE,
                  label_color = "white", label_size = input$corr_size) + 
        interactive_315_theme
    }

    p
  })
  
  
  output$bar <- renderPlotly({
    interactive_bar_rate_fr <- plot_ly(data, x = ~Rating, 
                                       y = ~Franchise, type = 'bar', 
                                       name = 'Franchise', color = I("red"), 
                                       alpha = 0.75) %>%
      add_trace(y = ~Not_Franchise, name = 'Not Franchise', color = I("blue"), 
                alpha = 0.75) %>%
      layout(title = "Restaurant Ratings by Franchise",
             yaxis = list(title = "Frequency of Rating",
                          color = "black",
                          size = 10,
                          font = "Arial"), 
             xaxis = list(title = "Rating",
                          color = "black",
                          size = 10,
                          font = "Arial"),
             barmode = 'stack')
    
    
    interactive_bar_rate_fr
  
    
    
  })
  
  output$dendrogram <- renderPlot({
    if(input$dend_palette == "Brown, Red, and Orange"){
      dend_colors <- c("darkgoldenrod4", "orange", "red")
    } else {
      dend_colors <- c("deepskyblue", "purple", "green")
    }
    
                         
    dend <- weight_data %>% scale %>% dist %>% hclust %>% as.dendrogram
    dend %>% 
      set("labels", weight_data$weight, order_value = TRUE) %>%
      set("labels_col", get_colors(weight_data$avg_rating,
                                   palette = dend_colors), 
          order_value = TRUE) %>% 
      ggplot(horiz = T) 
    
  })
  
  
}

