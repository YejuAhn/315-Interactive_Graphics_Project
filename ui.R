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


dashboardPage(
  dashboardHeader(title = "Group 14: Project"),
  skin = "purple", 
  dashboardSidebar(sidebarMenu(
                      menuItem("Chloropleth", tabName = "chloro", 
                             icon = icon("map-marked-alt")),
                      menuItem("Density Heat", tabName = "densheat",
                               icon = icon("fire")),
                      menuItem("Word Cloud", tabName = "parta", 
                               icon = icon("font")),
                      menuItem("Density", tabName = "partb", 
                               icon = icon("chart-area")),
                      menuItem("Heat Map", tabName = "heatmap",
                               icon = icon("chart-line")),
                      menuItem("Correlations", tabName = "cor",
                               icon = icon("signal")),
                      menuItem(text = "Bar Chart", tabName = "Bar", 
                               icon = icon("chart-bar")),
                      menuItem(text = "Dendrogram", tabName = "dend",
                               icon = icon("chart-line")))
                   ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "chloro",
              fluidRow(
                
                box(
                  title = "Controls",
                  selectInput(inputId = "states",
                              label = "State in Mexico",
                              choices = c("San Luis Potosi", "Morelos", 
                                          "Tamaulipas"),
                              selected = "San Luis Potosi")
                  
                )), 
              fluidRow(
                box(
                  title = "Chloropleth Map of State in Mexico by Cuisine",
                  plotOutput(outputId = "chloro_plot", height = "500px",
                               width = "800px"),
                  height = "600px", width = 10)
              )
      ),
      tabItem(tabName = "densheat",
              fluidRow(
                
                box(
                  title = "Controls",
                  selectInput(inputId = "days",
                              label = "Days of the Week Open",
                              choices = c("Weekdays", "Saturday", 
                                          "Sunday"),
                              selected = "Weekdays")
                  
                )), 
              fluidRow(
                box(
                  title = "Density of Hours Open and Service Rating",
                  plotOutput(outputId = "density_heat", height = "500px",
                               width = "1000px"),
                  width = 11, height = "600px")
              )
      ),
      tabItem(tabName = "parta",
              fluidRow(
                box(title = "Word Cloud of Customers",
                    plotOutput("plot1", 
                               height = "500px", width="500px")),
                box(
                  title = "Inputs", 
                  sliderInput(inputId = "freq",
                              label = "Frequency of Variable",
                              min = 10, max = 99, 
                              value = 30, step = 1))
                )
      ),
      
      tabItem(tabName = "partb",
              # Output: Tabset w/ plot, 
              fluidRow(
                box(title = "Density Plot of Rating Given Budget", 
                    plotlyOutput("plot2", height = "500px", width = "700px"),
                    height="600px", width= 9)   
              )
              
      ),
      
      tabItem(tabName = "heatmap",
              fluidRow(
                box(title = "Heat Map of Restaurants by Rating and Price",
                    d3heatmapOutput("heatmap", 
                                    height = "650px", width = "600px"),
                    height = "750px", width = 6),
                box(
                  title = "Controls",
                  checkboxInput(inputId = "dend",
                                label = strong("Show Dendrogram"), 
                                value = TRUE),
                  selectInput("palette", "Palette", 
                              c("Yellow and Red", "Red, Yellow, and Blue", 
                                "Greens", "Blues"),
                              selected = "Yellow and Red")
                ))
      ),

      tabItem(tabName = "cor",
              fluidPage(
                box(
                  title = "Correlations for Restaurant Customers",
                  plotOutput(outputId = "ggcor", 
                             height = "520px", width = "520px")
                ),
                
                box(
                  title = "Controls",
                  checkboxInput(inputId = "corr",
                                label = strong("Show Correlation Value"), 
                                value = FALSE),
                  selectInput("cor_palette", "Palette", 
                              c("Yellow And Red", "Blue and Purple", 
                                "Green and Orange"),
                              selected = "Yellow and Red"),
                  
                  
                  conditionalPanel(condition = "input.corr == true",
                                   sliderInput(inputId = "corr_size",
                                               label = "Change Size",
                                               min = 4, max = 10, 
                                               value = 1, step = 0.2))
                )
              )
      ),
      
      tabItem(tabName = "Bar", 
              fluidPage(
                
                box(title = "Restaurant Ratings by Franchise",
                    plotlyOutput(outputId = "bar", 
                                 height = "550px", width = "850px"),
                    height = "650px", width = 10)
                
              )),
      
      tabItem(tabName = "dend", 
              fluidRow(
                box(title = "Dendrogram of Weight of Consumer by Average Food Rating",
                    plotOutput(outputId = "dendrogram", 
                                 height = "650px", width = "500px"),
                    height = "730px", width = 6),
              
                box(
                  title = "Controls",
                  selectInput("dend_palette", "Palette", 
                              c("Brown, Red, and Orange", 
                                "Blue, Purple, and Green"),
                              selected = "Brown, Red, and Orange"))
              )
      )
    )
  )
)





