library(d3heatmap)
library(shiny)
library(ggcorrplot)
library(RColorBrewer)


ui <- fluidPage(
  #HEATMAP
  h1("HeatMap of Restaurants by Rating and Price"),
  selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
  d3heatmapOutput("heatmap"),

  #Correlation Plot
  h1("Correlations for Restaurant Customers"),
  checkboxInput(inputId = "corr",
                label = strong("Show Correlation Value"), value = FALSE),
    selectInput("cor_palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
  
  
    conditionalPanel(condition = "input.corr == true",
                   sliderInput(inputId = "corr_size",
                               label = "Change Size",
                               min = 4, max = 10, value = 1, step = 0.2)),
  
  
    plotOutput(outputId = "ggcor", height = "500px"))



server <- function(input, output, session) {
  output$heatmap <- renderD3heatmap({
    d3heatmap(mat,
      colors = input$palette,
      env = parent.frame(),
      dendrogram = "none"
    )
  })
  
  output$heatmap <- renderD3heatmap({
    d3heatmap(mat,
              colors = input$palette,
              env = parent.frame()
    )
  })
  
  output$ggcor <- renderPlot({
    p <- ggcorr(user_mat, palette = input$cor_palette)

    if (input$corr) {
      p <- ggcorr(user_mat,palette= input$cor_palette,label = TRUE,label_color = "white", 
                  label_size =input$corr_size)
    }
    
    p
  })
    
}
shinyApp(ui, server)
