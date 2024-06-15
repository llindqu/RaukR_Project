library(shiny)
library(dplyr)
library(tidyverse)
library(glue)



# Define the path to the directory containing the CSV files
csv_files <- list.files(path = "data/", pattern = "\\.csv$", full.names = TRUE)
csv_names <- tools::file_path_sans_ext(basename(csv_files))
data_list <- lapply(csv_files, read.csv)

#column_names <- colnames(data_list[[1]])
column_names <- c('time', 'od_repeats', 'channel_id', 'temperature', 'batchtime_h', 'od_corr')


myPlot <- function(data, x, y, z, ptsize, axsize, channel_id_list, LED) {
  
  # CHANNEL ID
  mask <- data$channel_id %in% channel_id_list
  data <- data[mask, ]
  
  # LED
  mask <- data$od_led %in% LED
  data <- data[mask, ]
  
  # SET COLOR
  cols <- c("1" = "#14a73f", "2" = '#1ac658', "3" = '#29ea9e', "4" = '#3ae2f4', "5" = '#4beedd', "6" = '#2cb195', "7"="#0d744c", "8"="#1e543e")
  
  p <- ggplot(data = data, aes(x = .data[[x]],
                               y = .data[[y]])) +
    geom_point(size = ptsize, aes(colour = factor(channel_id))) +
    theme(axis.title = element_text(size = axsize)) +
    scale_colour_manual(values = cols)
  
  return(p)
}




myText <- function(data, x, y, ptsize, axsize) {
  
  myString <- glue("ggplot(data = data, aes(x = {x}, y = {y})) +
    geom_point(size = {ptsize}) +
    theme(axis.title = element_text(size = {axsize}))")
  
  
  return(myString)
}



ui = fluidPage(
  titlePanel("testing testin"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", label = "Select data", choices = csv_names),
      selectInput("x", label = "Choose x",choices = column_names),
      selectInput("y", label = "Choose y",choices = column_names),
      selectInput("color", label = "Choose color",choices = column_names),
      selectInput("theme_Choice", label = "Theme", choices = c("Default", "Classic", "Black/White")),
      sliderInput("pt_size",label = "Point size", min = 0.5, max = 10,value = .4),
      sliderInput("axis_sz",label = "Axis title size", min = 8, max = 30,value = 1),
      selectInput("channel_id_list", label = "Channel ID", choices = c(1,2,3,4,5,6,7,8), multiple=TRUE),
      selectInput("LED", label = "LED", choices = c(680, 720), multiple=TRUE)
    ),
    
    mainPanel(
      plotOutput("Species_plot"),
      verbatimTextOutput("code1"),
      verbatimTextOutput("code2")
    )
  )
  
)

server <- function(input,output) {
  
  data <- reactive({data_list[[match(input$dataset, csv_names)]]})
  
  output$Species_plot <- renderPlot({myPlot(data(), input$x, input$y, input$color, input$pt_size, input$axis_sz, input$channel_id_list, input$LED)})
  
  output$code1 <- renderPrint({myText(data(), input$trait1, input$trait2, input$pt_size, input$axis_sz )})
  
}

shinyApp(ui, server)

###







