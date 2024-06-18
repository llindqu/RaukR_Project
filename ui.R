library(shiny)
library(tidyverse)

## Define the path to the directory containing the CSV files
csv_files <- list.files(path = "data", pattern = "\\.csv$", full.names = TRUE) # list files in data folder
csv_names <- tools::file_path_sans_ext(basename(csv_files)) # extract the unique names of the files
data_list <- lapply(csv_files, read.csv) # load all files into a list of data frames

# checkboxInput('LED', label = 'LED', choices = c(680, 720))
## Creation of the UI
# column(3, selectInput("LED", label = "LED", choices = c(680, 720), multiple=TRUE), checkboxInput("split_2", "split"))

ui <- fluidPage(
  titlePanel("Cultivation Visualization"),
  # first row with fields to select cultivation data sets & checkboxes for channels
  fluidRow(
    column(3, selectInput("dataset_1", label = "select cultivation 1", choices = csv_names), # selection of the first (left) cultivation data set
           checkboxGroupInput("channels_1", label = "Channels", choiceNames = c(1:8), choiceValues = c(1:8), inline = TRUE, selected = c(1:8)), # checkboxes to choose which channels to show for dataset 1
           sliderInput("x_lim_1", label = "Select x bar", min=0, max=200, value = c(0,100), step = NULL), 
           sliderInput("y_lim_1", label = "Select y bar", min=0, max=5, value = c(0,1), step = NULL)
    ),
    column(3, selectInput("LED1", label = "LED1", choices = c(680, 720), multiple=TRUE), checkboxInput("split_1", "split")),
    column(3, selectInput("dataset_2", label = "select cultivation 2", choices = csv_names), # selection of the second (right) cultivation data set
           checkboxGroupInput("channels_2", label = "Channels", choiceNames = c(1:8), choiceValues = c(1:8), inline = TRUE, selected = c(1:8)), # checkboxes to choose which channels to show for dataset 2
           sliderInput("x_lim_2", label = "Select x bar", min=0, max=200, value = c(0,100), step = NULL),
           sliderInput("y_lim_2", label = "Select y bar", min=0, max=5, value = c(0,1), step = NULL)
    ),
    column(3, selectInput("LED2", label = "LED2", choices = c(680, 720), multiple=TRUE), checkboxInput("split_2", "split"))
  ),
  # the second row which displays the plots
  fluidRow(
    column(6, plotOutput("plot_output_1", width = "600px", height = "300px")), # plot from dataset 1 (left)
    column(6, plotOutput("plot_output_2", width = "600px", height = "300px")) # plot from dataset 2 (right)
  )
)

cols <- c("1" = "#14a73f", "2" = '#1ac658', "3" = '#29ea9e', "4" = '#3ae2f4', "5" = '#4beedd', "6" = '#2cb195', "7"="#0d744c", "8"="#1e543e")

# function to create the standard cultivation plot (all channels in the same plot)
std_plot <- \(data, ch_id, LED, x_limit, y_limit) {
  tmp_df <- subset(data, od_led == LED) %>% filter(channel_id %in% ch_id)
  tmp <- ggplot(tmp_df, aes(x = batchtime_h, y = od_corr, color = as.factor(channel_id))) + theme_bw() + geom_point() +
    theme(legend.position = "top", legend.title = element_blank()) + scale_colour_manual(values = cols) + xlim(x_limit) + ylim(y_limit)
  return(tmp)
}

# function to create the split cultivation plot (one plot per channel)
split_plot <- \(data, ch_id, LED) {
  tmp_df <- subset(data, od_led == LED) %>% filter(channel_id %in% ch_id)
  tmp <- ggplot(tmp_df, aes(x = batchtime_h, y = od_corr, color = as.factor(channel_id))) + theme_bw() + geom_point() +
    theme(legend.position = "top", legend.title = element_blank()) + facet_wrap(vars(channel_id), nrow = 4) + scale_colour_manual(values = cols)
  return(tmp)
}

# creation of the server function
server <- function(input, output, session){
  
  data_1 <- reactive({data_list[[match(input$dataset_1, csv_names)]]})
  
  data_2 <- reactive({data_list[[match(input$dataset_2, csv_names)]]})
  
  observe({
    x_max_1 <- ceiling(max(data_1()$batchtime_h))
    x_max_2 <- ceiling(max(data_2()$batchtime_h))
    updateSliderInput(session, "x_lim_1", max = x_max_1+5, value = c(0,x_max_1))
    updateSliderInput(session, "x_lim_2", max = x_max_2+5, value = c(0,x_max_2))
  })
  
  observe({
    y_max_1 <- ceiling(max(data_1()$od_corr))
    y_max_2 <- ceiling(max(data_2()$od_corr))
    updateSliderInput(session, "y_lim_1", max = y_max_1, value = c(0,y_max_1))
    updateSliderInput(session, "y_lim_2", max = y_max_2, value = c(0,y_max_2))
  })
  
  # Rendering of the plot from dataset 1 (left)
  output$plot_output_1 <- renderPlot({
    if (input$split_1 == TRUE) { # if the split checkbox is checked, use the split plot function, otherwise use the standard plot function
      split_plot(data = data_1(), ch_id = input$channels_1, input$LED1) # call the split plot function
    } else {
      std_plot(data = data_1(), ch_id = input$channels_1, input$LED1, input$x_lim_1, input$y_lim_1) # call the standard plot function
    }
  })
  
  output$plot_output_2 <- renderPlot({
    if (input$split_2 == TRUE) { # if the split checkbox is checked, use the split plot function, otherwise use the standard plot function
      split_plot(data = data_2(), ch_id = input$channels_2, input$LED2) # call the split plot function
    } else {
      std_plot(data = data_2(), ch_id = input$channels_2, input$LED2, input$x_lim_2, input$y_lim_2) # call the standard plot function
    }
  })
}

# run shiny app
shinyApp(ui=ui,server=server)
