library(shiny)
library(tidyverse)

## Define the path to the directory containing the CSV files
csv_files <- list.files(path = "data", pattern = "\\.csv$", full.names = TRUE) # list files in data folder
csv_names <- tools::file_path_sans_ext(basename(csv_files)) # extract the unique names of the files
data_list <- lapply(csv_files, read.csv) # load all files into a list of data frames


## Creation of the UI

ui <- fluidPage(
  titlePanel("Cultivation Visualization"),
  # first row with fields to select cultivation data sets & checkboxes for channels
  fluidRow(
    column(3, selectInput("dataset_1", label = "select cultivation 1", choices = csv_names), # selection of the first (left) cultivation data set
           checkboxGroupInput("channels_1", label = "Channels", choiceNames = c(1:8), choiceValues = c(1:8), inline = TRUE, selected = c(1:8)) # checkboxes to choose which channels to show for dataset 1
    ),
    column(3, checkboxInput("split_1", "split")),
    column(3, selectInput("dataset_2", label = "select cultivation 2", choices = csv_names), # selection of the second (right) cultivation data set
           checkboxGroupInput("channels_2", label = "Channels", choiceNames = c(1:8), choiceValues = c(1:8), inline = TRUE, selected = c(1:8)) # checkboxes to choose which channels to show for dataset 2
    ),
    column(3, checkboxInput("split_2", "split"))
  ),
# the second row which displays the plots
  fluidRow(
    column(6, plotOutput("plot_output_1", width = "600px", height = "300px")), # plot from dataset 1 (left)
    column(6, plotOutput("plot_output_2", width = "600px", height = "300px")) # plot from dataset 2 (right)
  )
)

cols <- c("1" = "#14a73f", "2" = '#1ac658', "3" = '#29ea9e', "4" = '#3ae2f4', "5" = '#4beedd', "6" = '#2cb195', "7"="#0d744c", "8"="#1e543e")

# function to create the standard cultivation plot (all channels in the same plot)
std_plot <- \(data, ch_id) {
  tmp_df <- subset(data, od_led == 720) %>% filter(channel_id %in% ch_id)
  tmp <- ggplot(tmp_df, aes(x = batchtime_h, y = od_corr, color = as.factor(channel_id))) + theme_bw() + geom_point() +
    theme(legend.position = "top", legend.title = element_blank()) + scale_colour_manual(values = cols)
  return(tmp)
}

# function to create the split cultivation plot (one plot per channel)
split_plot <- \(data, ch_id) {
  tmp_df <- subset(data, od_led == 720) %>% filter(channel_id %in% ch_id)
  tmp <- ggplot(tmp_df, aes(x = batchtime_h, y = od_corr, color = as.factor(channel_id))) + theme_bw() + geom_point() +
    theme(legend.position = "top", legend.title = element_blank()) + facet_wrap(vars(channel_id), nrow = 4) + scale_colour_manual(values = cols)
  return(tmp)
}

# creation of the server function
server <- function(input, output){
  
  data_1 <- reactive({data_list[[match(input$dataset_1, csv_names)]]})
  
  data_2 <- reactive({data_list[[match(input$dataset_2, csv_names)]]})
  
  # Rendering of the plot from dataset 1 (left)
  output$plot_output_1 <- renderPlot({
    if (input$split_1 == TRUE) { # if the split checkbox is checked, use the split plot function, otherwise use the standard plot function
      split_plot(data = data_1(), ch_id = input$channels_1) # call the split plot function
    } else {
      std_plot(data = data_1(), ch_id = input$channels_1) # call the standard plot function
    }
  })
  
  output$plot_output_2 <- renderPlot({
    if (input$split_2 == TRUE) { # if the split checkbox is checked, use the split plot function, otherwise use the standard plot function
      split_plot(data = data_2(), ch_id = input$channels_2) # call the split plot function
    } else {
      std_plot(data = data_2(), ch_id = input$channels_2) # call the standard plot function
    }
  })
}

# run shiny app
shinyApp(ui=ui,server=server)
