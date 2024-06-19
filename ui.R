library(shiny)
library(tidyverse)
library(shinythemes)

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
    column(3, selectInput("dataset_1", label = "Select cultivation 1", choices = csv_names), # selection of the first (left) cultivation data set
           checkboxGroupInput("channels_1", label = "Channels", choiceNames = c(1:8), choiceValues = c(1:8), inline = TRUE, selected = c(1:8)), # checkboxes to choose which channels to show for dataset 1
    ),
    column(3, selectInput("LED1", label = "LED1", choices = c(680, 720), multiple=TRUE, selected=720), checkboxInput("split_1", "Split")),
    column(3, selectInput("dataset_2", label = "Select cultivation 2", choices = csv_names), # selection of the second (right) cultivation data set
           checkboxGroupInput("channels_2", label = "Channels", choiceNames = c(1:8), choiceValues = c(1:8), inline = TRUE, selected = c(1:8)), # checkboxes to choose which channels to show for dataset 2
    ),
    column(3, selectInput("LED2", label = "LED2", choices = c(680, 720), multiple=TRUE, selected=720), checkboxInput("split_2", "Split"))
  ),
  fluidRow(
    column(3,sliderInput("x_lim_1", label = "X-axis range", min=0, max=200, value = c(0,100), step = NULL)),
    column(3,sliderInput("y_lim_1", label = "Y-axis range", min=0, max=5, value = c(0,1), step = NULL)),
    column(3,sliderInput("x_lim_2", label = "X-axis range", min=0, max=200, value = c(0,100), step = NULL)),
    column(3,sliderInput("y_lim_2", label = "Y-axis range", min=0, max=5, value = c(0,1), step = NULL))
  ),
  
  # the second row which displays the plots
  fluidRow(
    column(6, plotOutput("plot_output_1", width = "100%", height = "500px")), # plot from dataset 1 (left)
    column(6, plotOutput("plot_output_2", width = "100%", height = "500px")) # plot from dataset 2 (right)
  ),
  fluidRow(
    column(12, plotOutput("unite", width = "1200px", height = "600px")),
    fluidRow(
      column(6, sliderInput("x_lim_u", label = "X-axis range", min=0, max=200, value = c(0,100), step = NULL)),
      column(6, sliderInput("y_lim_u", label = "Y-axis range", min=0, max=5, value = c(0,1), step = NULL))
    )
  )
)
cols_1 <- c("1" = "#14a73f", "2" = '#1ac658', "3" = '#29ea9e', "4" = '#3ae2f4', "5" = '#4beedd', "6" = '#2cb195', "7" = "#0d744c", "8" = "#1e543e")
cols_2 <- c("1" = "#f63c83", "2" = "#d9478c", "3" = "#a25395", "4" = "#b34295", "5" = "#844d9b", "6" = "#59609b", "7" = "#5457a0", "8" = "#2a48b7")
cols_unite <- c("1.1" = "#14a73f", "1.2" = '#1ac658', "1.3" = '#29ea9e', "1.4" = '#3ae2f4', "1.5" = '#4beedd', "1.6" = '#2cb195', "1.7" = "#0d744c", "1.8" = "#1e543e", "2.1" = "#f63c83", "2.2" = "#d9478c", "2.3" = "#a25395", "2.4" = "#b34295", "2.5" = "#844d9b", "2.6" = "#59609b", "2.7" = "#5457a0", "2.8" = "#2a48b7")

# function to create the standard cultivation plot (all channels in the same plot)
std_plot <- \(data, ch_id, LED, x_limit, y_limit, cols) {
  tmp_df <- subset(data, od_led == LED) %>% filter(channel_id %in% ch_id)
  tmp <- ggplot(tmp_df, aes(x = batchtime_h, y = od_corr, color = as.factor(channel_id))) +
    theme_bw() + 
    geom_point() + 
    labs(title="Name of plot", x="Time (h)", y="OD") + 
    guides(colour = guide_legend(nrow = 2, override.aes=list(size = 4))) + 
    theme(legend.position = c(.95,.95), 
          legend.title = element_blank(),
          legend.justification = c("right", "top"),
          plot.title = element_text(size=20, hjust=0.5),
          axis.text = element_text(size = 10),
          axis.title = element_text(size=15),
          legend.text = element_text(size = 15)) + 
    scale_colour_manual(values = cols) + 
    xlim(x_limit) + 
    ylim(y_limit)
  return(tmp)
}


# function to create the split cultivation plot (one plot per channel)
split_plot <- \(data, ch_id, LED, x_limit, y_limit, cols) {
  tmp_df <- subset(data, od_led == LED) %>% filter(channel_id %in% ch_id)
  tmp <- ggplot(tmp_df, aes(x = batchtime_h, y = od_corr, color = as.factor(channel_id))) + 
    theme_bw() + 
    geom_point() + 
    labs(title="Split plot", x="Time (h)", y="OD") + 
    guides(colour = guide_legend(nrow = 1, override.aes=list(size = 5))) +
    theme(legend.position = "top", 
          strip.background = element_blank(), 
          #legend.justification = c("right", "top"),
          legend.title = element_blank(), 
          plot.title = element_text(size=20, hjust=0.5),
          axis.text = element_text(size = 10), 
          axis.title = element_text(size=15),
          legend.text = element_text(size = 15)) + 
    facet_wrap(vars(channel_id), nrow = 4) + 
    scale_colour_manual(values = cols) + 
    xlim(x_limit) + 
    ylim(y_limit)
  return(tmp)
}

guides(color = guide_legend( 
  override.aes=list(shape = 18)))

#+theme(axis.text=element_text(size=20), legend.text = element_text(size=10))

# function to create the united cultivation plot
unite_plot <- \(data_one, data_two, ch_id_one, ch_id_two, LED_1, LED_2, x_limit, y_limit) {
  
  tmp_df_1 <- subset(data_one, od_led == LED_1) %>% filter(channel_id %in% ch_id_one)
  tmp_df_2 <- subset(data_two, od_led == LED_2) %>% filter(channel_id %in% ch_id_two)
  
  tmp_df_1$channel_id <- as.numeric(paste("1.", tmp_df_1$channel_id, sep = ""))
  tmp_df_2$channel_id <- as.numeric(paste("2.", tmp_df_2$channel_id, sep = ""))
  
  tmp_df <- rbind(tmp_df_1, tmp_df_2)
  
  tmp <- ggplot(tmp_df, aes(x = batchtime_h, y = od_corr, color = as.factor(channel_id))) + 
    theme_bw() + 
    geom_point() + 
    labs(title="United plot", x="Time (h)", y="OD") +
    guides(colour = guide_legend(nrow = 4, override.aes=list(size = 5))) +
    theme(legend.position = c(.95,.95), 
          legend.title = element_blank(),
          legend.justification = c("right", "top"),
          plot.title = element_text(size=20, hjust=0.5),
          axis.text = element_text(size = 10), 
          axis.title = element_text(size=15),
          legend.text = element_text(size = 15)) + 
    scale_colour_manual(values = cols_unite) + 
    xlim(x_limit) + 
    ylim(y_limit) 
  return(tmp)
}

# creation of the server function
server <- function(input, output,session){
  
  data_1 <- reactive({data_list[[match(input$dataset_1, csv_names)]]})
  
  data_2 <- reactive({data_list[[match(input$dataset_2, csv_names)]]})
  
  observe({
    x_max_1 <- ceiling(max(data_1()$batchtime_h))
    x_max_2 <- ceiling(max(data_2()$batchtime_h))
    updateSliderInput(session, "x_lim_1", max = x_max_1, value = c(0,x_max_1))
    updateSliderInput(session, "x_lim_2", max = x_max_2, value = c(0,x_max_2))
  })
  
  observe({
    y_max_1 <- ceiling(max(data_1()$od_corr))
    y_max_2 <- ceiling(max(data_2()$od_corr))
    updateSliderInput(session, "y_lim_1", max = y_max_1, value = c(0,y_max_1))
    updateSliderInput(session, "y_lim_2", max = y_max_2, value = c(0,y_max_2))
  })
  
  observe({
    x_max_u <- ceiling(max(c(data_1()$batchtime_h, data_2()$batchtime_h)))
    y_max_u <- ceiling(max(c(data_1()$od_corr, data_2()$od_corr)))
    updateSliderInput(session, "x_lim_u", max = x_max_u, value = c(0,x_max_u))
    updateSliderInput(session, "y_lim_u", max = y_max_u, value = c(0,y_max_u))
  })
  # Rendering of the plot from dataset 1 (left)
  output$plot_output_1 <- renderPlot({
    if (input$split_1 == TRUE) { # if the split checkbox is checked, use the split plot function, otherwise use the standard plot function
      split_plot(data = data_1(), ch_id = input$channels_1, input$LED1, input$x_lim_1, input$y_lim_1, cols_1) # call the split plot function
    } else {
      std_plot(data = data_1(), ch_id = input$channels_1, input$LED1, input$x_lim_1, input$y_lim_1, cols_1) # call the standard plot function
    }
  })
  
  output$plot_output_2 <- renderPlot({
    if (input$split_2 == TRUE) { # if the split checkbox is checked, use the split plot function, otherwise use the standard plot function
      split_plot(data = data_2(), ch_id = input$channels_2, input$LED2, input$x_lim_2, input$y_lim_2, cols_2) # call the split plot function
    } else {
      std_plot(data = data_2(), ch_id = input$channels_2, input$LED2, input$x_lim_2, input$y_lim_2, cols_2) # call the standard plot function
    }
  })
  output$unite <- renderPlot({
    unite_plot(data_one = data_1(), data_two = data_2(), ch_id_one = input$channels_1, 
               ch_id_two = input$channels_2, input$LED1, input$LED2, input$x_lim_u, input$y_lim_u)
  })
}

# run shiny app
shinyApp(ui=ui,server=server)



#df_1 <- data_list[[1]]
#df_2 <- data_list[[2]]
#channel_id_1 <- df_1$channel_id
#channel_id_2 <- df_2$channel_id
#LED <- 720
#x_limit=c(0,120)
#y_limit=c(0,7)
#cols_unite <- c("1.1" = "#14a73f", "1.2" = '#1ac658', "1.3" = '#29ea9e', "1.4" = '#3ae2f4', "1.5" = '#4beedd', "1.6" = '#2cb195', "1.7" = "#0d744c", "1.8" = "#1e543e", "2.1" = "#f63c83", "2.2" = "#d9478c", "2.3" = "#a25395", "2.4" = "#b34295", "2.5" = "#844d9b", "2.6" = "#59609b", "2.7" = "#5457a0", "2.8" = "#2a48b7")
#
#unite_plot <- \(data_one, data_two, ch_id_one, ch_id_two, LED, x_limit, y_limit) {
#  
#  tmp_df_1 <- subset(data_one, od_led == LED) %>% filter(channel_id %in% ch_id_one)
#  tmp_df_2 <- subset(data_two, od_led == LED) %>% filter(channel_id %in% ch_id_two)
#  
#  tmp_df_1$channel_id <- as.numeric(paste("1.", tmp_df_1$channel_id, sep = ""))
#  tmp_df_2$channel_id <- as.numeric(paste("2.", tmp_df_2$channel_id, sep = ""))
#  
#  tmp_df <- rbind(tmp_df_1, tmp_df_2)
#  
#  tmp <- ggplot(tmp_df, aes(x = batchtime_h, y = od_corr, color = as.factor(channel_id))) + theme_bw() + geom_point() +
#    theme(legend.position = "top", legend.title = element_blank()) + scale_colour_manual(values = cols_unite) + xlim(x_limit) + ylim(y_limit)
#  return(tmp)
#}
#
#unite_plot(df_1, df_2, channel_id_1, channel_id_2, LED, x_limit, y_limit)
