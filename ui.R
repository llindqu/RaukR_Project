library(shiny)
library(tidyverse)
library(shinythemes)
library(ggcats)
library(shinyWidgets)
library(gganimate)
library(magick)

## Define the path to the directory containing the CSV files
csv_files <- list.files(path = "data", pattern = "\\.csv$", full.names = TRUE) # list files in data folder
csv_names <- tools::file_path_sans_ext(basename(csv_files)) # extract the unique names of the files
data_list <- lapply(csv_files, read_csv) # load all files into a list of data frames

ui <- fluidPage(
  # All your styles will go here
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #3ae2f4}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #3ae2f4}")),
  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #b34295}")),
  tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #b34295}")),
  tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #5457a0}")),
  tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #5457a0}")),
  tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #3ae2f4}")),
  tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: #3ae2f4}")),
  tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {background: #b34295}")),
  tags$style(HTML(".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-9 .irs-bar {background: #b34295}")),
  
  titlePanel("Cultivation Visualization"),
  # first row with fields to select cultivation data sets & checkboxes for channels
  fluidRow(theme = shinytheme("slate"),
    column(3, style = "background-color:#ECFDFC;", 
           selectInput("dataset_1", label = "Select cultivation 1", choices = csv_names), # selection of the first (left) cultivation data set
           checkboxGroupInput("channels_1", label = "Channels", choiceNames = c(1:8), choiceValues = c(1:8), inline = TRUE, selected = c(1:8)), # checkboxes to choose which channels to show for dataset 1
           ),
    column(3,  
           selectInput("LED1", label = "LED1", choices = c(680, 720), multiple=TRUE, selected=720), 
           radioButtons("split_1", label = "Split", choiceNames = c("Yes","No"), choiceValues = c("Yes","No"), inline = TRUE, selected = "No")
           ),
    column(3, style = "background-color:#F9F0F7;", 
           selectInput("dataset_2", label = "Select cultivation 2", choices = csv_names), # selection of the second (right) cultivation data set
           checkboxGroupInput("channels_2", label = "Channels", choiceNames = c(1:8), choiceValues = c(1:8), inline = TRUE, selected = c(1:8)), # checkboxes to choose which channels to show for dataset 2
           ),
    column(3, 
           selectInput("LED2", label = "LED2", choices = c(680, 720), multiple=TRUE, selected=720), 
           radioButtons("split_2", label = "Split", choiceNames = c("Yes","No"), choiceValues = c("Yes","No"), inline = TRUE, selected = "No")
           )
    ),
  fluidRow(
    column(3,sliderInput("x_lim_1", label = "X-axis range", min=0, max=200, value = c(0,100), step = NULL)),
    column(3,sliderInput("y_lim_1", label = "Y-axis range", min=0, max=5, value = c(0,1), step = 0.005)),
    column(3,sliderInput("x_lim_2", label = "X-axis range", min=0, max=200, value = c(0,100), step = NULL)),
    column(3,sliderInput("y_lim_2", label = "Y-axis range", min=0, max=5, value = c(0,1), step = 0.005))
  ),
  
  fluidRow( 
    column(6,HTML("<br><br>"),style = "background-color:#ECFDFC;"),
    column(6,HTML("<br><br>"),style = "background-color:#F9F0F7;")
  ),
  
  # the second row which displays the plots
  fluidRow(
    column(6, plotOutput("plot_output_1", width = "100%", height = "500px")), # plot from dataset 1 (left)
    column(6, plotOutput("plot_output_2", width = "100%", height = "500px")) # plot from dataset 2 (right)
  ),
  
  fluidRow( 
    column(6,HTML("<br>"),style = "background-color:white"),
    column(6,HTML("<br>"),style = "background-color:white;")
  ),
  fluidRow(HTML("<br><br>"),style = "background-color:#E4E4F1;"),
  
  # third row for united plot display
  fluidRow(
    column(12, align="center", plotOutput("unite", width = "80%", height = "600px")),
    fluidRow(style = "background-color:#E4E4F1;",
      column(3),
      column(3, align="center", sliderInput("x_lim_u", label = "X-axis range", min=0, max=200, value = c(0,100), step = NULL, width="100%")),
      column(3, align="center", sliderInput("y_lim_u", label = "Y-axis range", min=0, max=5, value = c(0,1), step = 0.005, width="100%")),
      column(3, radioButtons("split_unite", label = "Split", choiceNames = c("Yes","No"), choiceValues = c("Yes","No"), inline = TRUE, selected = "No"))
    )
  ),
  
  fluidRow(HTML("<br>"),style = "background-color:white;"),
  
  # new row for growth rate plots
  fluidRow(
    column(6, plotOutput("g_rate_1", width = "100%", height = "500px"),
           fluidRow(
                    column(6, align="center", sliderInput("x_lim_gr_1", label = "X-axis range", min=0, max=200, value = c(0,100), step = NULL, width="100%")),
                    column(6, align="center", sliderInput("y_lim_gr_1", label = "Y-axis range", min=0, max=1, value = c(0,0.2), step = 0.005, width="100%"))
           )), # plot from dataset 1 (left)
    column(6, plotOutput("g_rate_2", width = "100%", height = "500px"),
           fluidRow(
                    column(6, align="center", sliderInput("x_lim_gr_2", label = "X-axis range", min=0, max=200, value = c(0,100), step = NULL, width="100%")),
                    column(6, align="center", sliderInput("y_lim_gr_2", label = "Y-axis range", min=0, max=1, value = c(0,0.2), step = 0.005, width="100%"))
           ) # plot from dataset 2 (right)
  )),
  
  
  
  fluidRow(
    column(12, style = "background-color:#E4E4F1;", checkboxInput("cat_input", label="cats?", value=FALSE))
  )
)
cols_1 <- c("1" = "#14a73f", "2" = '#1ac658', "3" = '#29ea9e', "4" = '#3ae2f4', "5" = '#4beedd', "6" = '#2cb195', "7" = "#0d744c", "8" = "#1e543e")
cols_2 <- c("1" = "#f63c83", "2" = "#d9478c", "3" = "#a25395", "4" = "#b34295", "5" = "#844d9b", "6" = "#59609b", "7" = "#5457a0", "8" = "#2a48b7")
cols_unite <- c("1.1" = "#14a73f", "1.2" = '#1ac658', "1.3" = '#29ea9e', "1.4" = '#3ae2f4', "1.5" = '#4beedd', "1.6" = '#2cb195', "1.7" = "#0d744c", "1.8" = "#1e543e", "2.1" = "#f63c83", "2.2" = "#d9478c", "2.3" = "#a25395", "2.4" = "#b34295", "2.5" = "#844d9b", "2.6" = "#59609b", "2.7" = "#5457a0", "2.8" = "#2a48b7")
nyan_cols <- c("1" = "#ED4242", "2" = '#FFEA47', "3" = '#68B84D', "4" = '#58F8B8', "5" = '#54B5FF', "6" = '#555BFF', "7" = "#AA71F3", "8" = "#FF86E7")

# function to create the standard cultivation plot (all channels in the same plot)
std_plot <- \(data, ch_id, LED, x_limit, y_limit, cols, heading) {
  tmp_df <- subset(data, od_led == LED) %>% filter(channel_id %in% ch_id)
  tmp <- ggplot(tmp_df, aes(x = batchtime_h, y = od_raw, color = as.factor(channel_id))) +
    theme_bw() + 
    geom_point() + 
    labs(title = paste(heading, " - OD Plot", sep = ""), x = "Time (h)", y = "OD") + 
    guides(colour = guide_legend(nrow = 2, override.aes=list(size = 4))) + 
    theme(legend.position = c(.95,.95), 
          legend.title = element_blank(),
          legend.justification = c("right", "top"),
          plot.title = element_text(size=20, hjust=0.5, face="bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size=15),
          legend.text = element_text(size = 15)) + 
    scale_colour_manual(values = cols) + 
    xlim(x_limit) + 
    ylim(y_limit)
  return(tmp)
}


# function to create the split cultivation plot (one plot per channel)
split_plot <- \(data, ch_id, LED, x_limit, y_limit, cols, heading) {
  tmp_df <- subset(data, od_led == LED) %>% filter(channel_id %in% ch_id)
  tmp <- ggplot(tmp_df, aes(x = batchtime_h, y = od_raw, color = as.factor(channel_id))) + 
    theme_bw() + 
    geom_point() + 
    labs(title = paste(heading, " - OD Plot", sep = ""), x = "Time (h)", y = "OD") + 
    guides(colour = guide_legend(nrow = 1, override.aes=list(size = 5))) +
    theme(legend.position = "top", 
          strip.background = element_blank(), 
          #legend.justification = c("right", "top"),
          legend.title = element_blank(), 
          plot.title = element_text(size=20, hjust=0.5, face="bold"),
          axis.text = element_text(size = 10), 
          axis.title = element_text(size=15),
          legend.text = element_text(size = 15)) + 
    facet_wrap(vars(channel_id), nrow = 4) + 
    scale_colour_manual(values = cols) + 
    xlim(x_limit) + 
    ylim(y_limit)
  return(tmp)
}


# Cat plot function
CAT_PLOT_ANIM <- function(data, LED, x_limit, y_limit){
  data <- subset(data, od_led == LED) %>% filter(channel_id %in% c(1:8))
  
  data$cat <- rep("nyancat", nrow(data))
  data$cat[data$channel_id == "1"] <- rep(c("pop_close", "pop"), 500)
  
  cat_gif <- ggplot(data, aes(x=batchtime_h, y=od_raw, group=as.factor(channel_id), color=as.factor(channel_id))) +
    geom_line(linewidth=1) +
    geom_cat(aes(cat = cat), size = 3) +
    scale_colour_manual(values = nyan_cols) +
    theme_bw() +
    ggtitle("NyanobCateria") +
    ylab("Number of cats born") +
    ylim(y_limit) +
    xlim(x_limit) +
    theme(legend.position = "none",
          plot.title = element_text(size=20, face="bold"),
          panel.background = element_rect(fill = "black"),
          panel.grid.major = element_line(color = "white",linewidth = 1, linetype = "1F"),
          panel.grid.minor = element_line(color = "white",linewidth = 0.5, linetype = "1F")) +
    transition_reveal(batchtime_h)
  return(cat_gif)
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
  
  tmp <- ggplot(tmp_df, aes(x = batchtime_h, y = od_raw, color = as.factor(channel_id))) + 
    theme_bw() + 
    geom_point() + 
    labs(title = "United plot", x="Time (h)", y="OD") +
    guides(colour = guide_legend(nrow = 4, override.aes=list(size = 5))) +
    theme(legend.position = c(.95,.95), 
          legend.title = element_blank(),
          legend.key.width=unit(1,"cm"),
          legend.justification = c("right", "top"),
          plot.title = element_text(size=20, hjust=0.5,face="bold"),
          axis.text = element_text(size = 10), 
          axis.title = element_text(size=15),
          plot.background = element_rect(fill="#E4E4F1", color="#E4E4F1"),
          legend.text = element_text(size = 15)) + 
    scale_colour_manual(values = cols_unite) + 
    xlim(x_limit) + 
    ylim(y_limit) 
  return(tmp)
}

# function to create unite split plot
unite_split_plot <- \(data_one, data_two, ch_id_one, ch_id_two, LED_1, LED_2, x_limit, y_limit) {
  
  tmp_df_1 <- subset(data_one, od_led == LED_1) %>% filter(channel_id %in% ch_id_one)
  tmp_df_2 <- subset(data_two, od_led == LED_2) %>% filter(channel_id %in% ch_id_two)
  
  tmp_df_1$channel_id <- paste("1.", tmp_df_1$channel_id, sep = "")
  tmp_df_2$channel_id <- paste("2.", tmp_df_2$channel_id, sep = "")
  
  tmp_df <- rbind(tmp_df_1, tmp_df_2) %>% separate(channel_id, into = c("cultivation", "channel"), sep = "\\.", remove = FALSE)
  
  tmp <- ggplot(tmp_df, aes(x = batchtime_h, y = od_raw, color = as.factor(channel_id))) + 
    theme_bw() + 
    geom_point() + 
    labs(title="United plot", x = "Time (h)", y = "OD") +
    guides(colour = guide_legend(nrow = 1, override.aes=list(size = 5))) +
    theme(legend.position = "top", 
          strip.background = element_blank(), 
          #legend.justification = c("right", "top"),
          legend.title = element_blank(), 
          plot.title = element_text(size=20, hjust=0.5, face="bold"),
          axis.text = element_text(size = 10), 
          axis.title = element_text(size=15),
          legend.text = element_text(size = 15)) + 
    scale_colour_manual(values = cols_unite) + 
    xlim(x_limit) + 
    ylim(y_limit) +
    facet_wrap(vars(channel), nrow = 4)
  return(tmp)
}


# function to create growth rate plot
gr_plot <- \(data, ch_id, LED, x_limit, y_limit, cols, heading) {
  
  tmp_df <- subset(data, od_led == LED & batchtime_h > x_limit[1] & batchtime_h < x_limit[2]) %>% 
    filter(channel_id %in% ch_id) %>% group_by(channel_id) %>% group_split()
  
  df_chunk <- map(.x = tmp_df, .f = \(df) {
    
    df$chunk <- 1
    chunk_val <- 1
    chunk_count <- 1
    
    for (ii in 2:nrow(df)) {
      
      if (df$od_raw[ii] < df$od_raw[[ii-1]] * 0.95 && chunk_count > 1) {
        chunk_val <- chunk_val + 1
        chunk_count <- 0
      } 
      
      if (chunk_count >= 20) {
        chunk_val <- chunk_val + 1
        chunk_count <- 0
      }
      
      df$chunk[ii] <- chunk_val
      
      chunk_count <- chunk_count +1
      
    }
    
    return(df)
    
  })
  
  df_mu <- map(.x = df_chunk, .f = \(df) {
    
    df <- df %>% group_by(chunk) %>% group_split()
    
    tmp_mu <- map(.x = df, .f = \(df2) {
      tmp_sum <- summary(lm(log(od_raw) ~ batchtime_h, df2))
      tmp_df <- tibble(channel_id = df2$channel_id[1],
                       batchtime_h = max(df2$batchtime_h),
                       mu = tmp_sum$coefficients[2,1],
                       r_squared = tmp_sum$adj.r.squared
      )
    }) %>% bind_rows()
    
    tmp_mu$left <- c(0, tmp_mu$batchtime_h[1:(nrow(tmp_mu)-1)])
    
    scale_factor <- max(tmp_mu$mu, na.rm = TRUE) / max(tmp_mu$r_squared, na.rm = TRUE)
    
    tmp_mu$r_squared <- tmp_mu$r_squared * scale_factor
    
    return(tmp_mu)
    
  }) %>% bind_rows()
  
  gr <- ggplot(df_mu) + theme_bw() + 
    geom_rect(aes(xmin = left, xmax = batchtime_h, ymax = mu, ymin = 0, color = "white", fill = as.factor(channel_id)), alpha = 0.75) + 
    geom_point(aes(x = batchtime_h, y = r_squared)) +
    labs(title = paste(heading, " - Growth Rate", sep = ""), x = "Time (h)", y = "Growth Rate") + 
    guides(fill = guide_legend(nrow = 1, override.aes=list(size = 5))) +
    theme(legend.position = "top", 
          strip.background = element_blank(), 
          #legend.justification = c("right", "top"),
          legend.title = element_blank(), 
          plot.title = element_text(size=20, hjust=0.5, face="bold"),
          axis.text = element_text(size = 10), 
          axis.title = element_text(size=15),
          legend.text = element_text(size = 15)) + 
    facet_wrap(vars(channel_id), nrow = 4) + 
    scale_fill_manual(values = cols) + scale_color_manual(values = c("white"), breaks = c()) + 
    xlim(x_limit) + 
    ylim(y_limit)
  
  return(gr)
}

# creation of the server function
server <- function(input, output,session){
  
  data_1 <- reactive({data_list[[match(input$dataset_1, csv_names)]]})
  data_2 <- reactive({data_list[[match(input$dataset_2, csv_names)]]})
  
  observe({ #sliders for split&std plot (x)
    x_max_1 <- ceiling(max(data_1()$batchtime_h))
    x_max_2 <- ceiling(max(data_2()$batchtime_h))
    updateSliderInput(session, "x_lim_1", max = x_max_1, value = c(0,x_max_1))
    updateSliderInput(session, "x_lim_2", max = x_max_2, value = c(0,x_max_2))
  })
  
  observe({ #sliders for split&std plot (y)
    y_max_1 <- round(max(data_1()$od_raw),digits=0.1)+0.1
    y_max_2 <- round(max(data_2()$od_raw),digits=0.1)+0.1
    updateSliderInput(session, "y_lim_1", max = y_max_1, value = c(0,y_max_1))
    updateSliderInput(session, "y_lim_2", max = y_max_2, value = c(0,y_max_2))
  })
  
  observe({ #Sliders for united-plot
    x_max_u <- ceiling(max(c(data_1()$batchtime_h, data_2()$batchtime_h)))
    y_max_u <- round(max(c(data_1()$od_raw, data_2()$od_raw)),digits=0.1)+0.1
    updateSliderInput(session, "x_lim_u", max = x_max_u, value = c(0,x_max_u))
    updateSliderInput(session, "y_lim_u", max = y_max_u, value = c(0,y_max_u))
  })
  
  observe({ #Sliders for growth-rate-plot (x)
    x_max_1 <- ceiling(max(data_1()$batchtime_h))
    x_max_2 <- ceiling(max(data_2()$batchtime_h))
    updateSliderInput(session, "x_lim_gr_1", max = x_max_1, value = c(0,x_max_1))
    updateSliderInput(session, "x_lim_gr_2", max = x_max_2, value = c(0,x_max_2))
  })
  
 # observe({ #Sliders for growth-rate-plot (y)
#    y_max_1 <- round(max(data_1()$od_raw),digits=0.1)+0.1
#    y_max_2 <- round(max(data_2()$od_raw),digits=0.1)+0.1
#    updateSliderInput(session, "y_lim_gr_1", max = y_max_1, value = c(0,y_max_1))
#    updateSliderInput(session, "y_lim_gr_2", max = y_max_2, value = c(0,y_max_2))
#  })
  
  #output$unite <- renderPlot({
  #  unite_split_plot(data_one = data_1(), data_two = data_2(), ch_id_one = input$channels_1, 
  #             ch_id_two = input$channels_2, input$LED1, input$LED2, input$x_lim_u, input$y_lim_u)
  #})
  
  
  
  output$unite <- renderPlot({
    if (input$split_unite == "Yes") { # if the split checkbox is checked, use the split plot function, otherwise use the standard plot function
      unite_split_plot(data_one = data_1(), data_two = data_2(), ch_id_one = input$channels_1, 
                       ch_id_two = input$channels_2, input$LED1, input$LED2, input$x_lim_u, input$y_lim_u) # call the split plot function
    } else {
      unite_plot(data_one = data_1(), data_two = data_2(), ch_id_one = input$channels_1, 
                       ch_id_two = input$channels_2, input$LED1, input$LED2, input$x_lim_u, input$y_lim_u) # call the standard plot function
    }
  })
  
  
  
  
  output$g_rate_1 <- renderPlot({
    gr_plot(data = data_1(), ch_id = input$channels_1, input$LED1, input$x_lim_gr_1, input$y_lim_gr_1, cols_1, input$dataset_1) #placeholder
  })
  
  output$g_rate_2 <- renderPlot({
    gr_plot(data = data_2(), ch_id = input$channels_2, input$LED2, input$x_lim_gr_2, input$y_lim_gr_2, cols_2,input$dataset_2) #placeholder
  })
  
  
  observe({
    
    if(input$cat_input){
      # CAT PLOT 1
      output$plot_output_1 <- renderImage({
        p <- CAT_PLOT_ANIM(data=data_1(), input$LED1, input$x_lim_1, input$y_lim_1)
        anim <- animate(p, fps = 10, width=1000, duration = 5, renderer = magick_renderer())
        anim_save("output1.gif", anim)
        list(src = "output1.gif", contentType = 'image/gif')
      }, deleteFile = FALSE)
      
      output$plot_output_2 <- renderImage({
        p <- CAT_PLOT_ANIM(data=data_2(), input$LED2, input$x_lim_2, input$y_lim_2)
        anim <- animate(p, fps = 10, width=1000, duration = 5, renderer = magick_renderer())
        anim_save("output2.gif", anim)
        list(src = "output2.gif", contentType = 'image/gif')
      }, deleteFile = FALSE)
    }
    
    else {
      # Rendering of the plot from dataset 1 (left)
      output$plot_output_1 <- renderPlot({
        if (input$split_1 == "Yes") { # if the split checkbox is checked, use the split plot function, otherwise use the standard plot function
          split_plot(data = data_1(), ch_id = input$channels_1, input$LED1, input$x_lim_1, input$y_lim_1, cols_1, input$dataset_1) # call the split plot function
        } else {
          std_plot(data = data_1(), ch_id = input$channels_1, input$LED1, input$x_lim_1, input$y_lim_1, cols_1, input$dataset_1) # call the standard plot function
        }
      })
      
      output$plot_output_2 <- renderPlot({
        if (input$split_2 == "Yes") { # if the split checkbox is checked, use the split plot function, otherwise use the standard plot function
          split_plot(data = data_2(), ch_id = input$channels_2, input$LED2, input$x_lim_2, input$y_lim_2, cols_2,input$dataset_2) # call the split plot function
        } else {
          std_plot(data = data_2(), ch_id = input$channels_2, input$LED2, input$x_lim_2, input$y_lim_2, cols_2, input$dataset_2) # call the standard plot function
        }
      })
    }
  })
}

# run shiny app
shinyApp(ui=ui,server=server)