library(shiny)


checkbox_group <- \(aa) {
  fluidRow(map(as.list(1:8), \(ii) {
    column(1, checkboxInput(paste("dataset_", as.character(aa), "_box_", as.character(ii)), 
                            label = as.character(ii), value = TRUE))
    
  }))
}


ui <- fluidPage(
  titlePanel("Cultivation Visualization"),
  fluidRow(
    column(6, selectInput("dataset_1", label = "select cultivation 1", choices = list.files("data")),
           helpText("Channels"),
           checkbox_group(1)
    ),
    column(6, selectInput("dataset_2", label = "select cultivation 2", choices = list.files("data")),
           helpText("Channels"),
           checkbox_group(2)
    )
  )
)




server <- function(input,output){}
shinyApp(ui=ui,server=server)