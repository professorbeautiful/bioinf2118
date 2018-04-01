#  Bias-variance-smoothing-shrinking

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bias-variance-smoothing-shrinking"),
   
   fluidPage(title = 'Bias-variance-smoothing-shrinking', 
             column(width = 6, 
                    h1("Notebook"),
                    wellPanel("stuff here")),
             
             column(width = 6, 
                    h1("Response by Predictor"),
                    wellPanel("stuff here"))
   
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

