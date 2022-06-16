

library(shiny)

source("simplebayesdecision.R")
source('inclRmd.R')
source('gerdPlot.R')
inclRmd('Simple_Bayes_Treatment_Decision/Simple_Bayes_Treatment_Decision-include.Rmd')


longparamnames = c(prev="prevalence", 
                   sens="sensitivity", spec="specificity",
                   L.SW="Loss_given_S_and_W",
                   L.HT="Loss_given_H_and_T")
paramArgDefaults = unlist(formals(plot.simple.bayes)[1:5])
###  a few alterations...
paramArgDefaults['prev'] = 0.1
paramArgDefaults['sens'] = 0.95
paramArgDefaults['spec'] = 0.95

paramArgNames = names(paramArgDefaults)
paramPlotRangeDefaults = c(
    prev='1/10^seq(0.5,4,0.1)',
    sens='seq(0.1,1,.01)',
    spec='seq(0.99,1,.0002)',
    L.SW='seq(0.5,20,1)',
    L.HT='seq(0,1,0.05)')

ui <- fluidPage(
    inputPanel(
        br(),
        numericInput('prevalenceDefault',
                     'prevalence ',
                     value = paramArgDefaults['prev'],
                     min = 0, max=1, step = 0.01),
        numericInput('sensitivityDefault',
                     'sensitivity ',
                     value = paramArgDefaults['sens'],
                     min = 0, max=1, step = 0.01),
        numericInput('specificityDefault',
                     'specificity ',
                     value = paramArgDefaults['spec'],
                     min = 0, max=1, step = 0.01),
        br(),
        numericInput('L.SWDefault',
                     'Loss Sick&Wait ',
                     value = paramArgDefaults['L.SW'],
                     min = 0, max=1, step = 0.01),
        numericInput('L.HTDefault',
                     'Loss Healthy&Treat ',
                     value = paramArgDefaults['L.HT'],
                     min = 0, max=1, step = 0.01),
        
        br(),br(),
        selectInput("parameterInputID", label = "Parameter to vary:",
                    choices = longparamnames, 
                    selected = longparamnames[1]),
        selectInput("dataInputID", label="Observation",
                    choices = c("P","N"), selected="P")
    )
)

server <- function(input, output) {
    renderPlot({
        ## First, set all parameters to their defaults.
        paramArgDefaults['prev'] = input$prevalenceDefault
        paramArgDefaults['sens'] = input$sensitivityDefault
        paramArgDefaults['spec'] = input$specificityDefault
        paramArgDefaults['L.SW'] = input$L.SWDefault
        paramArgDefaults['L.HT'] = input$L.HTDefault
        sapply(paramArgNames, function(param) 
            assign(param, paramArgDefaults[param], envir=.GlobalEnv))
        ### Here pos=1 is the same as env=.GlobalEnv -- try search().
        ### Now, swap in the vector for the selected parameter.
        which.varies = names(longparamnames)[which(input$parameterInputID == longparamnames)]
        cat(which.varies, '- ',
            longparamnames[which.varies], "--  ",
            "\n")
        horiz.label = input$parameterInputID 
        horiz.value.list = sapply(
            names(paramPlotRangeDefaults),
            function(the.rangeName)
                eval(parse(text=
                               paramPlotRangeDefaults[the.rangeName]))
        )
        horiz.values = horiz.value.list[[which.varies]]
        #Save the default value, and reassign the vector of values
        
        default.value = paramArgDefaults[which.varies]
        # You need to use "assign"; we doncan't hard-code the variable name.
        cat ('paramArgNames[which.varies] ',
             paramArgNames[which.varies], '\n')
        assign(which.varies,
               horiz.values)
        cat(horiz.label, 'default VALUE=',
            default.value, '\nvalues\n', 
            horiz.values, "\n")
        #  assign(input$parameterInputID, horiz.values)
        E.loss = simple.bayes.vectorized(
            prev, sens, spec, L.SW, L.HT, input$dataInputID)
        cat('E.loss', '\n')
        print(E.loss)
        E.loss.W = E.loss[ , 1]
        E.loss.T = E.loss[ , 2]
        plot(
            c(horiz.values, horiz.values), c(E.loss.W, E.loss.T), pch=" ",
            ylab="Bayes expected loss",
            xlab=input$parameterInputID,
            main=paste("Varying ", input$parameterInputID))
        lines (horiz.values, E.loss.W, pch= "W", col="red", lwd=3)
        lines (horiz.values, E.loss.T, pch= "T", col="green", lwd=3)
        points(horiz.values, E.loss.W, pch= "W", col="red", cex=4)
        points(horiz.values, E.loss.T, pch= "T", col="green", cex=4)
        abline(v=default.value, lty=3, lwd=10, col="blue")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
