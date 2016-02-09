library(shiny)
source('powerPlot.R')
function(input, output) {

  output$powerGraph = renderPlot({
      powerplot.z(delta=input$deltaSlider, alpha=input$alphaInput,
                  n=input$nInput, tails="upper")
  })
}
