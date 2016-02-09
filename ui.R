library(shiny)

fluidPage(
 

    titlePanel("Statistical Power Demo"),
 

    sidebarLayout(
        sidebarPanel(
          radioButtons("distRadio", "Distribution type:",
                       c("Normal" = "norm",
                         "t" = "t",
                         "F" = "F")),
          numericInput("muInput", "mean", value=0, step=1),
          numericInput("alphaInput", "alpha", value=0.05, step=0.01),
          numericInput("powerInput", "power", value=0.8, step=0.1),
          numericInput("betaInput", "beta", value=0.2, step=0.1),
          numericInput("sigmaInput","sigma", value=1, step=1),
          numericInput("nInput", "n", value=30, step=1),
          sliderInput("deltaSlider", "delta", min=0, max=5, value=3)
        ),
 

        mainPanel(
          plotOutput("powerGraph")
        )
    )
)
