fluidPage(
  # Application title
  titlePanel("Connectivity Cloud"),
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a Indicator:",
                  choices = indicators),
      #actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Subscriptions (Per 100 inhabitants):",
                  min = 0.01,  max = 300, value = 300,
                  step = 0.01,
                  animate =
                    animationOptions(interval = 2200, loop = TRUE)
                  ),#value = c(0.01,300)),
      sliderInput("max",
                  "Maximum Number of Countries:",
                  min = 1,  max = 30,  value = 5),
      sliderInput("year",
                  "Census Year:",
                  min = 2008,  max = 2018,  value = 2018,
                  step = 1,
                  pre = "Year ", sep = "",
                  animate = animationOptions(interval = 2200)
                  ),      checkboxInput("checkbox", "analyze lower performing Countries")
    ),
    # Show Word Cloud
    mainPanel(
      plotOutput(outputId = "plot", width = "100%")
    )
  )
)
