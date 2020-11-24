fluidPage(
  div(class="outer",
            tags$head(
              # Include our custom CSS
              includeCSS("styles.css"),
                #includeScript("gomap.js")
            ),
            # If not using custom CSS, set height of leafletOutput to a number instead of percent
            #leafletOutput("map", width="100%", height="100%"),
            tmapOutput("map", width="100%", height="100%"),
            # Shiny versions prior to 0.11 should use class = "modal" instead.
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    h2("Connectivity Explorer"),
                    selectInput("Indicator", "Indicator",choices = c("Fixed-broadband subscriptions","Fixed-telephone subscriptions","Mobile-cellular telephone subscriptions")),
                    sliderInput("Year",
                                "Census Year:",
                                min = 2008,  max = 2018,  value = 2018,
                                step = 1,
                                pre = "Year ", sep = "",
                                animate = animationOptions(interval = 8200)
                    ),
                    #HTML("<br>"),
                    #selectInput("size", "Size", c("vars","adultpop"), selected = "adultpop")#,
                    #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                    # Only prompt for threshold when coloring or sizing by superzip
                    #  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                    #plotOutput("scatterCollegeIncome", height = 250)
                      )
                    #plotOutput("histCentile", height = 200),
                  
      ) 
)
