library(networkD3)
library(shiny)
library(shinydashboard)
library(markdown)
source("./global.R")
markdown <- function(s) {
  s <- gsub("\\n[ \\t]*", "\n", s)
  HTML(markdownToHTML(fragment.only = TRUE, text = s))
}

ui <- fluidPage(
  # Application title
  titlePanel("increase/decrease in internet connectivity"),
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Summary",
               sliderInput("Year",inputId = "Year",
                           "Census Year:",
                           min = 2008,  max = 2018,  value = 2018,
                           step = 1,
                           pre = "Year ", sep = "",
                           animate = animationOptions(interval = 2200)
               ),
               box(sankeyNetworkOutput(outputId = "sankey_diagram"), width = 222)
      ),tabPanel("Summary by Region",
                       sliderInput("Year",inputId = "Year2",
                                   "Census Year:",
                                   min = 2008,  max = 2018,  value = 2018,
                                   step = 1,
                                   pre = "Year ", sep = "",
                                   animate = animationOptions(interval = 2200)
                       ),
                       selectInput(inputId = "Subregion", label = "Choose the Region:", choice = c("Antarctica","Australia and New Zealand","Caribbean","Central America","Central Asia","Eastern Africa","Eastern Asia","Eastern Europe","Melanesia","Micronesia","Middle Africa","Northern Africa","Northern America","Northern Europe","Polynesia","Seven seas (open ocean)","South America","South-Eastern Asia","Southern Africa","Southern Asia","Southern Europe","Western Africa","Western Asia","Western Europe"),selected=c("Western Africa")),
                       box(sankeyNetworkOutput(outputId = "sankey_diagram2"), width = 222)
      )
    )
  ),
)

server = function(input, output, session){
  observeEvent(input$Year, {
    withProgress(message = "Showing progress...", value = 0, {
      for (i in 1:10) {
        #incProgress(1/10)
        #Sys.sleep(0.25)
      }

      result <- get_data(input$Year)

      output$sankey_diagram = renderSankeyNetwork({

        t1 = data.frame(source = result$source,
                        target = result$target,
                        value = result$ICT)
        t2 = data.frame(id = c(0,1,2,3,4,5,6,7,8,9,10), name = c("Europe","Asia" ,"Americas","Africa","Oceania" ,"stable","fragile","FBS","FTS","MTS","PUI"))

        s = sankeyNetwork(Links = t1,
                          Nodes = t2,
                          Source = "source",
                          Target = "target",
                          Value = "value",
                          NodeID = "name",
                          NodeGroup = "name",
                          fontSize = 12,
                          nodeWidth = 30,
                          iterations = 0,
                          colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"))
        return(s)
      })
    })
  })

  observeEvent(c(input$Subregion,input$Year2),{
    withProgress(message = "Showing progress...", value = 0, {
      for (i in 1:10) {
        #incProgress(1/10)
        #Sys.sleep(0.25)
      }

      nameing <- get_names(input$Subregion,input$Year2)
      result <- get_data2(input$Subregion,input$Year2)


      output$sankey_diagram2 = renderSankeyNetwork({

        t1 = data.frame(source = result$target,
                        target = result$source,
                        value = result$value)

        #t2 = data.frame(id = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21), name = c("Benin", "Burkina Faso", "Cape Verde", "Gambia", "Ghana", "Liberia", "Mali", "Mauritania", "Niger", "Nigeria", "Senegal", "Togo", "fragile", "stable", "COHESION", "ECONOMIC", "FBS", "FTS", "MTS", "POLITICAL", "PUI", "SOCIAL"))
        t2 = data.frame(id = nameing$id, name = nameing$name)

        s = sankeyNetwork(Links = t1,
                          Nodes = t2,
                          Source = "source",
                          Target = "target",
                          Value = "value",
                          NodeID = "name",
                          NodeGroup = "name",
                          fontSize = 12,
                          nodeWidth = 30,
                          iterations = 0,
                          colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"))
        return(s)
      })
    })
  })
}

shinyApp(ui, server, enableBookmarking = "url")
