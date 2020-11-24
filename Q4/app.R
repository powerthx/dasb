library(shiny)
library(ggplot2)
library(tidyverse)

source("./global.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Forecast FSI/ICT Indicators"),
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Prediction Model", 
               selectInput(inputId = "economy", label = "Choose the economic region:", choices =  c("1. Developed region: G7","2. Developed region: nonG7","3. Emerging region: BRIC","4. Emerging region: MIKT","5. Emerging region: G20","6. Developing region","7. Least developed region")),
               selectInput(inputId = "grp", label = "Choose the grp standard:", choices = c("1. High income: OECD","2. High income: nonOECD","3. Upper middle income","4. Lower middle income","5. Low income")),
               selectInput(inputId = "region", label = "Choose the continent:", choice = c("Africa","Americas","Asia","Europe","Oceania")),
               selectInput(inputId = "STB", label = "Is the country stable?", choices = c("stable", "fragile")),
               selectInput(inputId = "Year", label = "Select a year", choices = c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")),
               textOutput("distPlot")
      ), 
      tabPanel("Summary",selectInput(inputId = "type", label = "Which summary would you like to see", choices = c(indicators)),
               plotOutput("summary")), 
      tabPanel("Compare Stability vs Connectivity", 
               #selectInput(inputId = "region2", label = "Choose the continent:", choice = c("Africa","Americas","Asia","Europe","Oceania")),
               selectInput(inputId = "STB2", label = "Is the country stable?", choices = c("stable", "fragile")),
               plotOutput("economy"))
    )
  ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderText({ 
    result <- get_data(input$economy, input$grp, input$region, input$STB, input$Year)
    paste("The average score for you input parameters are: FBS ", result[1], ", FTS ", result[2], ", MTS ", result[3], ", FSI ", result[4], ".")
  })
  
  output$summary<-renderPlot({
    if (input$type == "FBS"){
      ggplot(ds, aes(ds$region, ds$FBS,colour =ds$STB)) + 
        geom_boxplot() + 
        ggtitle("FBS scores by Stability Boxplot") + 
        xlab("Region") + 
        ylab("FBS scores")+
        labs(colour="FSI Index Stability")
    } else if (input$type == "FTS"){
      ggplot(ds, aes(ds$region, ds$FTS, colour=ds$STB)) + 
        geom_boxplot() + 
        ggtitle("FTS scores by Stability Boxplot") + 
        xlab("Region") + 
        ylab("FTS scores")+
        labs(colour="FSI Index Stability")
    } else if (input$type == "MTS"){
      ggplot(ds, aes(ds$region, ds$MTS,colour=ds$STB)) + 
        geom_boxplot() + 
        ggtitle("MTS scores by Stability Boxplot") + 
        xlab("Region") + 
        ylab("MTS scores") +
        labs(colour="FSI Index Stability")
    } else if (input$type == "ICT"){
      ggplot(ds, aes(ds$region, ds$ICT, colour=ds$STB)) + 
        geom_boxplot() + 
        ggtitle("ICT Index scores by Stability Boxplot") + 
        xlab("Region") + 
        ylab("ICT scores")+
        labs(colour="FSI Index Stability")
    } else if (input$type == "PUI"){
      ggplot(ds, aes(ds$region, ds$PUI, colour=ds$STB)) + 
        geom_boxplot() + 
        ggtitle("PUI scores by Stability Boxplot") + 
        xlab("Region") + 
        ylab("PUI scores")+
        labs(colour="FSI Index Stability")
    } else if (input$type == "FSI"){
      ggplot(ds, aes(ds$region, ds$FSI, colour=ds$STB)) + 
        geom_boxplot() + 
        ggtitle("FSI scores by Stability Boxplot (lower score is better)") + 
        xlab("Region") + 
        ylab("FSI scores")+
        labs(colour="FSI Index Stability")
    }
  })
  
  output$economy<-renderPlot({
    # filter for economy
    filterds <- filter(ds, STB == input$STB2)# region == input$region2 |)
    # add a combined score / ICT Index Score
    filterds$score <- filterds$ICT
    # plot the result
    ggplot(filterds, aes(filterds$region, filterds$score)) + 
      geom_boxplot() + 
      scale_y_continuous(limits = c(0, 0.35), breaks = seq(0, 0.35, by = 0.1))+ 
      ggtitle("ICT Index Scores by Stability") + 
      xlab("Continent") + 
      ylab("ICT Index Score")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
