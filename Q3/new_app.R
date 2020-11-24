library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

# Paths to the Datasets
dataset_itu_index_path <- "/home/tarnold/R/1PrjFINAL/transformed/dataset_itu_index_values.csv"
dataset_fsi_index_path <- "/home/tarnold/R/1PrjFINAL/transformed/dataset_fsi_index.csv"
metadata_countries_path <- "/home/tarnold/R/1PrjFINAL/data/metadata_countries.csv"

# The list of almost valid indicators
indicators <<- list("Fixed-broadband subscriptions" = "IN13FBS",
                    "Fixed-telephone subscriptions" = "IN14FTS",
                    "Mobile-cellular telephone subscriptions" = "IN15MTS"
                    #"Percentage of Individuals using the Internet" = "IN16PUI"                                        
                    #"Active mobile-broadband subscriptions" = "AMB",
                    #"Individuals using the Internet" = "IUI"
)

# Load data
dataset_fsi_index <- read_csv(dataset_fsi_index_path,
                              col_types = cols(X1 = col_skip(),`C1: Security Apparatus` = col_double(),
                                               `C2: Factionalized Elites` = col_double(),
                                               `C3: Group Grievance` = col_double(),
                                               `E1: Economy` = col_double(), `E2: Economic Inequality` = col_double(),
                                               `E3: Human Flight and Brain Drain` = col_double(),
                                               ISO2 = col_character(), ISO3 = col_character(),
                                               `P1: State Legitimacy` = col_double(),
                                               `P2: Public Services` = col_double(),
                                               `P3: Human Rights` = col_double(),
                                               Rank = col_double(), `S1: Demographic Pressures` = col_double(),
                                               `S2: Refugees and IDPs` = col_double(),
                                               Total = col_double(), UN = col_double(),
                                               `X1: External Intervention` = col_double(),
                                               Year = col_date(format = "%Y")
                              ))

names(dataset_fsi_index) <- c("Country",
                              "Year",
                              "Rank",
                              "IN0_Total",
                              "IN1_C1:Security Apparatus" ,
                              "IN2_C2:Factionalized Elites" ,
                              "IN3_C3:Group Grievance" ,
                              "IN4_E1:Economic Decline" ,
                              "IN5_E2:Uneven Economic Development" ,
                              "IN6_E3:Human Flight and Brain Drain" ,
                              "IN7_P1:State Legitimacy" ,
                              "IN8_P2:Public Services" ,
                              "IN9_P3:Human Rights and Rule of Law" ,
                              "IN10_S1:Demographic Pressures" ,
                              "IN11_S2:Refugees and IDPs" ,
                              "IN12_X1:External Intervention",
                              "ISO2",
                              "ISO3","UN")

## Read itu data source
dataset_itu_index <- read_csv(dataset_itu_index_path, col_types = cols("X1" =col_skip(), 
                                                                       "Country" = col_character(), 
                                                                       "Year" = col_date(format = "%Y"), 
                                                                       "ISO2" = col_character(), 
                                                                       "ISO3" = col_character(), #FOCUS COUNTRIES "AFG","ALB","DZA","AGO","ATG","ARG","ARM","AUS","AUT","AZE","BHS","BHR","BGD","BRB","BLR","BEL","BLZ","BEN","BTN","BOL","BIH","BWA","BRA","BRN","BGR","BFA","BDI","KHM","CMR","CAN","CPV","CAF","TCD","CHL","CHN","COL","COM","COD","COG","CRI","CIV","HRV","CUB","CYP","CZE","PRK","DNK","DJI","DOM","ECU","EGY","SLV","GNQ","ERI","EST","ETH","FJI","FIN","FRA","GAB","GMB","GEO","DEU","GHA","GRC","GRD","GTM","GIN","GNB","GUY","HTI","HND","HUN","ISL","IND","IDN","IRN","IRQ","IRL","ISR","ITA","JAM","JPN","JOR","KAZ","KEN","KOR","KWT","KGZ","LAO","LVA","LBN","LSO","LBR","LBY","LTU","LUX","MDG","MWI","MYS","MDV","MLI","MLT","MRT","MUS","MEX","FSM","MDA","MNG","MNE","MAR","MOZ","MMR","NAM","NPL","NLD","NZL","NIC","NER","NGA","NOR","OMN","PAK","PAN","PNG","PRY","PER","PHL","POL","PRT","QAT","ROU","RUS","RWA","WSM","STP","SAU","SEN","SRB","SYC","SLE","SGP","SVK","SVN","SLB","SOM","ZAF","SSD","ESP","LKA","SDN","SUR","SWZ","SWE","CHE","SYR","TJK","TZA","MKD","THA","TLS","TGO","TTO","TUN","TUR","TKM","UGA","UKR","ARE","GBR","USA","URY","UZB","VEN","VNM","YEM","ZMB","ZWE",
                                                                       "UN" = col_character(), 
                                                                       "FBS" = col_double(), 
                                                                       "FTS" = col_double(), 
                                                                       "PUI" = col_double(), 
                                                                       "MTS" = col_double(), 
                                                                       "AMB" = col_skip(), 
                                                                       "IUI" = col_skip()
))
names(dataset_itu_index) <- c("Country", "Year", "ISO2", "ISO3","UN", "IN13FBS", "IN14FTS", "IN15PUI", "IN16MTS")
dataset_itu_index = subset(dataset_itu_index, select = -c(`Country`,`UN`,`ISO2`) )

dataset_fsi_index <- inner_join(dataset_itu_index, dataset_fsi_index,by = c("ISO3" = "ISO3", "Year" = "Year"))
ds <- inner_join(dataset_itu_index, dataset_fsi_index,by = c("ISO3" = "ISO3", "Year" = "Year"),keep = FALSE)


ds <- dataset_fsi_index %>%
  pivot_longer(
    cols = starts_with("IN"),
    names_to = "ID",
    names_prefix = "wk",
    values_to = "Score",
    values_drop_na = TRUE
  )

trend_data <- ds %>%
  dplyr::select("ID","Year","Score","Country") %>%
  #filter(Country == "Sudan")  %>%
  arrange(desc(Year) )
names(trend_data) <- c("type", "date", "close","Country")

# Load data
trend_data <- trend_data
trend_description <- read_csv("data/trend_description.csv")

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                titlePanel("FSI Index"),
                sidebarLayout(
                  sidebarPanel(
                    selectizeInput(
                      inputId = "Country", label = strong("Countries"), choices = c("Afghanistan","Albania","Algeria","Angola","Antigua and Barbuda","Argentina","Armenia","Australia","Austria","Azerbaijan","Bahamas","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Belize","Benin","Bhutan","Bolivia","Bosnia and Herzegovina","Botswana","Brazil","Brunei Darussalam","Bulgaria","Burkina Faso","Burundi","Cambodia","Cameroon","Canada","Cape Verde","Central African Republic","Chad","Chile","China","Colombia","Comoros","Congo Democratic Republic","Congo Republic","Costa Rica","Cote d'Ivoire","Croatia","Cuba","Cyprus","Czech Republic","Denmark","Djibouti","Dominican Republic","Ecuador","Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Fiji","Finland","France","Gabon","Gambia","Georgia","Germany","Ghana","Greece","Grenada","Guatemala","Guinea","Guinea Bissau","Guyana","Haiti","Honduras","Hungary","Iceland","India","Indonesia","Iran","Iraq","Ireland","Israel and West Bank","Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Kuwait","Kyrgyz Republic","Laos","Latvia","Lebanon","Lesotho","Liberia","Libya","Lithuania","Luxembourg","Macedonia","Madagascar","Malawi","Malaysia","Maldives","Mali","Malta","Mauritania","Mauritius","Mexico","Micronesia","Moldova","Mongolia","Montenegro","Morocco","Mozambique","Myanmar","Namibia","Nepal","Netherlands","New Zealand","Nicaragua","Niger","Nigeria","North Korea","Norway","Oman","Pakistan","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Poland","Portugal","Qatar","Romania","Russia","Rwanda","Samoa","Sao Tome and Principe","Saudi Arabia","Senegal","Serbia","Seychelles","Sierra Leone","Singapore","Slovak Republic","Slovenia","Solomon Islands","Somalia","South Africa","South Korea","South Sudan","Spain","Sri Lanka","Sudan","Suriname","Swaziland","Sweden","Switzerland","Syria","Tajikistan","Tanzania","Thailand","Timor-Leste","Togo","Trinidad and Tobago","Tunisia","Turkey","Turkmenistan","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States","Uruguay","Uzbekistan","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe"),selected = "Sudan", multiple = TRUE
                    ),
                    # Select variable type of trend to plot
                    selectInput(inputId = "type", label = strong("FSI Indicators"),
                                choices = c("FSI Total Score"="IN0_Total",
                                                   "C1: Security Apparatus" = "IN1_C1:Security Apparatus",
                                                   "C2: Factionalized Elites" = "IN2_C2:Factionalized Elites",
                                                   "C3: Group Grievance" = "IN3_C3:Group Grievance",
                                                   "E1: Economic Decline" = "IN4_E1:Economic Decline",
                                                   "E2: Uneven Economic Development" = "IN5_E2:Uneven Economic Development",
                                                   "E3: Human Flight and Brain Drain" = "IN6_E3:Human Flight and Brain Drain",
                                                   "P1: State Legitimacy" = "IN7_P1:State Legitimacy",
                                                   "P2: Public Services" = "IN8_P2:Public Services",
                                                   "P3: Human Rights and Rule of Law" = "IN9_P3:Human Rights and Rule of Law",
                                                   "S1: Demographic Pressures" = "IN10_S1:Demographic Pressures",
                                                   "S2: Refugees and IDPs" = "IN11_S2:Refugees and IDPs",
                                                   "X1: External Intervention" ="IN12_X1:External Intervention"),
                                selected = "IN0_Total"),

                    selectInput(inputId = "type2",strong("ICT Indicators"),
                                choices = indicators),  
                    
                    # Select date range to be plotted
                    dateRangeInput(inputId ="date", strong("Date range"), start = "2008-01-01", end = "2018-01-31",
                                   min = "2008-01-01", max = "2018-01-31"),

                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),

                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Smoother span:",
                                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Higher values give more smoothness.")
                    )
                  ),
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px"),
                    HTML("<br>"),
                    textOutput(outputId = "desc"),
                    HTML("<br>"),
                    tags$a(href = "https://fragilestatesindex.org/", "Source: Fagile States Index - The Fund for Peace", target = "_blank")
                  )
                )
)

# Define server function
server <- function(input, output) {

  # Subset data
  selected_trends <- reactive({
    req(input$date)
    req(input$Country)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    trend_data %>%
      filter(
        type == input$type,
        Country == input$Country,
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
        ))
  })

  selected_trends2 <- reactive({
    req(input$date)
    req(input$Country)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    trend_data %>%
      filter(
        type == input$type2,
        Country == input$Country,
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
        ))
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    #par(mar = c(4, 4, 1, 1))
    par(mar = c(5, 5, 3, 5))
    plot(x = selected_trends()$date, y = selected_trends()$close, type = "l",
         xlab = "Year", ylab = "FSI Index", col = "black", fg = color, col.lab = color, col.axis = color#, ylim=c(0,130)
         )
    par(new = TRUE)
    plot(x = selected_trends()$date,y = selected_trends2()$close, type = "l", xaxt = "n", yaxt = "n",
         ylab = "", xlab = "", col = "red", lty = 2)
    axis(side = 4)
    mtext("ICT Indicator", side = 4, line = 3)
    legend("bottom", c("FSI", "ICT"),
           col = c("black", "red"), lty = c(1, 2))
    #lines(x = selected_trends()$date,z = selected_trends2()$close, type = "l",col="green")
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$close, f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
}

  })

  # Pull in description of trend
  output$desc <- renderText({
    trend_text <- filter(trend_description, type == input$type) %>% pull(text)
    paste(trend_text, " Thanks for reading this far.")
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
