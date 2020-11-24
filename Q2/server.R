library(leaflet)
library(shiny)
library(leaflet.providers)
library(rgdal)
library("rnaturalearth")
library("rnaturalearthdata")
library(spDataLarge)   # loads the world dataset
library(data.table)
library(rgdal)
library(lattice)

function(input, output, session) {
    #Output TMAP
    output$map <- renderLeaflet({
      worldmap <- map_data_react()  # Add this
      
      # Color and Legende Settings    
      bins <- c(0, 0.5, 5, 10, 20, 50, 100, 200, Inf)
      pal <- colorBin("YlOrRd", domain = worldmap$Value)#, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g Subscriptions per 100 inhabitants",
        worldmap$admin, worldmap$Value
      ) %>% lapply(htmltools::HTML)
      
      worldmap %>% leaflet() %>%
        setView(lng = 15.85, lat = 54.45, zoom = 4) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
          id = "mapbox.light",
          accessToken = Sys.getenv('pk.eyJ1Ijoib3B3ZXJ0aHgiLCJhIjoiY2psa3Vua2tsMHFwcTN3bzVxejRvdmpyeCJ9.nJ4jre_Zq6xxlp-vJI0nEQ'))) %>%
        addPolygons(
          fillColor = ~pal(Value),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "1",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
        addTiles(
               attribution = 'PROJECT GROUP NINE - DASB'
             ) %>%
        #addProviderTiles(providers$Stamen.Toner) %>%
        addLegend(pal = pal, values = ~Value, opacity = 0.7, title = NULL,
                  position = "bottomright")
    })

    output$scatterCollegeIncome <- renderPlot({
      print(xyplot( `2018_Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions` ~ `2018_Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions`, data = worldmap, ylim = range(worldmap$`2018_Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions`), xlim = range(worldmap$`2018_Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions`)))
    })
    
    
    
    map_data_react <- reactive({
      #Dynamic Allocation of Cols does not work, no clue why Shiny is not able to handle this - wasted a lot of time, why?
      colnames(worldmap) <- c("scalerank","featurecla","labelrank","sovereignt","sov_a3","adm0_dif","level","type","admin","adm0_a3","geou_dif","geounit","gu_a3","su_dif","subunit","su_a3","brk_diff","name","name_long","brk_a3","brk_name","brk_group","abbrev","postal","formal_en","formal_fr","note_adm0","note_brk","name_sort","name_alt","mapcolor7","mapcolor8","mapcolor9","mapcolor13","pop_est","gdp_md_est","pop_year","lastcensus","gdp_year","economy","income_grp","wikipedia","fips_10","iso_a2","iso_a3","iso_n3","un_a3","wb_a2","wb_a3","woe_id","adm0_a3_is","adm0_a3_us","adm0_a3_un","adm0_a3_wb","continent","region_un","subregion","region_wb","name_len","long_len","abbrev_len","tiny","homepart","Country","ISO2","UN","2018_Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions","2017_Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions","2016_Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions","2015_Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions","2010_Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions","2012_Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions","2013_Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions","2014_Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions","2009_Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions","2008_Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions","2018_Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions","2017_Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions","2016_Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions","2015_Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions","2014_Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions","2013_Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions","2012_Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions","2010_Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions","2011_Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions","2009_Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions","2017_Percentage_%_Individuals using the Internet","2016_Percentage_%_Individuals using the Internet","2015_Percentage_%_Individuals using the Internet","2014_Percentage_%_Individuals using the Internet","2013_Percentage_%_Individuals using the Internet","2012_Percentage_%_Individuals using the Internet","2011_Percentage_%_Individuals using the Internet","2010_Percentage_%_Individuals using the Internet","2009_Percentage_%_Individuals using the Internet","2008_Percentage_%_Individuals using the Internet","2017_Per 100 inhabitants_Per 100 inhabitants_Mobile-cellular telephone subscriptions","2016_Per 100 inhabitants_Per 100 inhabitants_Mobile-cellular telephone subscriptions","2018_Per 100 inhabitants_Per 100 inhabitants_Mobile-cellular telephone subscriptions","2015_Per 100 inhabitants_Per 100 inhabitants_Mobile-cellular telephone subscriptions","2014_Per 100 inhabitants_Per 100 inhabitants_Mobile-cellular telephone subscriptions","2013_Per 100 inhabitants_Per 100 inhabitants_Mobile-cellular telephone subscriptions","2012_Per 100 inhabitants_Per 100 inhabitants_Mobile-cellular telephone subscriptions","2011_Per 100 inhabitants_Per 100 inhabitants_Mobile-cellular telephone subscriptions","2009_Per 100 inhabitants_Per 100 inhabitants_Mobile-cellular telephone subscriptions","2010_Per 100 inhabitants_Per 100 inhabitants_Mobile-cellular telephone subscriptions","2008_Per 100 inhabitants_Per 100 inhabitants_Mobile-cellular telephone subscriptions","2018_Per 100 inhabitants_Per 100 inhabitants_Active mobile-broadband subscriptions","2017_Per 100 inhabitants_Per 100 inhabitants_Active mobile-broadband subscriptions","2016_Per 100 inhabitants_Per 100 inhabitants_Active mobile-broadband subscriptions","2015_Per 100 inhabitants_Per 100 inhabitants_Active mobile-broadband subscriptions","2014_Per 100 inhabitants_Per 100 inhabitants_Active mobile-broadband subscriptions","2013_Per 100 inhabitants_Per 100 inhabitants_Active mobile-broadband subscriptions","2012_Per 100 inhabitants_Per 100 inhabitants_Active mobile-broadband subscriptions","2011_Per 100 inhabitants_Per 100 inhabitants_Active mobile-broadband subscriptions","2010_Per 100 inhabitants_Per 100 inhabitants_Active mobile-broadband subscriptions","2011_Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions","2008_Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions","2018_Per 100 inhabitants_Per 100 inhabitants_Individuals using the Internet","2017_Per 100 inhabitants_Per 100 inhabitants_Individuals using the Internet","2018_Percentage_%_Individuals using the Internet","2016_Per 100 inhabitants_Per 100 inhabitants_Individuals using the Internet","2015_Per 100 inhabitants_Per 100 inhabitants_Individuals using the Internet","2014_Per 100 inhabitants_Per 100 inhabitants_Individuals using the Internet","2013_Per 100 inhabitants_Per 100 inhabitants_Individuals using the Internet","2012_Per 100 inhabitants_Per 100 inhabitants_Individuals using the Internet","2011_Per 100 inhabitants_Per 100 inhabitants_Individuals using the Internet","2010_Per 100 inhabitants_Per 100 inhabitants_Individuals using the Internet","2009_Per 100 inhabitants_Per 100 inhabitants_Individuals using the Internet","2008_Per 100 inhabitants_Per 100 inhabitants_Individuals using the Internet","2009_Per 100 inhabitants_Per 100 inhabitants_Active mobile-broadband subscriptions","2008_Per 100 inhabitants_Per 100 inhabitants_Active mobile-broadband subscriptions","geometry")
      selected <- paste(input$Year,"_Per 100 inhabitants_Per 100 inhabitants_",input$Indicator, sep = "")
      worldmap <- setnames(worldmap,selected,"Value",skip_absent=TRUE)
})
# observe({
#   Indicator <- input$Indicator
#   })
}

