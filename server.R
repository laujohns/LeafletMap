#Load appropriate packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgeos)
library(rgdal)
library(raster)
library(ggmap)
library(maptools)
library(shinyjs)

#load data from "data" subfolder
split_dentist<-readOGR("data", "split_dentist")
MVP<-readOGR("data","MVP")
split_dentist_gen<-readOGR("data","split_dentist_gen")
split_dentist_surg<-readOGR("data","split_dentist_surg")
split_dentist_orth<-readOGR("data","split_dentist_orth")
split_dentist_ped<-readOGR("data","split_dentist_ped")
split_dentist_oth<-readOGR("data","spl_dentist_oth")
options(shiny.trace = FALSE) #don't show java code

#transform shapefile of dentist addresses to dataframe
data2 <- data.frame(split_dentist)

#for % with college degree layer, round percentage to 2 decimal places
MVP@data$bach25<-round(MVP@data$bach25, 2)

#find quantiles for all three variables to be used in chloropleth layers
cuts<-round(quantile(MVP@data$revenue, probs=seq(0, 1, 0.20), na.rm=FALSE))
cuts
cuts2<-round(quantile(MVP@data$dispinc, probs=seq(0, 1, 0.20), na.rm=FALSE))
cuts2
cuts3<-round(quantile(MVP@data$bach25, probs=seq(0, 1, 0.20), na.rm=FALSE))
cuts3

#create color palettes based on for each of the three layers: revenue, household medium income, % with college degree
#the listed color codes are HTML hex colors - find at this website: http://htmlcolorcodes.com/
palette1 <- colorBin(c('#e8f8f5',
                       '#d1f2eb',
                       '#a3e4d7',
                       '#76d7c4',
                       '#48c9b0',
                       '#1abc9c',#revenue
                       '#17A589'), 
                     bins = c(0, 2024, 39581, 69164, 112298, 209602, 1069781)) #these cutoffs are based on defined quantiles from above

palette2 <- colorBin(c('#fdedec',  
                       '#fadbd8',
                       '#f5b7b1',
                       '#f1948a',
                       '#e74c3c',
                       '#CB4335'), 
                     bins = c(0, 9167 , 24945, 33595 , 41470, 54851 , 125612)) 

palette3 <- colorBin(c('#d2b4de',  
                       '#bb8fce',
                       '#a569bd',
                       '#7d3c98',
                       '#6c3483',
                       '#5B2C6F'), 
                     bins = c(0,0.06060606 , 0.12006319, 0.19211823, 0.31142857, 0.68745067))


#create popups for each block-group level by layer
#when someone clicks on the block-group level show the following statistics: 
popup1 <- paste0("<span style='color: #7f0000'><strong>2015 ACS Data</strong></span>",
                 "<br>Estimated Total Revenue:",
                 paste0(" ","$",format(MVP@data$revenue, scientific=FALSE, big.mark=",")), #show revenue of block group (add $ sign and comma separator for #)
                 "<br>Total Population: ", 
                 format(MVP@data$totpop, scientific=FALSE, big.mark=",")) #show total population (add comma separator to #)
popup2 <- paste0("<span style='color: #7f0000'><strong>2015 ACS Data</strong></span>",
                 "<br>Median Household Disposable Income:",
                 paste0(" ","$",format(MVP@data$dispinc, scientific=FALSE, big.mark=",")), #show median disposable income (add $ sign and comma separator to #)
                 "<br>Total Population: ", 
                 format(MVP@data$totpop, scientific=FALSE, big.mark=",")) #show total population (add comma separator to #)
popup3 <- paste0("<span style='color: #7f0000'><strong>2015 ACS Data</strong></span>",
                 "<br>At least a bachelor's degree:",
                 paste0(" ",MVP@data$bach25*100,"%"), #show % population with colleg education (add % sign)
                 "<br>Total Population: ", 
                 format(MVP@data$totpop, scientific=FALSE, big.mark=",")) #show total population (add comma separator to #)

#add red icon for geocoded address that user inputs
leaficon<-icons (
  iconUrl="http://icons.iconarchive.com/icons/paomedia/small-n-flat/64/map-marker-icon.png",
  iconWidth = 55, iconHeight = 55)

function(input, output, session) {
  #render base leaflet map and setview is at the lat and long of Birmingham, AL
  #set max and min zoom so user cannot zoom outside of Birmingham, AL
  output$map2<- renderLeaflet({leaflet() %>% addTiles(urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",options = tileOptions(minZoom=9, maxZoom=16))%>%
      setView(-86.8008, 33.4718, zoom=10)})
  
  #the following code adds markers for different dental specialities based on user choice from drop-down menu
  observeEvent(input$specialty, {
    map2<-leafletProxy("map2") %>% clearGroup(c("A","B","C","D","E")) #set default so the map resets in between choice
    
    if (input$specialty=="General Dentist") {
      leafletProxy("map2") %>%addMarkers(data=split_dentist_gen, 
                                         popup = ~paste0(as.character(split_dentist_gen$ADDRESS),", ",as.character(split_dentist_gen$CITY),", ","AL, ",split_dentist_gen$ZIP_CODE), group="A") #label groups so can clear after selecting another specialty
    } #adds popup for each marker so the user can click on the map markers and see exactly where each dentist/specialty is located --> adds address, city, zip code separated by commas
    else if (input$specialty=="Oral & Maxillofacial Surgery") {
      leafletProxy("map2") %>%addMarkers(data=split_dentist_surg,
                                         popup = ~paste0(as.character(split_dentist_surg$ADDRESS),", ",as.character(split_dentist_surg$CITY),", ","AL, ",split_dentist_surg$ZIP_CODE), group="B")
    }
    else if (input$specialty=="Orthodontics") {
      leafletProxy("map2") %>%addMarkers(data=split_dentist_orth,
                                         popup = ~paste0(as.character(split_dentist_orth$ADDRESS),", ",as.character(split_dentist_orth$CITY),", ","AL, ",split_dentist_orth$ZIP_CODE), group="C")
      }
    else if (input$specialty=="Pediatrics") {
      leafletProxy("map2") %>%addMarkers(data=split_dentist_ped,
                                         popup = ~paste0(as.character(split_dentist_ped$ADDRESS),", ",as.character(split_dentist_ped$CITY),", ","AL, ",split_dentist_ped$ZIP_CODE), group="D")
    }         

    else if (input$specialty=="Other Specialty") {
      leafletProxy("map2") %>%addMarkers(data=split_dentist_oth,
                                         popup = ~paste0(as.character(split_dentist_oth$ADDRESS),", ",as.character(split_dentist_oth$CITY),", ","AL, ",split_dentist_oth$ZIP_CODE), group="E")
      }
    
  }
  )
  
  #the following code adds a new legend and chloropleth layer based on user choice from radiobuttons --> chooses from revenue, median income, % with college education
  observeEvent(input$variable,{
    
    map2 <- leafletProxy("map2") %>% clearControls() %>% clearGroup(c("F","G","H")) #set default map so will reset in between each choice
    
    if (input$variable == 'Estimated Revenue for New Office')
    {map2 <- map2 %>% 
      addPolygons(data = MVP, 
                  fillColor = ~palette1(MVP@data$revenue),  ## we want the polygon filled with one of the palette-colors
                  fillOpacity = 0.6,         ## set transparency of polygon
                  color = "#darkgrey",       ## color the borders of block groups
                  weight = 1,            ## width of borders
                  popup = popup1,
                  group="F") %>% #name this group so can clear if user chooses another layer
      addLegend(position = 'topleft', ## choose bottomleft, bottomright, topleft or topright
                colors = c('#e8f8f5',  #these collors should match the same as those used in the palettes above
                           '#d1f2eb',
                           '#a3e4d7',
                           '#76d7c4',
                           '#48c9b0',
                           '#1abc9c',
                           '#17A589'), 
                labels = c('$0',"","","","","",'$1,069,781'), #add labels to legend (min and max only shown)
                opacity = 0.6,      ##transparency again
                title = "Total Estimated <br> Revenue")} 
    else if (input$variable == 'Population Median Disposable Income')
    {map2 <- map2 %>%  addPolygons(data = MVP, 
                                   fillColor = ~palette2(MVP@data$dispinc),
                                   fillOpacity = 0.6,       
                                   color = "#darkgrey",     
                                   weight = 1,          
                                   popup = popup2, 
                                   group= "G") %>%
      addLegend(position = 'topleft', 
                colors = c('#fdedec',  
                           '#fadbd8',
                           '#f5b7b1',
                           '#f1948a',
                           '#ec7063',
                           '#e74c3c',
                           '#CB4335'), 
                labels = c('$0',"","","","","",'$125,612'),  
                opacity = 0.6,      
                title = "Median Household <br> Disposable Income")}
    else if (input$variable == 'Population with College Degree')
    {map2 <- map2 %>%  addPolygons(data = MVP, 
                                   fillColor = ~palette3(MVP@data$bach25),  
                                   fillOpacity = 0.6,       
                                   color = "#darkgrey",      
                                   weight = 1,          
                                   popup = popup3,
                                   group="H") %>%
      addLegend(position = 'topleft',
                colors = c('#d2b4de',  
                           '#bb8fce',
                           '#a569bd',
                           '#7d3c98',
                           '#6c3483',
                           '#5B2C6F'),
                labels = c('0%',"","","","",'69%'),  
                opacity = 0.6,      
                title = "Percent with <br> College Degree")}
    else if (input$variable == 'None'){
      map2
    }
  }
  )
  
  #the following geocodes an input address and adds a circle around the address (based on specified "buffer zone") to see competitors in the area
  observeEvent (input$geocode, {
    
    
    address <- input$address
    add <- paste(address, "Birmingham", "Alabama", sep=" ,") #address is free text but city and state are set at Birmingham, AL
    
      v <- geocode(add, output="latlon") #geocode address
      
   if (!is.na(v$lon) & !is.na(v$lat)) {
      leafletProxy('map2') %>% setView(lng=v$lon, lat=v$lat, zoom=12) %>% addMarkers(data=v, icon=leaficon, layerId="B") %>% #add marker for geocoded address
        addCircles(lng=v$lon,lat=v$lat, fillOpacity = 0.3, color = "blue", radius = (input$radius)*1609, layerId="A") #draw a circle around marker based on user's choice (slide bar in UI)
        #the radius is in meters by default so need to convert to miles
      }
      else if (is.na(v$lon) & is.na(v$lat))
      {
        leafletProxy('map2') %>% setView(-86.8008, 33.4718, zoom=10) %>% removeShape("A") %>% removeMarker("B")
        updateTextInput(session, "address", value = " ")    #if there is no address, just show the default map and remove previous geocoded markers and circle
      }

    })

  #this code clears geocoded marker and circle once the user pushes "clear" 
  observeEvent (input$clear, {
    leafletProxy('map2') %>% removeShape("A") %>%setView(-86.8008, 33.4718, zoom=10) %>% removeMarker("B")
    updateTextInput(session, "address", value = " ")   
  })
}
