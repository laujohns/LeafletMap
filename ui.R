#load the necessary packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgeos)
library(rgdal)
library(ggmap)
library(shinyjs)

#create the header of the Shiny Dashboard
header <- dashboardHeader(
  title=strong("Dentalography", style="font-family: 'Tahoma'; font-size: 30px;", img(src="pin.png",height=40)), #add title with a map icon
  titleWidth = 330)

#create sidebard of Shiny Dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",menuItem(strong("Interactive Map"), #strong indicates bolded
                                    tabName="Map", icon = icon("map-marker")), #create a tab for "map" and an icon
                                        menuItem(strong("Google Slides Presentation"), tabName="Presentation", icon = icon("laptop")) #create a tab for Google slides embedded presentation and icon
                                        )
)

body<-dashboardBody(
  tabItems( tabItem("Map", 
  tags$style(type = "text/css", #specify font type and size for the dashboard body for the map tab
             "label { font-size: 16px; }", #all labels are 16 pixel font
             ".selectize-input { font-size: 16px;} ",   #the titles of the text inputs and drop-down menus are 16 pixels
             ".selectize-dropdown { font-size: 16px; }",
             "#city { font-size: 16px; }", #increase the size of the city and address labels on the text input
             "#address { font-size: 16px; }",
             "#state { font-size: 16px; }"
  ),

  #create columns
 fluidRow(
   column(width = 8, HTML("<div style='height: 600px;'>"), #set height of the box the map is in so that it lines up with the right-hand boxes
          p("Please wait to click until map loads.", style="color:grey; font-size:10pt"), #add a notification above the map 
         box(width = NULL, status="success", solidHeader = TRUE, title="Map of Birmingham, AL", #create a green box that has the map
               leafletOutput("map2", height=940)), #show map in this box
         HTML("</div>") #add some space at the bottom of the map
         ),
 
  column(width=4,style='padding:0px;', #add padding so that the title of this box lines up with the map
         h3(strong("Explore Map Features")), #add bolded title to this column
         p("Click on the map and markers for additional statistics", style="font-size:12pt"), 
         
  box(width=NULL, status="warning", #add red box that includes a drop-down menus for specialty (changes markers on map)
             selectInput("specialty", h4(strong("Select a Speciality:")), c("General Dentist","Oral & Maxillofacial Surgery","Orthodontics",
                                                                "Pediatrics", "Other Specialty"), selected="General Dentist")), #the default selected specialty is "General Dentist"
  box(width=NULL, status="warning", #create a red box that includes radiobuttons for which the user selects map cholorpleth layer to display
             radioButtons(inputId="variable", label=h4(strong("Select a Feature:")), 
                          choices=c("Estimated Revenue for New Office","Population Median Disposable Income","Population with College Degree", "None"), 
                          selected = "Estimated Revenue for New Office")), #default selected is Est. Revenue

  h3(strong("Map a Specific Location")), #add a title for the next box

  box(width=NULL, status = "primary", 
       textInput("address",h4(strong("Street address"))), #the user inputs address
      p(em("Example: 937 48th St N", style="color:red; font-size:12pt")), #add a notification that is italicized indicating an example address in Birmingham, AL
       textInput("city",h4(strong("City")), "Birmingham"), 
       textInput("state",h4(strong("State")), "AL"), 
       sliderInput("radius",h4(strong("Select a buffer distance (miles):")), 1, 10, 5)))), #add a sliderinput in which the user can choose a buffer distance in miles
      
fluidRow(
  column(width=2,offset = 8,
         box(width=NULL, status="info", actionButton("geocode", h4(strong("Map location"))))), #add a button that the user pushes to geocode inputted address and draws a circle
  column(width=2, offest=10,
         box (width=NULL, status="danger", actionButton("clear", h4(strong("Clear location"))))) #add a button to clear the geocoded address and circle

)
),

tabItem("Presentation", #add a tab for Google Slides embedded presentation
        
        column (width=12,
 HTML('<iframe src="https://docs.google.com/presentation/d/1CUe7v5m_cGUyAk0NnR2d92-wk6enc1ck_Q1JoAXbT-E/embed?start=false&loop=false&delayms=60000" frameborder="0" width="1000" height="600" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true"></iframe>')
 )
)
)
)
dashboardPage(header, #put entired dashboard together
  sidebar,
  body)

