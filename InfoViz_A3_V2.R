############################## FOR BACKUP ######################################

library(jsonlite)
library(httr)
library(DT)
library(shiny)
library(shinydashboard)
library(googleway)
library(sf)
library(leaflet)
library(ggplot2)
library(data.table)
library(reshape2)
library(tidyr)
library(dplyr)
library(shinyjs)
library(ggiraph)

source('tableau-in-shiny-v1.0.R')

gmaps_key = 'AIzaSyAjF1AUSDhoAQqvdEmeVK0yLM-mEutdkqw'

################
###On-Street####
################

#get geojson data from API
realtime = fromJSON("https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/on-street-parking-bay-sensors/exports/geojson?limit=5478&timezone=UTC&use_labels=false&epsg=4326")

rt_data = data.frame(realtime)
rt_data = head(rt_data,700)

sort_data = rt_data[order(rt_data$features.properties$lastupdated), ]

sort_data = sort_data %>% cbind(., sort_data$features.properties$lastupdated, sort_data$features.properties$status_description)

sort_data$carColor <- 'red'
sort_data$carColor[sort_data$`sort_data$features.properties$status_description` == 'Unoccupied'] <- 'green'
sort_data$`sort_data$features.properties$status_description`[sort_data$`sort_data$features.properties$status_description` == 'Present'] = 'Occupied'

coordinate = sort_data[, 3]$coordinates

Long = list()
Lat = list()
for (i in coordinate) {
  Long = append(Long,i[1])
  Lat = append(Lat,i[2])
}
coor_df = cbind(Long,Lat)
coor_df = as.data.frame(coor_df)

final_df = cbind(sort_data,coor_df)
long_list =final_df$Long 
long = as.numeric(unlist(long_list))
lat_list = final_df$Lat
lat = as.numeric(unlist(lat_list))


################
#####Events#####
################

event_data <- read.csv("free-and-cheap-support-services-with-opening-hours-public-transport-and-parking-.csv", stringsAsFactors = FALSE)
event_with_geo <- event_data[complete.cases(event_data[, "Longitude"]), ]
event_with_geo = melt(event_with_geo,measure.vars=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

event_with_geo$eventIcon[event_with_geo$Category.1 == 'Helpful phone number'] <- 'phone'
event_with_geo$eventIcon[event_with_geo$Category.1 == 'Legal / Financial Advice'] <- 'briefcase'
event_with_geo$eventIcon[event_with_geo$Category.1 == 'Drug and Alcohol'] <- 'glass'
event_with_geo$eventIcon[event_with_geo$Category.1 == 'Hospitals / Emergency'] <- 'plus-square'
event_with_geo$eventIcon[event_with_geo$Category.1 == 'Food'] <- 'coffee'
event_with_geo$eventIcon[event_with_geo$Category.1 == 'Counselling and Psychiatric Services'] <- 'comments'
event_with_geo$eventIcon[event_with_geo$Category.1 == 'Health Services / Pharmacy'] <- 'medkit'
event_with_geo$eventIcon[event_with_geo$Category.1 == 'Accommodation'] <- 'bed'
event_with_geo$eventIcon[event_with_geo$Category.1 == 'Clothes and Blankets'] <- 'shopping-bag'
event_with_geo$eventIcon[event_with_geo$Category.1 == 'Travel Assistance'] <- 'plane'
event_with_geo$eventIcon[event_with_geo$Category.1 == 'Tenancy Assistance'] <- 'handshake-o'
event_with_geo$eventIcon[event_with_geo$Category.1 == 'Needle Exchange'] <- 'stethoscope'
event_with_geo$eventIcon[event_with_geo$Category.1 == 'Employment Assistance'] <- 'laptop'
event_with_geo$eventIcon[event_with_geo$Category.1 == 'Showers / Laundry'] <- 'shower'

current_day <- weekdays(Sys.time())


################
##Bike-shared###
################

bike_data <- read.csv("bike-share-dock-locations.csv", stringsAsFactors = FALSE)

#split the address
for (i in 1:nrow(bike_data)) {
  split_data <- strsplit(bike_data$name[i], "-")[[1]]
  split_data_trimmed <- trimws(split_data)
  new_cols <- paste0("SplitLocation", 1:length(split_data_trimmed))
  bike_data[i, new_cols] <- split_data_trimmed
}


################
#####Trains#####
################

current_day <- weekdays(Sys.time())
train_data <- read.csv("metro-train-stations-with-accessibility-information.csv", stringsAsFactors = FALSE)
#dim(train_data) #219 6
#head(train_data)

train_data$Latitude = as.numeric(sapply(strsplit(as.character(train_data$Geo.Point),", "),"[[",1))
train_data$Longitude = as.numeric(sapply(strsplit(as.character(train_data$Geo.Point),", "),"[[",2))

train_data$trainColor <- 'red'
train_data$trainColor[train_data$lift == 'Yes'] <- 'green'

################
#####Airbnb#####
################
airbnb_data <- read.csv("listings.csv")

################
#####Entertainment#####
################
######################
# DATA PREPROCESSING #
######################

# read data from the csv file..
industry_data <- read.csv('business-establishments-with-address-and-industry-classification.csv')

tourism_industry_data <- industry_data[industry_data$Industry_ANZSIC4_description == "Cafes and Restaurants" |
                                         industry_data$Industry_ANZSIC4_description == "Pharmaceutical, Cosmetic and Toiletry Goods Retailing" |
                                         industry_data$Industry_ANZSIC4_description == "Casino Operation" |
                                         industry_data$Industry_ANZSIC4_description == "Clothing Retailing" |
                                         industry_data$Industry_ANZSIC4_description == "Clubs (Hospitality)",]

############################
# Symbols..Colors..Popup.. #
############################

#cafe
tourism_industry_data$IndustryColor <- 'beige'

tourism_industry_data$IndustryColor[tourism_industry_data$Industry_ANZSIC4_description == "Pharmaceutical, Cosmetic and Toiletry Goods Retailing"] <- 'lightblue'
tourism_industry_data$IndustryColor[tourism_industry_data$Industry_ANZSIC4_description == "Casino Operation" ] <- 'cadetblue'
tourism_industry_data$IndustryColor[tourism_industry_data$Industry_ANZSIC4_description == "Clothing Retailing"] <- 'blue'
tourism_industry_data$IndustryColor[tourism_industry_data$Industry_ANZSIC4_description == "Clubs (Hospitality)"] <- 'darkblue'

#cafe
tourism_industry_data$IndustryIcon <- 'coffee'

tourism_industry_data$IndustryIcon[tourism_industry_data$Industry_ANZSIC4_description == "Pharmaceutical, Cosmetic and Toiletry Goods Retailing"] <- 'female'
tourism_industry_data$IndustryIcon[tourism_industry_data$Industry_ANZSIC4_description == "Casino Operation" ] <- 'money'
tourism_industry_data$IndustryIcon[tourism_industry_data$Industry_ANZSIC4_description == "Clothing Retailing"] <- 'shopping-bag'
tourism_industry_data$IndustryIcon[tourism_industry_data$Industry_ANZSIC4_description == "Clubs (Hospitality)"] <- 'glass'


cbd_industry_data <- tourism_industry_data[tourism_industry_data$CLUE_small_area == "Melbourne (CBD)",]


################
###### UI ######
################

ui <- fluidPage(
  header=setUpTableauInShiny(),
  tags$head(
    tags$style(HTML("
      .skin-blue .main-sidebar {
        left: 0;
        right: auto;
        width: 230px;
      }
    "))
  ),
  dashboardPage(
    dashboardHeader(title = "Dashboard"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "Home", icon = icon("signal")),
        menuItem("Transportation", tabName = "map", icon = icon("map-pin"),
        menuSubItem("On-street Parking Map", tabName = "on-street"),
        menuSubItem("Services Map", tabName = "service"),
        menuSubItem("Bike Map", tabName = "bike"),
        menuSubItem("Train Map",tabName = "train")
        
        
      ),
        # entertainment menu..
        menuItem("Entertainment", tabName = "entertainment", icon = icon("baseball"),
                 
                 menuSubItem("Entertainment in Melbourne", tabName = "Tourism_Industries"),
                 menuSubItem("Entertainment in CBD", tabName = "CBD_Industries")
                 ),
        menuItem("TBD", tabName = "tbd", icon = icon("question")),

          fluidRow(
            column(12,
                   #HTML('<h3 style="color: blue; font-size: 20px;">This is a styled heading</h3>'),
                   #HTML('<p style="margin-left: 18px;">This is a paragraph with customized margin.</p>'),
                   HTML(paste('<p style="font-weight: bold;">  Current Date: ', current_day, '</p>'))
            )
          ),
        menuItem("Accommodation", tabName = "Accommodation", icon = icon("globe"))
        
    )
    ),
    
    dashboardBody(
      
        tabItems(
          tabItem(
            tabName = "Home",
              fluidRow(
                box(title = "Come and Say G’day", "Explore the beauty and wonders of Australia with our comprehensive tourism dashboard. Australia is a vast and 
                    diverse continent, known for its stunning landscapes, vibrant cities, unique wildlife, and rich cultural heritage. Whether you're planning a visit or simply curious 
                    about this remarkable destination, our dashboard provides you with key insights and information to make your Australian adventure unforgettable."),
                    HTML('<div style="text-align:center;">
                   <img src="https://i.gifer.com/origin/a5/a5ff90493cf44ea9a859a8106cf4a751_w200.webp" width="1000" height="500">
                </div>')
              )
          ),
          tabItem(
            tabName = "Accommodation",
              fluidRow(
                box(title = "Find Your Ideal Stay", "Stay updated with the latest Airbnb trends and insights for Australia. Our dashboard provides information on occupancy rates, average prices, and popular neighborhoods, allowing you to make informed decisions for your stay."),
                column(width = 5, selectInput("room_type", "Property Type", choices = unique(airbnb_data$room_type))
                ),
                column(width = 5, sliderInput("price_range", "Price Range", min = 1, max = 500, value = c(1, 500))
                ),
              ),      
            br(),
          splitLayout(
            leafletOutput("map", width = "140%", height = "450px"),
                tableauPublicViz(
                  id='tableauViz', 
                  url='https://public.tableau.com/views/Book1_16975116479590/Sheet5?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link',
                  height="300px"
                  )
              )
        
          ),
          
          # entertainment..
          
          tabItem("Tourism_Industries",
                  header=setUpTableauInShiny(),
                  sidebarLayout(
                    sidebarPanel(
                      
                      selectInput(
                        'industry',
                        label = 'Tourism Industry',
                        choices = c(sort(unique(tourism_industry_data$Industry_ANZSIC4_description))),
                        selected = 'Cafes and Restaurants'
                      ),
                      
                      selectInput(
                        'suburbs',
                        label = 'Suburbs',
                        choices = c(sort(unique(tourism_industry_data$CLUE_small_area))),
                        selected = 'Melbourne (CBD)'
                      ),
                    ), 
                  mainPanel(
                    
                    h2("Melbourne Entertainment"),
                    br(),
                    
                    
                    splitLayout(
                      cellWidths = c("50%","50%"),
                      style = "width: 900px; height: 600px",
                      
                      leafletOutput('map_tourism_industries',width="140%",height=570),
                      
                      tableauPublicViz(
                        id = 'tableauViz', 
                        url = 'https://public.tableau.com/views/shopping_16973342151260/Suburbs?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link',
                        height=700,
                        width="200%"
                      )
                    )
                    
                  )
                  )
          ),
          
          tabItem("CBD_Industries",
                  header=setUpTableauInShiny(),
                  mainPanel(
                    
                    h2("CBD Entertainment"),
                    br(),
                    
                    
                    splitLayout(
                      cellWidths = c("50%","50%"),
                      style = "width: 900px; height: 600px",
                      
                      leafletOutput('map_CBD_industries',width="140%",height=570),
                      
                      tableauPublicViz(
                        id = 'tableauViz_cbd', 
                        url = 'https://public.tableau.com/views/shopping_16973342151260/CBD?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link',
                        height=700,
                        width="200%"
                      )
                    )
                    
                  )
          ),
          
          tabItem(
            "map",
            box("This is the topie of transportation")
          ),
          tabItem("on-street",
                  mainPanel(
                    h2("On-street Parking in Melbourne (Real-time)"),
                    br(),
                    fluidRow(
                      column(
                        width = 5,
                        textAreaInput("searchTXT1","Search Content")
                      ),
                      column(
                        width = 3,
                        numericInput("R1","Search Radiu Range",value = 3000, min = 200, max = 5000)
                      ),
                      column(
                        width = 3,
                        numericInput("quantity1","Number of Results", value = 10, min = 1, max = 20)
                      )
                    ),
                  
                  fluidRow(
                    column(width = 2, actionButton("generatedataButton1","Generate Data"))
                  ),
                  br(),
                  leafletOutput('plot_carpark',width="140%",height=450)
                  )
                  ),
          tabItem("service",
                  mainPanel(
                    h2("Service Information in Melbourne"),
                    br(),
                    fluidRow(
                      column(
                        width = 5,
                        textAreaInput("searchTXT2","Search Content")
                      ),
                      column(
                        width = 3,
                        numericInput("R2","Search Radiu Range",value = 3000, min = 200, max = 5000)
                      ),
                      column(
                        width = 3,
                        numericInput("quantity2","Number of Results", value = 10, min = 1, max = 20)
                      )
                    ),
                    fluidRow(
                      column(
                        width = 4,
                        selectInput('Category','Category',choices = c('Helpful phone number','Legal / Financial Advice','Drug and Alcohol',
                                                                      'Hospitals / Emergency','Food','Counselling and Psychiatric Services',
                                                                      'Health Services / Pharmacy','Accommodation','Clothes and Blankets',
                                                                      'Travel Assistance','Tenancy Assistance','Needle Exchange',
                                                                      'Employment Assistance','Showers / Laundry'))
                      )
                    ),
                    
                    fluidRow(
                      column(width = 1, actionButton("generatedataButton","Generate Data"))
                    ),
                    br(),
                    leafletOutput('Event_place',width="140%",height=370),
                    tags$style(".leaflet-popup-content-wrapper { max-width: 300px; }")
                  )
          ),
          tabItem("bike",
                  header=setUpTableauInShiny(),
                  mainPanel(
                    header=setUpTableauInShiny(),
                    h2("Bike Share Dock Locations"),
                    br(),
                   
                   setUpTableauInShiny(),
                    splitLayout(
                      cellWidths = c("35%","65%"),
                      style = "width: 900px; height: 600px",
                      
                      leafletOutput('plot_bike',width="140%",height=570),
                      
                      tableauPublicViz(
                        id='tableauViz',       
                        url='https://public.tableau.com/views/mix-draft/bike-share-bar?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link',
                        height=700,
                        width="200%"
                      )
                    )
                    
                  )
          ),
          
          tabItem("train",
                  mainPanel(
                    h2("Metro Train Stations with Accessibility Information"),
                    br(),
                    leafletOutput('plot_train',width="140%",height=570)
                  )
          )
        )
      
    )
  )
)

server <- function(input, output, session) {
  output$plot_carpark <- renderLeaflet({
    leaflet(final_df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng=144.96849,lat=-37.8125,zoom=14)
  })
  observe({
    leafletProxy("plot_carpark") %>%
      clearMarkers() %>%
      addAwesomeMarkers(lng = long,lat = lat,
                        label=final_df$`sort_data$features.properties$status_description`,
                        icon=awesomeIcons(library='fa',
                                          icon='car',
                                          iconColor='#ffffff',
                                          markerColor=final_df$carColor),
                        clusterOptions = markerClusterOptions()
      )
  })
  
  google_result = function(search_text,lat,lng,radius,num_result) {
    search = google_places(search_string = search_text, location = c(lat, lng), radius = radius, key=gmaps_key)
    results = search$result
    complete_data = results %>% cbind(., results$geometry$location,results$opening_hours$open_now)
    
    open_hour = as.data.frame(complete_data$`results$opening_hours$open_now`)
    
    selected_df = complete_data[,c("name","rating","user_ratings_total","price_level")]
    selected_df = cbind(selected_df,open_hour)
    names(selected_df) = c('Name','Rating','Total User Ratings','Price Level','Open Hour')
    selected_df = head(selected_df,num_result)
    
    return(selected_df)
    
  }
  
  observeEvent(input$plot_carpark_marker_click, {
    click_info = input$plot_carpark_marker_click
    
    nearby_places = google_result(input$searchTXT1,click_info$lat,click_info$lng,input$R1,input$quantity1)
    
    
    if (!is.null(click_info)) {
      lng = click_info$lng
      lat = click_info$lat
      showModal(modalDialog(
        title = paste0("Here are the top ",input$quantity1," ",input$searchTXT1," information within a ", input$R1, " meter radius:"),
        DT::renderDataTable({
          DT::datatable(nearby_places, options = list(pageLength=5))
        })
        
      ))
    }
  })
  Popup <- function(row) {
    
    paste0(strong(row$Name), ' (', row$Suburb, ')', br(),
           strong(icon("subway"),'Tram Rountes: '), row$Tram.routes, br(), 
           strong(icon('usd'),'Cost: '), row$Cost, br(), 
           strong(icon('calendar'),'Operation Time of Today: '),row$value) 
  }
  event_with_geo$variable = as.character(event_with_geo$variable)
  today_data = event_with_geo[event_with_geo$variable == current_day,]
  today_data$Popup <- by(today_data, seq_len(nrow(today_data)), Popup)
  
  filter_cols = c("Name","Longitude","Latitude","Category.1",
                  "Suburb","Tram.routes","Cost"
                  ,"Popup","eventIcon"
                  )
  
  plot_info = today_data %>%
    select(all_of(filter_cols))
  names(plot_info) = c('Name',"Longitude","Latitude","Category","Suburb","Tram Routes","Cost"
                       ,"Popup","EventIcons"
                       )
  
  output$Event_place <- renderLeaflet({
    filter_data = plot_info[plot_info[, "Category"] == input$Category, ]
    
    leaflet(filter_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng=144.96849,lat=-37.8105,zoom=12) %>%
      #addMarkers(lng = filter_data$Longitude,lat = filter_data$Latitude,
                 
              #   label = lapply(1:nrow(filter_data),function(i) {
               #    HTML(Popup(filter_data[i,]))
                   
                # }),
                 #options = markerOptions(
                  # labelOptions = labelOptions(
                   #  noHide = FALSE,
                    # direction = "auto"
                   #)
                 #)
                 
      #)
    addAwesomeMarkers(lng = filter_data$Longitude,lat = filter_data$Latitude,
                      label=lapply(1:nrow(filter_data),function(i) {
                        HTML(Popup(filter_data[i,]))
                      }),
                      icon=~awesomeIcons(library='fa',
                                        icon=filter_data$EventIcons,
                                        iconColor='#ffffff'
                                        ),
                      
                      options = markerOptions(
                        labelOptions = labelOptions(
                          noHide = FALSE,
                          direction = "auto"
                        )
                      )
    )
  })
  
  observeEvent(input$Event_place_marker_click, {
    click_info = input$Event_place_marker_click
    
    nearby_places = google_result(input$searchTXT2,click_info$lat,click_info$lng,input$R2,input$quantity2)
    
    if (!is.null(click_info)) {
      lng = click_info$lng
      lat = click_info$lat
      showModal(modalDialog(
        title = paste0("Here are the top ",input$quantity2," ",input$searchTXT2," information within a ", input$R2, " meter radius:"),
        DT::renderDataTable({
          DT::datatable(nearby_places, options = list(pageLength=5))
        })
        
      ))
    }
  })
  
  filter_bike = c("name","capacity","lat","lon",
                  "SplitLocation1","SplitLocation2","SplitLocation3"
  )
  
  plot_bike = bike_data %>%
    select(all_of(filter_bike))
  names(plot_bike) = c('Full Address',"Capacity","Latitude","Longitude",
                       "Address Detail","Street Name","SuburbName"
  )
  
  
  output$plot_bike <- renderLeaflet({
    #bike_data = plot_bike[plot_bike[, "Suburb Name"] == input$area, ]
    bike_data = plot_bike
    
    suburb <- input$tableauViz_mark_selection_changed$Suburb
    
    if(is.null(suburb)) return()
    
    bike_data <- bike_data %>% 
      filter(SuburbName == suburb) 
    
    leaflet(bike_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      #setView(lng=144.96849,lat=-37.8145,zoom=14) %>%
      addAwesomeMarkers(lng = bike_data$Longitude,lat = bike_data$Latitude,
                        label=paste0('Capacity: ',bike_data$Capacity),
                        icon=awesomeIcons(library='fa',
                                          icon='bicycle',
                                          iconColor='#ffffff')
                        
      )
    
  })
  
  Popup1 <- function(row) {
    
    paste0(strong(icon('train'),"Station Name: "),row$station,br(),
           strong(icon('wheelchair'),'Lift: '), row$lift
    )
  }
  train_data$Popup <- by(train_data, seq_len(nrow(train_data)), Popup1)
  
  output$plot_train <- renderLeaflet({
    leaflet(train_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng=144.96749,lat=-37.8145,zoom=13) %>%
      addAwesomeMarkers(lng = train_data$Longitude,lat = train_data$Latitude,
                        label=lapply(1:nrow(train_data),function(i) {
                          HTML(Popup1(train_data[i,]))
                        }),
                        icon=awesomeIcons(library='fa',
                                          icon='train',
                                          iconColor='#ffffff',
                                          markerColor=train_data$trainColor)
      )
  })

  
  ###########
  # Entertainment and hospital
  #############
  
  # update the area selection based on the industry..
  observeEvent(input$industry, {
    
    updated_choices <- unique(tourism_industry_data$CLUE_small_area[tourism_industry_data$Industry_ANZSIC4_description == input$industry])
    
    # update selections..
    updateSelectInput(session, "suburbs", choices = updated_choices)
    
    # Filter Tableau viz based on the industry..
    industry <- input$industry
    runjs(sprintf('let viz = document.getElementById("tableauViz");
        let sheet = viz.workbook.activeSheet;
        sheet.applyFilterAsync("Tourism Industry", ["%s"], FilterUpdateType.Replace);', industry))
    
    
    
    
  })
  
  
  # update the cbd selection based on the industry..
  observeEvent(input$cbd_industry, {
    
    # Filter Tableau viz based on the industry..
    cbd_industry <- input$cbd_industry
    
    runjs(sprintf('let viz = document.getElementById("tableauViz_cbd");
        let sheet = viz.workbook.activeSheet;
        sheet.applyFilterAsync("CBD Industry", ["%s"], FilterUpdateType.Replace);', cbd_industry))
    
    
    
  })
  
  
  
  
  
  getFilteredTourismDataALL <- reactive({
    
    filter(tourism_industry_data, 
           Industry_ANZSIC4_description == input$industry,
           CLUE_small_area == input$suburbs)
    
    
    
    
  })
  
  
  
  
  
  # map output..
  output$map_tourism_industries <- renderLeaflet({
    
    
    leaflet(getFilteredTourismDataALL()) %>%
      addProviderTiles(providers$CartoDB) %>%
      addAwesomeMarkers(lng=~Longitude, lat=~Latitude,
                        # different Icons for different time with ~ symbol..
                        icon=~awesomeIcons(library='fa',
                                           icon= IndustryIcon,
                                           iconColor = "#ffffff",
                                           markerColor= IndustryColor),
                        label=~Business_address,
                        #popup=~Popup,
                        layerId=~Business_address)
    
  })
  
  
  # cbd map output..
  output$map_CBD_industries <- renderLeaflet({
    # Find the name of the hospital clicked by the user
    tableau_Block_ID <- input$tableauViz_cbd_mark_selection_changed$Block_ID[1]
    
    # If no hospital selected, stop
    if(is.null(tableau_Block_ID)) return()
    
    cbd_industry_data <- cbd_industry_data %>% 
      filter(Block_ID == tableau_Block_ID) 
    
    leaflet(cbd_industry_data) %>%
      addProviderTiles(providers$CartoDB) %>%
      addAwesomeMarkers(lng=~Longitude, lat=~Latitude,
                        # different Icons for different time with ~ symbol..
                        icon=~awesomeIcons(library='fa',
                                           icon= IndustryIcon,
                                           iconColor = "#ffffff",
                                           markerColor= IndustryColor),
                        label=~Business_address,
                        #popup=~Popup,
                        layerId=~Business_address)
    
  })
  

#####Airbnb Server#####

  
  filtered_data <- reactive({
    filtered <- airbnb_data %>%
      filter(room_type %in% input$room_type,
             neighbourhood == "Melbourne",  # Set neighborhood to "Melbourne CBD" statically
             price >= input$price_range[1],
             price <= input$price_range[2])
    return(filtered)
  })
  
  output$map <- renderLeaflet({
    m <- leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addMarkers(
        lat = ~latitude,
        lng = ~longitude,
        popup = ~popup_content(room_type, name, price)  # Define custom popup content
      )
    
    m
  })
  
  #custom function to format the popup content
  popup_content <- function(room_type, name, price) {
    content <- paste(
      "<b>Room Type:</b>", room_type,
      "<br><b>Name:</b>", name,
      "<br><b>Price:</b>", price
    )
    return(content)
  }


}

#############
# Run Shiny #
#############

shinyApp(ui, server, options = list(launch.browser=TRUE))









