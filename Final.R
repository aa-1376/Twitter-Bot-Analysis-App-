# Libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinyjs)
library(dplyr)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(ggplot2)
library(shiny)
library(hms)
library(DT)
library(stringr)
library(tools)
library(readr)
library(tidyverse)



# Load and clean  data ----------------------------------------------
tweet <- read_csv("Partisan_Tweets2.csv")
names(tweet) <- str_to_title(names(tweet))
tweet$Candidate<-str_to_title(tweet$Candidate)
tweets_data <- mutate(tweet, Created_at = as.Date(Created_at, format= "%m/%d/%Y"))
tweets_data <- tweets_data %>% rename(Rtweet = `Retweet_no`)
tweets_data <- tweets_data %>% rename(BotP = `Bot_probability`)
tweets_data <- tweets_data %>% rename(Poso = `Pos_sent`)
tweets_data <- tweets_data %>% rename(Nego = `Neg_sent`)
tweets_data <- tweets_data %>% rename(latitude = `Lat`)
tweets_data <- tweets_data %>% rename(longitude = `Long`)

# County data and merging files
urlfile="https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv"
counties<-read_csv(url(urlfile))
counties <- counties %>% rename(GEOID = `county_fips`)
lines.load <- st_read("./cb_2018_us_county_500k/cb_2018_us_county_500k.shp")

co <- lines.load %>%
  left_join(counties, by = c("GEOID" = "GEOID"))
co <- co %>% mutate (party = case_when(per_gop < per_dem ~ "Biden", per_gop > per_dem ~ "Trump"))


icons <- awesomeIconList(
  MS4 = makeAwesomeIcon(icon = "road", library = "fa", markerColor = "gray"),
  Combined = makeAwesomeIcon(icon = "cloud", library = "fa", markerColor = "blue"),
  `Non-combined` = makeAwesomeIcon(icon = "tint", library = "fa", markerColor = "green"),
  `On-site management` = makeAwesomeIcon(icon = "building-o", library = "fa", markerColor = "cadetblue")
)

ui <- fluidPage(
  theme = shinythemes::shinytheme("yeti"),
  titlePanel(title=div(img(height = 105, width = 300, src="picture.png") , "Partisan Bot Tweet Locator "), windowTitle = "myBrowserTitle"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "selected_type",
                   label = "Select Candidate:",
                   choices = c("Biden", "Trump" ),
                   selected = "Biden"),
      
      
      # Set start date ---------------------------------------------
      sliderInput(inputId = "startdate",
                  label = "Slect Tweet Date Range:", 
                  min = as.Date("2020-10-13"), max = as.Date("2020-10-20"), 
                  value = c(as.Date("2020-10-13"), as.Date("2020-10-17"))),
      # Select Manufacturer for Y-axis ----------------------------------
      sliderInput(inputId = "bot_prob",
                  label = "Bot Probability of the Tweeter:", 
                  min = 0, max = 1, 
                  value = c(0, 1)),
      # Add Download Button
      downloadButton("downloadData", "Download"),
      h6("Press the download button to save the dataset you are looking at."),
      
      #Add blank row
      hr(),
      # Reference map description
      h6("Reference Map: Counties by their partisan association"),
      h6("Red = Republican | Blue = Democrat"),
      
      
      # Map Output
      leafletOutput("leaflet2"),
      
    ),
    mainPanel(
      tabsetPanel(tabPanel("Map", shinyjs::useShinyjs(),
                           # Style the background and change the page
                           tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: white}"),
                           # Map Output
                           leafletOutput("leaflet"),
                           
                           
                           # Number of tweets
                           textOutput("text")),
                  tabPanel("Tweets",
                           fluidPage(
                             wellPanel(DT::dataTableOutput("table"))
                           )),
                  tabPanel("plots", h4("Distribution by Date."), plotOutput(outputId = "plot_dist"), hr(), h4("Positive Negative Distribution."), plotOutput(outputId = "plot_NegPos"), hr(), h4("Distribution by sentiment."), plotOutput(outputId = "plot_senti"))
                  
      )

    )

  )
  
)
# Define server logic required to create a map
server <- function(input, output) {
  # Basic Map1
  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
      setView(-74.0060, 40.7128, 3) %>%
      addLayersControl(baseGroups = c("Google", "Wiki"))
  })
  # Basic Map 2
  output$leaflet2 <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
      setView(-95.7129, 37.0902, 3) %>%
      addLayersControl(baseGroups = c("Google", "Wiki"))
  })
  
  # Tweet Filtered data
  TweetInfInputs <- reactive({
    tweetInf <-  tweets_data %>%
      
      req(input$selected_type) # ensure availablity of value before proceeding
    req(input$startdate)
    req(input$bot_prob)
    filter(tweetInf, Candidate %in% input$selected_type & Created_at >= input$startdate[1] & Created_at <= input$startdate[2] & BotP >= input$bot_prob[1] & BotP <= input$bot_prob[2])
    

  })
  

  
  # Replace layer with filtered partisan data
  observe({
    TweetInf <- TweetInfInputs()

    leafletProxy("leaflet", data = TweetInf) %>%
      # In this case either lines 92 or 93 will work
      # clearMarkers() %>%
      clearGroup(group = "TweetInf") %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(icon = ~icons[Candidate], clusterOptions = markerClusterOptions(), popup = ~paste0("<b>", "</b>: ", Candidate), group = "TweetInf", layerId = ~...1)
  })

  # Filter for county partisan data
  
  county_input <- reactive({
    counttt <- subset(co, party == input$selected_type)
    return(counttt)
    
    
  })
  #Dataset for Republican counties
  trump_counties <- subset(co, party == "Trump")
  #Dataset for Democrat counties
  biden_counties <- subset(co, party == "Biden")
  
  #Plot partisan county map
  observe({
    par_count <- county_input()
    # Map2 with Republican counties
    leafletProxy("leaflet2", data = trump_counties) %>%

      clearGroup(group = "par_count") %>%
      addPolygons(popup = ~paste0("<b>", county_name, "</b>"), group = "county", layerId = ~GEOID, fill = FALSE, color = "red") #%>%
    # setView(lng = boros$longitude, lat = boros$latitude, zoom = 9)
  })
  
  # Map2 with democrat counties
  observe({
    par_count <- county_input()
    # Data is par_count
    leafletProxy("leaflet2", data = biden_counties) %>%
      
      clearGroup(group = "par_count") %>%
      addPolygons(popup = ~paste0("<b>", county_name, "</b>"), group = "county", layerId = ~GEOID, fill = FALSE, color = "blue") #%>%
    # setView(lng = boros$longitude, lat = boros$latitude, zoom = 9)
  })
  
  #Output Table
  output$table <- DT::renderDataTable({
    subset(TweetInfInputs(),select = c(Tweet, Hashtags, Candidate, Rtweet, BotP))
  })
  # Subset to data Only on screen
  onScreen <- reactive({
    req(input$leaflet_bounds)
    bounds <- input$leaflet_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(TweetInfInputs(), latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  # Print Tweets in viw
  output$text <- renderText({
    paste("You are viewing", nrow(onScreen()), "tweets")
  })
  
  # Create barchart object the plotOutput function is expecting --
  output$plot_dist <- renderPlot({
    ggplot(data = TweetInfInputs(), aes(x = Created_at)) +
      geom_bar(color = 4, fill = "4") +
      ggtitle("Number of Partisan Tweets") +
      xlab("Date") +
      ylab("Tweet Count")+
      theme_classic()+
      theme(axis.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.y = element_text(face = "bold"))
    
    
  })
  
  # Create pie chart object the plotOutput function is expecting --
  output$plot_NegPos <- renderPlot({
    pie_data <- TweetInfInputs() %>%
      count(Sentiment) %>% 
      mutate(percent = n/sum(n)) 
    ggplot(data = pie_data, aes(x ="", y = percent,  fill = Sentiment)) +
      geom_bar(position = "fill", width = 1, stat = "identity", color = "white") +
      geom_text(aes(x = 1.0, label = scales::percent(percent, accuracy = .1)), position = position_stack(vjust = .5)) +
      coord_polar(theta = "y")+
      theme_void()
    
    
    
    
  })
  
  # Create scatterplot object the plotOutput function is expecting --
  output$plot_senti <- renderPlot({
    ggplot(data = TweetInfInputs(), aes_string(x = "Poso", y = "Nego")) +
      theme_classic()+
      theme(axis.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.y = element_text(face = "bold"))+
      geom_point(color = 4) 
  })
  
  #Download button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-Sentiment", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(TweetInfInputs(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
co
