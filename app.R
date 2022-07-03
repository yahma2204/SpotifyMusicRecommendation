library(dplyr)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(readr)

url <- "https://raw.githubusercontent.com/mhoangvslev/SpotifyTracksDB/master/dataset/SpotifyFeatures.csv" 
spotify <-read_csv(url(url))
spotify <- as.data.frame(spotify)

spotify <- spotify %>% 
  mutate(artist_name = as.character(artist_name),
         track_name = as.character(track_name),
         track_id = as.character(track_id),
         duration_ms = duration_ms/60000) %>% 
  rename(duration_min = duration_ms)

# Remove duplicated data 
spotify_clean <- spotify[!duplicated(spotify$track_id),]

# Assign track_id into rownames
rownames(spotify_clean) <- spotify_clean$track_id

# Filter only numeric variables
spotify_clean <- spotify_clean %>% 
  select(where(is.numeric))

# scaling
spotify_scale <- scale(spotify_clean)

# Model Fitting
RNGkind(sample.kind = "Rounding")
set.seed(100)
spotify_kmeans <- kmeans(x = spotify_scale, centers = 3)
spotify_clean$cluster <- spotify_kmeans$cluster

# Remove Row Names
spotify_clean$track_id <- rownames(spotify_clean)
rownames(spotify_clean) <- NULL

# Combine dataset
spotify_track <- spotify %>% 
  select(track_id, track_name, artist_name, genre) %>% 
  left_join(spotify_clean, by = "track_id") %>% 
  arrange(-popularity)



ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Recommendation"),
  dashboardSidebar(
    # Sidebar with a slider input for number of bins
    sidebarMenu(
      fluidRow(
        column(width = 12,
            selectizeInput(
              inputId = "Artist",
              label = "Pilih penyanyi yang Anda sukai: ",
              choices = unique(spotify_track$artist_name),
              options = list(
                placeholder = 'Type to search for Artist',
                onInitialize = I('function() { this.setValue(""); }'))
              )
            )),
      fluidRow(
        column(width = 12, 
               selectizeInput(
                 inputId = "Song",
                 label = "Pilih lagu yang Anda sukai: ",
                 choices = NULL,
                 options = list(
                   placeholder = 'Type to search for Track',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
               )
               )
      )
  )),
  dashboardBody(
    fluidRow(
      box(width = 12,
      column(
        width = 12,
        selectizeInput(
          inputId = "recom",
          label = "Pilih penyanyi yang ingin Anda eksplor: ",
          choices = unique(spotify_track$artist_name), 
          multiple = T,
          selected = sample_n(tbl = data.frame(unique(spotify_track$artist_name)),size = 3)))
          ,
      column( 
        width = 12,
        selectizeInput(
          inputId = "genre",
          label = "Pilih genre yang ingin Anda eksplor: ",
          choices = NULL, 
          multiple = T
          ))
      
      ))
  ,
    # Show a plot of the generated distribution
  fluidRow(
    box(title = "Rekomendasi lagu :",
        width = 7,
        background = "green",
        column(offset = 0,
             width = 7),
        DT::dataTableOutput(outputId = "recommend")),
    box(width = 5,background = "black",
        column(offset =0.4, width = 5,
    tags$img(src = "acastro_180213_1777_0001.0.jpg", height = "280px", width = "350px")))
    )
))

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  observe({
    spotify_artist <- spotify_track %>% 
      filter(artist_name == input$Artist) %>% 
      select(artist_name,track_name,cluster, genre) 
    updateSelectizeInput(session, inputId = "Song",
                         label = "Pilih lagu yang Anda sukai:" , 
                         choices = unique(spotify_artist$track_name))
  })
  
  observe({
    spotify_artist <- spotify_track %>% 
      filter(artist_name == input$Artist) %>% 
      select(artist_name,track_name,cluster, genre)
    
    spotify_song <- spotify_artist %>%
      filter(track_name == input$Song) %>% 
      head(1)
    
    spot_recom <- spotify_track %>% 
      filter(artist_name %in% input$recom)
    updateSelectizeInput(session, inputId = "genre", choices = unique(spot_recom$genre), 
                         selected = unique(spot_recom$genre))
  })
  
  
  output$recommend <- DT::renderDataTable({
    
      spotify_artist <- spotify_track %>% 
        filter(artist_name == input$Artist) %>% 
        select(artist_name,track_name,cluster, genre)
        
      spotify_song <- spotify_artist %>%
        filter(track_name == input$Song) %>% 
        head(1)
        
      spot_recom <- spotify_track %>% 
        filter(artist_name %in% input$recom) 
      
      spot_recom %>% 
        filter(genre %in% input$genre) %>% 
        slice_sample(n = 6) %>% 
        select(-track_id) %>% 
        arrange(-popularity) %>% 
        select(track_name, artist_name, genre, popularity) %>% 
        rename(Song = track_name,
               Artist = artist_name,
               Genre = genre) 
      
  },options = list(lengthMenu = c(3,6), pageLength = 3, scrollX = TRUE))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
