# Tidyverse
library(tidyverse)  # data handling and plotting
library(rvest)      # scrape data


# Interactive elements
library(leaflet)    # interactive maps

library(rvest)
library(htmltools)

#team logos
#install.packages("teamcolors")
library(teamcolors)

library(shiny)
library(shinythemes)
library(rsconnect)


## Scrape Data from Wiki table


nba_scrape <-
  read_html("https://en.wikipedia.org/wiki/National_Basketball_Association") %>% 
  html_nodes(xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[4]") %>%
  html_table(fill = TRUE, header = NA) %>%
  .[[1]]  # list was returned, so extract first list element

#glimpse(nba_scrape)  # a data frame



## Wrangle up the data



nba_wrangle <- nba_scrape %>% 
  select(-length(.)) %>%  # remove the last column (NA)
  dplyr::filter(!str_detect(Division, "Conference")) %>% 
  mutate(
    Conference = c(rep("Eastern", 15), rep("Western", 15)),
    Capacity = as.numeric(str_remove(Capacity, ","))
  ) %>% 
  separate(`Location`, c("City", "State"), sep = ", ") %>% 
  separate(Coordinates, c("Coords1", "Coords2", "Coords3"), " / ") %>% 
  separate(Coords3, c("Latitude", "Longitude"), sep = "; ") %>% 
  separate(Longitude, c("Longitude", "X"), sep = " \\(") %>% 
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(
      str_remove(Longitude, "\\ufeff")  # remove rogue unicode
    )  
  ) %>% 
  select(
    Team, Conference, everything(),
    -Founded, -Joined, -Coords1, -Coords2, -X
  ) %>% 
  as_tibble()  # convert to tibble






## Merge abreveation/ color for teams


nba_abbr_cols <- 
  read_html(
    "https://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Basketball_Association/National_Basketball_Association_team_abbreviations"
  ) %>% 
  html_nodes(xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table") %>%
  html_table(header = TRUE) %>%
  .[[1]] %>% 
  rename(Code = `Abbreviation/Acronym`) %>% 
  mutate(
    # {leaflet} markers take a named colour
    colour_marker = case_when(
      Code == "ATL" ~ "red",
      Code == "BKN" ~ "black",
      Code == "BOS" ~ "green",
      Code == "CHA" ~ "darkblue",
      Code == "CHI" ~ "red",
      Code == "CLE" ~ "darkred",
      Code == "DAL" ~ "blue",
      Code == "DEN" ~ "darkblue",
      Code == "DET" ~ "red",
      Code == "GSW" ~ "blue",
      Code == "HOU" ~ "red",
      Code == "IND" ~ "darkblue",
      Code == "LAC" ~ "red",
      Code == "LAL" ~ "blue",
      Code == "MEM" ~ "lightblue",
      Code == "MIA" ~ "red",
      Code == "MIL" ~ "darkgreen",
      Code == "MIN" ~ "darkblue",
      Code == "NOP" ~ "darkblue",
      Code == "NYK" ~ "blue",
      Code == "OKC" ~ "blue",
      Code == "ORL" ~ "blue",
      Code == "PHI" ~ "blue",
      Code == "PHX" ~ "darkblue",
      Code == "POR" ~ "red",
      Code == "SAC" ~ "purple",
      Code == "SAS" ~ "black",
      Code == "TOR" ~ "red",
      Code == "UTA" ~ "darkblue",
      Code == "WAS" ~ "darkblue"
    ),
    # {leaflet} marker icons take hex
    colour_icon = case_when(
      Code == "ATL" ~ "#C1D32F",
      Code == "BKN" ~ "#FFFFFF",
      Code == "BOS" ~ "#BA9653",
      Code == "CHA" ~ "#00788C",
      Code == "CHI" ~ "#000000",
      Code == "CLE" ~ "#FDBB30",
      Code == "DAL" ~ "#B8C4CA",
      Code == "DEN" ~ "#FEC524",
      Code == "DET" ~ "#1D42BA",
      Code == "GSW" ~ "#FFC72C",
      Code == "HOU" ~ "#000000",
      Code == "IND" ~ "#FDBB30",
      Code == "LAC" ~ "#1D428A",
      Code == "LAL" ~ "#FDB927",
      Code == "MEM" ~ "#12173F",
      Code == "MIA" ~ "#F9A01B",
      Code == "MIL" ~ "#EEE1C6",
      Code == "MIN" ~ "#9EA2A2",
      Code == "NOP" ~ "#C8102E",
      Code == "NYK" ~ "#F58426",
      Code == "OKC" ~ "#EF3B24",
      Code == "ORL" ~ "#C4CED4",
      Code == "PHI" ~ "#ED174C",
      Code == "PHX" ~ "#E56020",
      Code == "POR" ~ "#000000",
      Code == "SAC" ~ "#63727A",
      Code == "SAS" ~ "#C4CED4",
      Code == "TOR" ~ "#000000",
      Code == "UTA" ~ "#F9A01B",
      Code == "WAS" ~ "#E31837"
    )
  ) %>% 
  as_tibble()

#sample_n(nba_abbr_cols, 5)




## combine tables


nba_table <- nba_wrangle %>% 
  left_join(nba_abbr_cols, by = c("Team" = "Franchise")) %>%
  select(Code, everything())

#glimpse(nba_table)

logos <- teamcolors%>%
  filter(league == "nba")%>%
  select(name,logo)


nba_logos <- merge(nba_table,logos, by.x = "Team" , by.y = "name" )




# Oh the combinations of data tables



fullnba<-read_csv('Seasons_Stats.csv')
#remove all * from the Player column
fullnba$Player<- gsub("[*]", "", fullnba$Player)


fullnba <- fullnba%>%
  mutate(id = as.numeric(factor(Tm)))




Team_table <- nba_logos%>%
  mutate(id = row_number())%>%
  relocate(.,logo,.before = Team)

comnba <-  merge(fullnba,Team_table, by.x = "id" , by.y = "id" )%>%
  select(-Code,- ...1)%>%
  relocate(., Year,Team, Player, Age, Pos,.after = id)



ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("NBA stats 1984-2017"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("team_logo"),
      selectInput("team_input", "Select team", choices = unique(comnba$Team)),
      selectInput("year_input", "Select year", choices = rev(sort(unique(comnba$Year)))),
      downloadButton("download_data", "Save data")
    ),
    mainPanel(
      leafletOutput("map")
    )
  ),
  dataTableOutput("table")
)

server <- function(input, output, session) {
  
  output$team_logo <- renderUI({
    tags$div(
      style = "display: flex; justify-content: center;",
      tags$img(src = nba_logos[nba_logos$Team == input$team_input, "logo"], width = "300px", height = "200px")
    ) })
  
  output$map <- renderLeaflet({
    leaflet(nba_logos) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      addAwesomeMarkers(
        lng = ~Longitude, lat = ~Latitude, label = ~htmlEscape(Team),
        popup = ~paste0(
          "<b>", nba_logos$Team, "</b>",
          "<br>", paste0(nba_logos$Arena, ", ", nba_logos$City),
          if_else(
            nba_logos$Conference == "Eastern",
            paste0("<br><font color='#0000FF'>", nba_logos$Division,
                   " Division (Eastern Conference)</font>"),
            paste0("<br><font color='#FF0000'>", nba_logos$Division,
                   " Division (Western Conference)</font>")
          )
        ),
        icon = awesomeIcons(
          library = "ion", icon = "ion-ios-basketball",
          markerColor = nba_logos$colour_marker,
          iconColor = nba_logos$colour_icon
        )
      )
  })
  
  filtered_data <- reactive({
    comnba %>% filter(Team == input$team_input, Year == input$year_input)
  })
  
  
  output$table <- renderDataTable({
    filtered_data()
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$team_input, "_nba_stats.csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$team_input, {
    leafletProxy("map") %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        data = nba_logos[nba_logos$Team == input$team_input, ],
        lng = ~Longitude, lat = ~Latitude, label = ~htmlEscape(Team),
        popup = ~paste0(
          "<b>", nba_logos$Team, "</b>",
          "<br>", paste0(nba_logos$Arena, ", ", nba_logos$City),
          if_else(
            nba_logos$Conference == "Eastern",
            paste0("<br><font color='#0000FF'>", nba_logos$Division,
                   " Division (Eastern Conference)</font>"),
            paste0("<br><font color='#FF0000'>", nba_logos$Division,
                   " Division (Western Conference)</font>")
          )
        ),
        icon = awesomeIcons(
          library = "ion", icon = "ion-ios-basketball",
          markerColor = nba_logos$colour_marker,
          iconColor = nba_logos$colour_icon
        )
      )
    
    output$team_logo <- renderUI({
      tags$img(src = nba_logos[nba_logos$Team == input$team_input, "logo"], width = "70px", height = "50px")
    })
    
  })
}

shinyApp(ui, server)
