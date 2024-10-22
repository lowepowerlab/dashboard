# load packages
library(tidyverse)
library(htmlwidgets)
library(shiny)
library(bslib)
library(ggplot2)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs) 
library(patchwork)
library(cowplot)
library(ggthemes)
library(fresh)
library(plotly)
library(gsheet)
library(googlesheets4)


# create theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "hotpink"
  ),
  adminlte_sidebar(
    width = "300px",
    dark_color = "black",
    dark_bg = "hotpink",
    dark_hover_bg = "black",
    dark_submenu_color = "black"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)


# load data and transformations
RSSC <- read_csv("RSSC_Practice.csv")

Phylotype_selected = c("I","II", "III","IV")
PandemicLineage_selected = c("1", "2")

RSSC1 = RSSC %>% 
  mutate(Phylotype2 = Phylotype) %>%
  mutate(Phylotype2 = case_when(!is.na(Phylotype2) ~ Phylotype2,
                                is.na(Phylotype2) ~ "Unknown")) %>% 
  
  mutate(Phylotype = case_when( Phylotype %in% Phylotype_selected  ~Phylotype,
                                is.na(Phylotype) ~ "Unknown",
                                !is.na(Phylotype) & !Phylotype %in% Phylotype_selected ~ "Others")) %>%
  
            #mutate(Sequevar2 = Sequevar) %>%
            #mutate(Sequevar2 = case_when(!is.na(Sequevar2) ~ Sequevar2,
            #                             is.na(Sequevar2) ~ "Unknown")) %>%
  
  unite("latlong", Latitude, Longitude, sep ="/", remove = F ) %>% 
  group_by(latlong) %>% 
  mutate(n = n(),
         Latitude = case_when(n > 1 ~ rnorm(n, Latitude,0.01),
                              n == 1 ~ Latitude),
         Longitude = case_when(n > 1 ~ rnorm(n, Longitude,0.01),
                               n == 1 ~ Longitude)
  ) %>%
  
  mutate(`Host Species (Common name)` = case_when(!is.na(`Host Species (Common name)`) ~ `Host Species (Common name)`,
                                                  is.na(`Host Species (Common name)`) ~ "Unknown")) %>%
  group_by(Phylotype) %>% 
  mutate(n_iso_per_Phylotype = n(),
         Phylotype3 = paste(Phylotype," (",n_iso_per_Phylotype,")", sep = ""),) %>% 
  ungroup() %>% 
  dplyr::select(-n, -latlong)


# ui
ui = dashboardPage(skin = "black",
        dashboardHeader(title = "RSSC Db", titleWidth = 250),
        dashboardSidebar(collapsed = F, width = 250,
        br(),
        div(style = "display:inline-block;width:80%;margin-left:18px;text-align: left;",
        "A georeferenced database of isolates of the", em("Ralstonia solanacearum"), "Species Complex.
        Use the filters below to refine your search, visualize data, and download metadata."),
          sidebarMenu(id = "sidebarid",
           # filter drop-down menus
            pickerInput(inputId = "publication_year",
                        label = "Publication Year",
                        choices = unique(RSSC1$`Year published`),
                        options = list(`actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = unique(RSSC1$`Year published`),
                        multiple = T
            ),
            pickerInput(inputId = "isolation_year",
                        label = "Isolation Year",
                        choices = unique(RSSC1$`Year isolated`),
                        options = list(`actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = unique(RSSC1$`Year isolated`),
                        multiple = T
            ),
            pickerInput(inputId = "phylo",
                        label = "Phylotype",
                        choices = unique(RSSC1$Phylotype3),
                        options = list(`actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = unique(RSSC1$Phylotype3),
                        multiple = T
            ),  
            pickerInput(inputId = "host_family",
                        label = "Host Family",
                        choices = unique(RSSC1$`Host Family`),
                        options = list(`actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = unique(RSSC1$`Host Family`),
                        multiple = T
            ),
            pickerInput(inputId = "host_species",
                        label = "Host Species",
                        choices = unique(RSSC1$`Host Species (Common name)`),
                        options = list(`actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = unique(RSSC1$`Host Species (Common name)`),
                        multiple = T
            ), 
                                                 
              #selectizeInput(inputId = "VegProp_Host",
              #         label = "Vegetatively Propagated Hosts)",
              #         list("Vegetatively Propagated Hosts" = 
              #               list("Potato" = "RSSC$`Host Species`~RSSC$`Solanum tuberosum`", 
              #                    "Banana/Plantain = "Musa", 
              #                    "Ginger/Turmeric" = "Zingiber", 
              #                    "Geranium" = "G",
              #                    "Pothos" = "p",
              #                    "Anthurium" = "a",
              #                    "Rose" = "r")),
              #               multiple = T
              #      ), 
                                                 
            pickerInput(inputId = "continent",
                        label = "Continent",
                        choices = unique(RSSC1$`Location (continent)`),
                        options = list(`actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = unique(RSSC1$`Location (continent)`),
                        multiple = T
            ),
            pickerInput(inputId = "country",
                        label = "Country or Territory",
                        choices = unique(RSSC1$`Location (Country or Territory)`),
                        options = list(`actions-box` = T,
                        size = 10,
                       `selected-text-format` = "count > 1"
                        ),
                        selected = unique(RSSC1$`Location (Country or Territory)`),
                        multiple = T
            ),
                                                 
           div(style="display:inline-block;width:25%;text-align: center;",
               actionButton(inputId = "search",
               label = "Filter",
               icon =icon("filter"))),
           div(style="display:inline-block;width:25%;text-align: center;",
               actionButton(inputId = "reset",
               label = "Select All",
               icon =icon("retweet"))),
           div(style="display:inline-block;width:60%;text-align: center;",
               downloadButton("download", "Get Data")))
            ),
                    
        dashboardBody(use_theme(mytheme),
                      shinyjs::useShinyjs(),
                      div(id = "my app",
                    #row  
                      fluidRow(
                        valueBoxOutput("n_Isolates", width = 3),
                        infoBoxOutput("n_Citations", width = 3),
                        infoBoxOutput("n_Distribution", width = 3),
                        infoBoxOutput("n_Hosts", width = 3)
                        ),
                    #row
                      fluidRow(
                        box(title = "Isolate Map",
                            solidHeader = T,
                            width = 12,
                            collapsible = T,
                            leafletOutput("map_phylo",
                            width = "100%",
                            height = 500)
                            )
                      ),
                    # row
                      fluidRow(
                        box(title = "Host Distribution",
                            solidHeader = T,
                            width = 6, 
                            collapsible = T,
                            collapsed = F,
                            #plotOutput("Host_chart")
                            ),
                        box(title = "Phylotype Distribution",
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            collapsed = F,
                            #plotOutput("Phylotype_chart")
                            )
                      ),
                    # row
                      fluidRow(
                        box(title = "Metadata Table",
                            solidHeader = T,
                            width = 12, 
                            collapsible = T,
                            collapsed = F,
                            #div(DT::dataTableOutput("Metadata_table"), style = "font-size: 70%;"))
                            )
                      )
                    )
                  )
)

# server

server = function(input, output, session) {
  
  observeEvent(input$reset, {
    shinyjs::reset("publication_year")
    shinyjs::reset("isolation_year")
    shinyjs::reset("phylo")
    shinyjs::reset("host_family")
    shinyjs::reset("host_species")
    shinyjs::reset("continent")
    shinyjs::reset("country")
  })
  
  
  filtered_PublicationYear_type <- eventReactive(input$search,{
    RSSC1 
    
  })
  
  filtered_IsolationYear_type <- eventReactive(input$search,{
    filtered_PublicationYear_type() %>%
      filter(`Year isolated` %in% input$isolation_year)
  })
  
  filtered_Phylotype_type <- eventReactive(input$search,{
    filtered_IsolationYear_type() %>%
      filter(Phylotype3 %in% input$phylo)
  })
  
  filtered_HostFamily_type <- eventReactive(input$search,{
    filtered_Phylotype_type() %>%
      filter(`Host Family` %in% input$host_family)
  })
  
  filtered_HostSpecies_type <- eventReactive(input$search,{
    filtered_HostFamily_type() %>%
      filter(`Host Species (Common name)` %in% input$host_species)
  })
  
  filtered_Continent_type <- eventReactive(input$search,{
    filtered_HostSpecies_type() %>%
      filter(`Location (continent)` %in% input$continent)
  })
  
  filtered_Country_type <- eventReactive(input$search,{
    filtered_Continent_type() %>%
      filter(`Location (Country or Territory)` %in% input$country)
  })
  
  output$map_phylo =  renderLeaflet({
    
    
    if(input$search == 0){
      data_leaflet = RSSC1
    }else{
      data_leaflet = filtered_Country_type()
    }
    if(nrow(data_leaflet) == 0){
      leaflet(a)%>%
        addTiles()
    }else{
      
      factpal <- colorFactor(palette = "Set2", domain = unique(RSSC1$Phylotype2))
      
      leaflet(data_leaflet,
              width = "100%",
              height = 15) %>%
        setView(-0, 15, zoom = 2) %>%
        addTiles() %>%
        
        addCircleMarkers(
          radius = 3,
          stroke = FALSE,
          lng = ~Longitude,
          lat =~Latitude,
          color = ~factpal(Phylotype2),
          fillOpacity = 1,
          label = paste(data_leaflet$Phylotype,"- click for details"),
          labelOptions = labelOptions(style = list("font-style" = "italic")),
          popup = paste("Phylotype:</b>", data_leaflet$Phylotype,
                        "<br>",
                        "Host:", data_leaflet$`Host Species (Common name)`,
                        "<br>",
                        "Location:", data_leaflet$`Location Isolated`,
                        "<br>",
                        "Year of collection:", data_leaflet$`Year isolated`,
                        "<br>",
                        "Reference:", data_leaflet$Citation)
        ) %>%
        addLegend("bottomright",
                  pal = factpal,
                  values = ~Phylotype2,
                  title = "Phylotype",
                  opacity = 1
        ) %>%
        addLayersControl(
          baseGroups = c("Default", "Aerial", "Terrain"),
          overlayGroups = "Phylotype",
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Back to initial view",
          onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
        addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
        addProviderTiles("OpenTopoMap", group = "Terrain") %>%
        addScaleBar("bottomleft") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Default")
      # addProviderTiles(providers$CartoDB.Voyager, group = "Default")
      
      
    }
  })
  
  output$download <- downloadHandler(
    filename = function(){"RSSCdb_data.csv"}, 
    content = function(fname){
      
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Country_type()
      }
      
      
      write.csv(data_leaflet, fname)
    })
  
  output$n_Isolates = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = RSSC1
    }else{
      data_leaflet = filtered_Country_type()
    }
    
    n =nrow(data_leaflet)
    if(n>1){sub = "Isolates"}else{sub = "Isolate"}
    infoBox(title = "Collection",
            value = n,
            subtitle = sub,
            color= "fuchsia",
            icon = icon("list")
    )  
    
  })
  
  output$n_Citations = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = RSSC1
    }else{
      data_leaflet = filtered_Country_type()
    }
    
    n = length(unique(data_leaflet$Citation))
    
    if(n>1){sub = "Articles"}else{sub = "Article"}
    infoBox(title = "Literature",
            value = n,
            subtitle = sub,
            color= "fuchsia",
            icon = icon("newspaper")
    )
  })
  
  output$n_Distribution = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = RSSC1
    }else{
      data_leaflet = filtered_Country_type()
    }
    n = length(unique(data_leaflet$`Location (Country or Territory)`))
    if(n>1){sub = "Countries"}else{sub = "Country"}
    infoBox(title = "Distribution",
            value = n,
            subtitle = sub,
            color= "fuchsia",
            icon = icon("globe")
    )
  })
  
  output$n_Hosts = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = RSSC1
    }else{
      data_leaflet = filtered_Country_type()
    }
    n = length(unique(data_leaflet$`Host Species (Common name)`))
    if(n>1){sub = "Hosts"}else{sub = "Host"}
    infoBox(title = "Host Range",
            value = n,
            subtitle = sub,
            color= "fuchsia",
            icon = icon("leaf")
    )
  })
  
}

shinyApp(ui, server)