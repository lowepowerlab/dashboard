# load packages
library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(maps)
library(ggmap)
library(DT)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(htmlwidgets)
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

# load map skin
world <- ne_countries(scale = "medium", returnclass = "sf")

# load data and transformations
RSSC <- read_csv("RSSC_Practice.csv")

Phylotype_selected = c("I", "II", "III", "IV")
PandemicLineage_selected = c("1", "2")
VegetativelyPropagatedHosts_selected = c("Anthurium sp. (Laceleaf)", "Curcuma longa (Turmeric)", "Curcuma aromatica (Wild Turmeric)",
                                  "Curcuma zedoaria (White Turmeric)", "Curcuma aeruginoa (Blue and Pink Ginger)", "Curcuma mangga (Mango Ginger)",
                                  "Epipremnum aureum (Pothos)", "Kaempferia galanga (Aromatic Ginger)", "Musa acuminata (Banana)",
                                  "Musa paradisiaca (Plantain)", "Musa sp. (Banana Plant)", "Musa acuminata x balbisiana AAB",
                                  "Pelargonium capitatum (Rose Geranium)", "Pelargonium sp. (Geranium)", "Pelargonium x asperum",
                                  "Pelargonium x hortorum", "Pelargonium zonale (Geranium)", "Rosa sp. (Rose)",
                                  "Solanum tuberosum (Potato)", "Zingiber cassumunar (Cassumunar Ginger)", "Zingiber mioga (Myoga Ginger)",
                                  "Zingiber officinale (Ginger)")

RSSC1 = RSSC %>% 
  mutate(Phylotype2 = Phylotype) %>%
  mutate(Phylotype2 = case_when(!is.na(Phylotype2) ~ Phylotype2,
                                is.na(Phylotype2) ~ "Unknown")) %>% 
  
  #mutate(Sequevar2 = Sequevar) %>%
  #mutate(Sequevar2 = case_when(!is.na(Sequevar2) ~ Sequevar2,
  #                             is.na(Sequevar2) ~ "Unknown")) %>%
  #mutate(Sequevar2 = case_when( Sequevar2 %in% PandemicLineage_selected  ~Sequevar2,
  #                    is.na(Sequevar2) ~ "Unknown",
  #                    !is.na(Sequevar2) & !Sequevar2 %in% PandemicLineage_selected ~ "Others")) %>%
  
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
  
  mutate(VPH = `Host Species (Common name)`) %>%
  mutate(VPH = case_when(!is.na(VPH) ~ VPH,
                               is.na(VPH) ~ "Unknown")) %>%
  mutate(VPH = case_when(VPH %in% VegetativelyPropagatedHosts_selected  ~VPH,
                      is.na(VPH) ~ "Unknown",
                      !is.na(VPH) & !`VPH` %in% VegetativelyPropagatedHosts_selected ~ "Others")) %>%
  
  mutate(`Host Family` = case_when(!is.na(`Host Family`) ~ `Host Family`,
                                                  is.na(`Host Family`) ~ "Unknown")) %>%
  
  mutate(`Year published` = case_when(!is.na(`Year published`) ~ as.character(`Year published`),
                                                  is.na(`Year published`) ~ "Unknown")) %>%
  
  mutate(`Year isolated` = case_when(!is.na(`Year isolated`) ~ `Year isolated`,
                                                  is.na(`Year isolated`) ~ "Unknown")) %>%
  
  mutate(`Location (continent)` = case_when(!is.na(`Location (continent)`) ~ `Location (continent)`,
                                     is.na(`Location (continent)`) ~ "Unknown")) %>%
  
  mutate(`Location (Country or Territory)` = case_when(!is.na(`Location (Country or Territory)`) ~ `Location (Country or Territory)`,
                                     is.na(`Location (Country or Territory)`) ~ "Unknown")) %>%
  
  group_by(Phylotype) %>% 
  mutate(n_iso_per_Phylotype = n(),
         Phylotype3 = paste(Phylotype," (",n_iso_per_Phylotype,")", sep = ""),) %>% 
  ungroup() %>% 
  dplyr::select(-n, -latlong)


# ui
ui = dashboardPage(skin = "black",
        dashboardHeader(title = "Ralstonia Wilt Db", titleWidth = 250),
        dashboardSidebar(collapsed = F, width = 250,
        br(),
        div(style = "display:inline-block;width:80%;margin-left:18px;text-align: left;",
        "A georeferenced database of isolates of the", em("Ralstonia solanacearum"), "Species Complex.
        Use the filters below to refine your search, visualize data, and download metadata."),
          sidebarMenu(id = "sidebarid",
           # filter drop-down menus
            pickerInput(inputId = "publication_year",
                        label = "Publication Year",
                        choices = sort(unique(RSSC1$`Year published`)),
                        options = list(
                        `live-search` = T,  
                        `actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = sort(unique(RSSC1$`Year published`)),
                        multiple = T
            ),
            pickerInput(inputId = "isolation_year",
                        label = "Isolation Year",
                        choices = sort(unique(RSSC1$`Year isolated`)),
                        options = list(
                        `live-search` = T,
                        `actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = sort(unique(RSSC1$`Year isolated`)),
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
            pickerInput(inputId = "pandemic_lineage",
                        label = "Pandemic Lineages",
                        choices = c("Sequevar 1", "Sequevar 2"),
                        options = list(`actions-box` = T,
                                  size = 10,
                                  `selected-text-format` = "count > 1"
                        ),
                        selected = c("Sequevar 1", "Sequevar 2"),
                        multiple = T
            ),
            pickerInput(inputId = "host_family",
                        label = "Host Family",
                        choices = sort(unique(RSSC1$`Host Family`)),
                        options = list(
                        `live-search` = T,
                        `actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = sort(unique(RSSC1$`Host Family`)),
                        multiple = T
            ),
            pickerInput(inputId = "host_species",
                        label = "Host Species",
                        choices = sort(unique(RSSC1$`Host Species (Common name)`)),
                        options = list(
                        `live-search` = T,
                        `actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = sort(unique(RSSC1$`Host Species (Common name)`)),
                        multiple = T
            ), 
            #pickerInput(inputId = "vegprophost",
             #           label = "Vegetatively Propagated Hosts",
              #          choices = sort(unique(RSSC1$VPH)),
               #         options = list(
                #        `live-search` = T,
                 #       `actions-box` = T,
                  #      size = 10,
                   #     `selected-text-format` = "count > 1"
                    #    ),
                     #   selected = sort(unique(RSSC1$VPH)),
                        #choicesOpt = list(disabled = c("Others")),
                      #  multiple = T
          #  ), 
            pickerInput(inputId = "continent",
                        label = "Continent",
                        choices = sort(unique(RSSC1$`Location (continent)`)),
                        options = list(
                        `live-search` = T,  
                        `actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = sort(unique(RSSC1$`Location (continent)`)),
                        multiple = T
            ),
            pickerInput(inputId = "country",
                        label = "Country or Territory",
                        choices = sort(unique(RSSC1$`Location (Country or Territory)`)),
                        options = list(
                        `live-search` = T,  
                        `actions-box` = T,
                        size = 10,
                       `selected-text-format` = "count > 1"
                        ),
                        selected = sort(unique(RSSC1$`Location (Country or Territory)`)),
                        multiple = T
            ),
            pickerInput(inputId = "genome",
                        label = "Genome Available on NCBI?",
                        choices = c("Yes", "No"),
                        options = list(
                        `live-search` = F,  
                        `actions-box` = T,
                         size = 10,
                        `selected-text-format` = "count > 1"
                      ),
                         selected = c("Yes", "No"),
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
                  #  box(title = "Practice Graph",width = 40, plotlyOutput("graph")),
                   # box(title = "Table of Countries within a Phylotype", width = "600px",
                    #    dataTableOutput("filtered_table"))
                    #row
                    fluidRow(
                      box(title = "How to Interpret this Map",
                          "This map shows the reported isolation locations of Ralstonia and 
                            each datapoint should be regarded with healthy skepticism. Isolation of 
                            Ralstonia at a location does not mean it is currently established at that
                            location; eradication has been successful in certain cases (e.g. in Sweden) 
                            and some isolations might be from imported plants that were quarantined/destroyed. 
                            Additionally, our meta-analysis database likely contains a low incidence of 
                            errors from the primary literature, from our data entry, or from the geocoding 
                            algorithm that assigned latitude/longitude coordinates to written locations.",
                          solidHeader = T,
                          width = 12,
                          collapsible = T,
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
                            DT::dataTableOutput("Metadata_table")))
                            
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
    shinyjs::reset("vegprophost")
    shinyjs::reset("continent")
    shinyjs::reset("country")
  })
  
  
  filtered_PublicationYear_type <- eventReactive(input$search,{
    RSSC1 %>%
      filter(`Year published` %in% input$publication_year)
    
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
  
  filtered_VegProp_type <- eventReactive(input$search,{
    filtered_HostSpecies_type() %>%
      filter(VPH %in% input$vegprophost)
  })
  
  filtered_Continent_type <- eventReactive(input$search,{
    filtered_VegProp_type() %>%
      filter(`Location (continent)` %in% input$continent)
  })
  
  filtered_Country_type <- eventReactive(input$search,{
    filtered_Continent_type() %>%
      filter(`Location (Country or Territory)` %in% input$country)
  })
  
# Output leaflet map
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

# Output metadata table
    output$Metadata_table = DT::renderDataTable({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Country_type()
      }
    },options = list(autoWidth = F,autoHeight = F, scrollX = TRUE))  
  
# Output download data button    
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

# Output info boxes at top of page    
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