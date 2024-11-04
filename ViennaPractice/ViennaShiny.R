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
library(htmltools)
library(htmlwidgets)
library(markdown)
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
library(MetBrewer)
library(devtools)
#install.packages("MetBrewer")
#install.packages("devtools")
#devtools::install_github("BlakeRMills/MetBrewer")


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
RSSC <- read_csv("RSSC_Whole.csv")

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
  
  mutate(Sequevar2 = Sequevar) %>%
  mutate(Sequevar2 = case_when(!is.na(Sequevar2) ~ Sequevar2,
                               is.na(Sequevar2) ~ "Unknown")) %>%
  mutate(Sequevar2 = case_when( Sequevar2 %in% PandemicLineage_selected ~ Sequevar2,
                      is.na(Sequevar2) ~ "Unknown",
                      !is.na(Sequevar2) & !Sequevar2 %in% PandemicLineage_selected ~ "Non pandemic lineage")) %>%
  
  mutate(Genome2 = `Genome Accession`) %>%
  mutate(Genome2 = case_when(!is.na(Genome2) ~ "Yes",
                                is.na(Genome2) ~ "No")) %>% 
  
  unite("latlong", Latitude, Longitude, sep ="/", remove = F ) %>% 
  group_by(latlong) %>% 
  mutate(n = n(),
         Latitude = case_when(n > 1 ~ rnorm(n, Latitude,0.01),
                              n == 1 ~ Latitude),
         Longitude = case_when(n > 1 ~ rnorm(n, Longitude,0.01),
                               n == 1 ~ Longitude)
        ) %>%
  
  mutate(VPH = `Host Species (Common name)`) %>%
  mutate(VPH = case_when(!is.na(VPH) ~ VPH,
                               is.na(VPH) ~ "Unknown Host")) %>%
  mutate(VPH = case_when(VPH %in% VegetativelyPropagatedHosts_selected ~ VPH,
                      is.na(VPH) ~ "Unknown Host",
                      !is.na(VPH) & !VPH %in% VegetativelyPropagatedHosts_selected ~ "Non VP Host")) %>%
  mutate(VPH = recode(VPH, "Musa acuminata (Banana)" = "Banana/Plantain spp.", 
                           "Musa paradisiaca (Plantain)" = "Banana/Plantain spp.",
                           "Musa sp. (Banana Plant)" = "Banana/Plantain spp.", 
                           "Musa acuminata x balbisiana AAB" = "Banana/Plantain spp.",
                           "Pelargonium capitatum (Rose Geranium)" = "Geranium spp.", 
                           "Pelargonium sp. (Geranium)" = "Geranium spp.", 
                           "Pelargonium x asperum" = "Geranium spp.",
                           "Pelargonium x hortorum" = "Geranium spp.", 
                           "Pelargonium zonale (Geranium)" = "Geranium spp.")) %>%
  
  mutate(`Host Species (Common name)` = case_when(!is.na(`Host Species (Common name)`) ~ `Host Species (Common name)`,
                                                  is.na(`Host Species (Common name)`) ~ "Unknown")) %>%
  
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
  
  group_by(Phylotype2) %>% 
  mutate(n_iso_per_Phylotype = n(),
         Phylotype3 = paste(Phylotype2," (",n_iso_per_Phylotype,")", sep = ""),) %>% 
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
                        choices = sort(unique(RSSC1$Phylotype3)),
                        options = list(`actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = unique(RSSC1$Phylotype3),
                        multiple = T
            ),  
            pickerInput(inputId = "pandemic_lineage",
                        label = "Pandemic Lineages",
                        choices = c("Sequevar 1"="1", "Sequevar 2"="2", "Non pandemic lineage", "Unknown"),
                        options = list(
                        `actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = c("Sequevar 1"="1", "Sequevar 2"="2", "Non pandemic lineage", "Unknown"),
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
            pickerInput(inputId = "vegprophost",
                        label = "Vegetatively Propagated (VP) Hosts",
                        choices = list(
                          Araceae = c("Anthurium sp. (Laceleaf)", "Epipremnum aureum (Pothos)"),
                          Geraniacea = list("Geranium spp."),
                          Musaceae = list("Banana/Plantain spp."),
                          Rosaceae = list("Rosa sp. (Rose)"),
                          Solanaceae = list("Solanum tuberosum (Potato)"),
                          Zingiberaceae = c("Curcuma aromatica (Wild Turmeric)", "Curcuma longa (Turmeric)",
                                            "Curcuma zedoaria (White Turmeric)", "Curcuma aeruginoa (Blue and Pink Ginger)",
                                            "Kaempferia galanga (Aromatic Ginger)", "Zingiber cassumunar (Cassumunar Ginger)",
                                            "Curcuma mangga (Mango Ginger)", "Zingiber mioga (Myoga Ginger)", "Zingiber officinale (Ginger)"),
                          Other = c("Non VP Host", "Unknown Host")
                          ),
                        options = list(
                        `live-search` = T,
                        `actions-box` = T,
                        size = 10,
                        `selected-text-format` = "count > 1"
                        ),
                        selected = c("Anthurium sp. (Laceleaf)","Epipremnum aureum (Pothos)","Geranium spp.","Banana/Plantain spp.",
                                     "Rosa sp. (Rose)","Solanum tuberosum (Potato)","Curcuma aromatica (Wild Turmeric)",
                                     "Curcuma longa (Turmeric)","Curcuma zedoaria (White Turmeric)","Curcuma aeruginoa (Blue and Pink Ginger)",
                                     "Kaempferia galanga (Aromatic Ginger)","Zingiber cassumunar (Cassumunar Ginger)",
                                     "Curcuma mangga (Mango Ginger)","Zingiber mioga (Myoga Ginger)","Zingiber officinale (Ginger)",
                                     "Non VP Host", "Unknown Host"),
                        multiple = T
            ), 
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
                    tabBox(title = "",
                           width = 12,
                           height = "100%",
                           tabPanel(icon = icon("disease"),
                                     "RSSC Visualizations",
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
                    #row
                      fluidRow(
                        box(title = "How to Interpret this Map",
                                    "This map shows the reported isolation locations of", em("Ralstonia"), "and 
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
                        box(title = "Phylotype Abundance by Host",
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            collapsed = F,
                            uiOutput("Host_chart"),
                            actionButton("host_linear","Plot by Count"),
                            actionButton("host_log","Plot by Proportion")
                            ),
                        box(title = "Phylotype Abundance by Continent",
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            collapsed = F,
                            uiOutput("Continent_chart"),
                            actionButton("continent_linear","Plot by Count"),
                            actionButton("continent_log","Plot by Proportion")
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
                            
                      ),
                    tabPanel(icon = icon("circle-info"),
                             "About Page",
                     #row
                    fluidRow(  
                     box(htmltools::includeMarkdown("AboutPage.Rmd"),
                         width = 12))
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
    shinyjs::reset("pandemic_lineage")
    shinyjs::reset("host_family")
    shinyjs::reset("host_species")
    shinyjs::reset("vegprophost")
    shinyjs::reset("continent")
    shinyjs::reset("country")
    shinyjs::reset("genome")
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
  
  filtered_PandemicLineage_type <- eventReactive(input$search,{
    filtered_Phylotype_type() %>%
      filter(Sequevar2 %in% input$pandemic_lineage)
  })
  
  filtered_HostFamily_type <- eventReactive(input$search,{
    filtered_PandemicLineage_type() %>%
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
  
  filtered_Genome_type <- eventReactive(input$search,{
    filtered_Country_type() %>%
      filter(Genome2 %in% input$genome)
  })
  
# Output leaflet map
    output$map_phylo =  renderLeaflet({
    
    if(input$search == 0){
      data_leaflet = RSSC1
    }else{
      data_leaflet = filtered_Genome_type()
    }
    if(nrow(data_leaflet) == 0){
      leaflet(a)%>%
        addTiles()
    }else{
      
      factpal <- colorFactor(palette = "Set1", domain = unique(RSSC1$Phylotype2))
      
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
          label = paste(data_leaflet$Phylotype2,"- click for details"),
          labelOptions = labelOptions(style = list("font-style" = "italic")),
          popup = paste("Phylotype:</b>", data_leaflet$Phylotype2,
                        "<br>",
                        "Host:", data_leaflet$`Host Species (Common name)`,
                        "<br>",
                        "Location:", data_leaflet$`Location Isolated`,
                        "<br>",
                        "Year of Collection:", data_leaflet$`Year isolated`,
                        "<br>",
                        "Reference:", data_leaflet$`Publication`)
        ) %>%
        addLegend("bottomright",
                  pal = factpal,
                  values = ~Phylotype2,
                  title = "Phylotype",
                  opacity = 1
        ) %>%
        addLayersControl(
          baseGroups = c("Default", "Aerial", "Terrain"),
          overlayGroups = "Phylotype2",
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Back to initial view",
          onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
        addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
        addProviderTiles("OpenTopoMap", group = "Terrain") %>%
        addScaleBar("bottomleft") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Default")
      
    }
  })

# Output Host Chart  
    observeEvent(input$host_linear, { 
      output$Host_chart <- renderUI({ plotlyOutput("plot_linear") })
    })
    
    observeEvent(input$host_log, { 
      output$Host_chart <- renderUI({ plotlyOutput("plot_log") })
    })
    
    output$plot_linear = renderPlotly({
      
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topten <- data_leaflet %>%
       count(`Host Family`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=10) %>%
        as.data.frame()
      
      host_phylo = data_leaflet %>%
        filter(`Host Family` %in% topten$`Host Family`) %>%
        group_by(`Host Family`,Phylotype2) %>%  
        summarise(count = n()) %>% 
        ggplot(aes(reorder(`Host Family`,count), count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values=met.brewer("Java"),na.value = "grey50")+
        labs(x = "Host Family",
             y = "Isolations Reported (#)",
             fill = "Phylotype") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(host_phylo) 
      
    })

    output$plot_log = renderPlotly({
      
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topten2 <- data_leaflet %>%
        count(`Host Family`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=10) %>%
        as.data.frame()
      
      host_phylo = data_leaflet %>% 
        filter(`Host Family` %in% topten2$`Host Family`) %>%
        group_by(`Host Family`,Phylotype2) %>%  
        summarise(count = n()) %>% 
        ggplot(aes(reorder(`Host Family`,count),count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values=met.brewer("Java"),na.value = "grey50")+
        labs(x = "Host Family",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()
      
      ggplotly(host_phylo) 
      
    })
    
# Output Continent Chart    
    observeEvent(input$continent_linear, { 
      output$Continent_chart <- renderUI({ plotlyOutput("cont_linear") })
    })
    
    observeEvent(input$continent_log, { 
      output$Continent_chart <- renderUI({ plotlyOutput("cont_log") })
    })
    
    output$cont_linear = renderPlotly({
      
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      cont_phylo = data_leaflet %>% 
        group_by(`Location (continent)`,Phylotype2) %>%  
        summarise(count = n()) %>% 
        ggplot(aes(reorder(`Location (continent)`,count),count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values=met.brewer("Java"),na.value = "grey50")+
        labs(x = "Continent",
             y = "Isolations Reported (#)",
             fill = "Phylotype") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(cont_phylo) 
      
      
    })
    
    output$cont_log = renderPlotly({
      
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      cont_phylo = data_leaflet %>% 
        group_by(`Location (continent)`,Phylotype2) %>%  
        summarise(count = n()) %>% 
        ggplot(aes(reorder(`Location (continent)`,count),count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values=met.brewer("Java"),na.value = "grey50")+
        labs(x = "Continent",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(cont_phylo) 
      
      
    })
    
# Output metadata table
    output$Metadata_table = DT::renderDataTable({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      data_leaflet %>% 
        dplyr::select(Index, Phylotype, Sequevar, Strainname,`Host Species (Common name)`,
                      `Host Family`, `Host Order`, `Year isolated`, `Location Isolated`, 
                      `Location (Country or Territory)`, `Location (continent)`, `Genome Accession`, Publication)
    },options = list(autoWidth = F,autoHeight = F, scrollX = TRUE))  
  
# Output download data button    
  output$download <- downloadHandler(
    filename = function(){"RSSCdb_data.csv"}, 
    content = function(fname){
      
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      
      write.csv(data_leaflet, fname)
    })

# Output info boxes at top of page    
  output$n_Isolates = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = RSSC1
    }else{
      data_leaflet = filtered_Genome_type()
    }
    
    n =nrow(data_leaflet)
    if(n>1){sub = "Isolates"}else{sub = "Isolate"}
    infoBox(title = "Collection",
            value = n,
            subtitle = sub,
            color= "fuchsia",
            icon = icon("viruses")
    )  
    
  })
  
  output$n_Citations = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = RSSC1
    }else{
      data_leaflet = filtered_Genome_type()
    }
    
    n = length(unique(data_leaflet$Publication))
    
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
      data_leaflet = filtered_Genome_type()
    }
    n = length(unique(data_leaflet$`Location (Country or Territory)`))
    if(n>1){sub = "Countries"}else{sub = "Country"}
    infoBox(title = "Distribution",
            value = n,
            subtitle = sub,
            color= "fuchsia",
            icon = icon("earth-americas")
    )
  })
  
  output$n_Hosts = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = RSSC1
    }else{
      data_leaflet = filtered_Genome_type()
    }
    n = length(unique(data_leaflet$`Host Species (Common name)`))
    if(n>1){sub = "Hosts"}else{sub = "Host"}
    infoBox(title = "Host Range",
            value = n,
            subtitle = sub,
            color= "fuchsia",
            icon = icon("plant-wilt")
    )
  })
  
}

shinyApp(ui, server)