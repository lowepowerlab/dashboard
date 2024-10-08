library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(shinyjs) 
library(patchwork)
library(cowplot)
library(ggthemes)
library(fresh)
library(plotly)
library(gsheet)
library(googlesheets4)

# read config file (looks for "config.yml" file in working directory)
config <- config::get(config = "development") # use "development" settings for now

# use consistent seed
set.seed(1)

## load the data ##

# config tells use whether we are using google sheets.
# if yes, set authorization info + load from there.
# otherwise, load from local file.
if (config$data_source$use_google_sheet) {
  options(
    gargle_oauth_cache = config$gauth$oauth_cache,
    gargle_oauth_email = config$gauth$oauth_email
  )
  # run sheets authentication
  gs4_auth(use_oob = TRUE)
  
  # load data from sheets
  rssc_data_load <- gsheet2tbl(config$data_source$google_sheet_link)
  
} else {
  # otherwise, load data from local CSV file
  rssc_data_load <- read_csv(config$data_source$local_file_path)
}

# if deploying to shinyapps.io server:
if (config$deploy$deploy_shinyio) {
  files = c(config$shinyio$auth,
            ".Rprofile",
            "app.R")
  rsconnect::deployApp(appFiles = files)
}

## Data transformations ##

# define the selected species and Phylotypes to focus on
#VNE specie_selected = c("F. graminearum","F. meridionale", "F. asiaticum","F. boothii")
Phylotype_selected = c("I", "II", "III", "IV")

# Start data transformations
rssc_data = rssc_data_load %>% 
  # Create a new column Phylotype2 as a copy of Phylotype
  mutate(Phylotype2 = Phylotype) %>%
  # If Phylotype2 is NA, set it to "Undetermined", otherwise keep it as it is
  mutate(Phylotype2 = case_when(!is.na(Phylotype2) ~ Phylotype2,
                                   is.na(Phylotype2) ~ "Undetermined")) %>% 
  
  # Transform Phylotype: keep selected phylotypes, set NA values to "Undetermined",
  # and any phylotype not in the selected list to "Others"
  mutate(Phylotype = case_when( Phylotype %in% Phylotype_selected  ~Phylotype,
                                   is.na(Phylotype) ~ "Undetermined",
                                   !is.na(Phylotype) & !Phylotype %in% Phylotype_selected ~ "Others")) %>%
  # Transform RSSC: keep the value if it's not NA, otherwise set it to "Undetermined"
  # VNE mutate(RSSC = case_when(!is.na(RSSC) ~ RSSC,
  #VNE                        is.na(RSSC) ~ "Undetermined")) %>% 
  # Create a new column RSSC2: if RSSC is one of the selected species, keep it,
  # otherwise, if it's not NA and not in the selected species, set it to "Others"
  # VNE mutate(RSSC2 = case_when(RSSC %in% specie_selected ~ RSSC,
  # VNE                         is.na(RSSC) ~ RSSC,
  # VNE                         !is.na(RSSC) & !RSSC %in% specie_selected ~ "Others"))%>% 
  # Combine Latitude and Longitude into a single "latlong" column (without removing the originals)
  unite("latlong", Latitude, Longitude, sep ="/", remove = F ) %>% 
  # Group data by the new "latlong" column to handle multiple entries for the same coordinates
  group_by(latlong) %>% 
  # If there are multiple points in the same latlong, jitter points (randomly adjust the Latitude and Longitude slightly)
  mutate(n = n(),
    Latitude = case_when(n > 1 ~ rnorm(n, Latitude,0.01),
                         n == 1 ~ Latitude),
    Longitude = case_when(n > 1 ~ rnorm(n, Longitude,0.01),
                          n == 1 ~ Longitude)
  ) %>%
  # Transform Citation: if it's NA, set it to "Unpublished", otherwise keep it as it is
  # VNE mutate(Citation = case_when(!is.na(Citation) ~ Citation,
  # VNE                        is.na(Citation) ~ "Unpublished")) %>% 
  # Transform Host_species: if it's NA, set it to "unknown", otherwise keep it as it is
  # VNE mutate(Host Species = case_when(!is.na(Host Species) ~ Host Species,
  # VNE                        is.na(Host Species) ~ "unknown")) %>%
  # VNE group_by(RSSC) %>%
  # Count the number of isolates per phylotype and create a new RSSC3 column with that count
  # VNE mutate(n_iso_per_phylotype = n(),
  # VNE       RSSC3 = paste(RSSC," (",n_iso_per_phylotype,")", sep = ""),) %>% 
  # VNE ungroup() %>% 
  # Remove temporary columns "n" and "latlong" used for the previous calculations
  dplyr::select(-n, -latlong)

## DEFINE UI and SERVER ##

# create custom theme for dashboard display
mytheme <- create_theme(
  # main dashboard color scheme
  adminlte_color(
    hot_pink = "hotpink"
  ),
  # customize appearance of sidebar
  adminlte_sidebar(
    width = "300px",
    dark_color = "black",
    light_bg = "lightpink",
    dark_hover_bg = "black",
    dark_submenu_color = "black"
  ),
  # style for content box backgrounds
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

# Define dashboard UI
ui = dashboardPage(skin = "hotpink",
  
  dashboardHeader(title = "RSSC Dashboard",
                  titleWidth = 250),
  
  dashboardSidebar(collapsed = F,
                   width = 250,
     br(),
     div(style="display:inline-block;width:80%;margin-left:18px;text-align: left;",
     "A georeferenced database of isolates of the", em("Ralstonia solanacearum") ," species complex.
       Use the filters below to refine search, visualize, and download the data."),   
    sidebarMenu(id = "sidebarid",
                
      # menuItem("Explorer", tabName = "map_view", icon = icon("grid-horizontal")),
      
      # sliderInput(inputId = "year",
      #             label = "Select year interval",
      #             min = 1971,
      #             max = 2024,
      #             step = 1,
      #             value = c(1971,2024)),
      
      # filter drop-down menus
      pickerInput(inputId = "Phylotype",
                  label = "Select Phylotype(s)",
                  choices = unique(rssc_data$Phylotype2),
                  options = list(
                    `actions-box` = TRUE, 
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  selected = unique(rssc_data$Phylotype2),
                  multiple = T),
      
     # pickerInput(inputId = "host",
      #            label = "Host",
       #           choices = sort(unique(rssc_data$Host)),
        #          options = list(
         #           `actions-box` = TRUE, 
          #          size = 10,
           #         `selected-text-format` = "count > 3"
            #      ),
             #     selected = unique(rssc_data$Host),
              #    multiple = T),
      
      # pickerInput(inputId = "country",
        #          label = "Country",
         #         choices = sort(unique(rssc_data$Country)),
          #        options = list(
           #         `actions-box` = TRUE, 
            #        size = 10,
             #       `selected-text-format` = "count > 3"
              #    ),
               #   selected = unique(rssc_data$Country),
                #  multiple = T),
      
      # pickerInput(inputId = "article",
        #          label = "Reference",
         #         choices = sort(unique(rssc_data$Pub1)),
          #        options = list(
           #         `actions-box` = TRUE, 
            #        size = 10,
             #       `selected-text-format` = "count > 3"
              #    ),
               #   selected = unique(rssc_data$Pub1),
                #  multiple = T),
      
      # buttons on sidebar
      div(style="display:inline-block;width:25%;text-align: center;",
      actionButton(inputId = "search",
                   label = "Filter",
                   icon =icon("filter"))),
      div(style="display:inline-block;width:25%;text-align: center;",
      actionButton(inputId = "reset",
                   label = "Select all",
                   icon =icon("retweet"))),
      # br(),
      div(style="display:inline-block;width:60%;text-align: center;",
        downloadButton("download",
                       "Get data"))
      )
  ),
  # main body part of dashboard
  dashboardBody(use_theme(mytheme),
    # tabItems(
      # tabItem(tabName = "map_view",
              shinyjs::useShinyjs(),
              div(id = "myapp",
              fluidRow(
                # counts boxes at the top
                valueBoxOutput("n_isolados", width = 3),
                infoBoxOutput("n_artigos", width = 3),
                infoBoxOutput("n_countries", width = 3),
                valueBoxOutput("n_species", width = 3),
                # infoBoxOutput("n_hosts", width = 2),
                
                # main display box with tabs
                tabBox(title = "",
                       width = 12,
                       height = "100%",
                       # species map tab
                      # tabPanel(icon = icon("map"),
                       #         "Map: Species",
                        #        leafletOutput("map_specie",
                         #                     width = "100%",
                          #                    height = 500)
                                ),
                       # Phylotype map tab
                       tabPanel(icon = icon("map"),
                                "Map: Phylotypes",
                                leafletOutput("map_myco",
                                              width = "100%",
                                              height = 500)
                                ),
                       # table tab
                       tabPanel(icon = icon("table"),
                                "Grid View",
                                # div(
                                  DT::dataTableOutput("grid")#)
                       ),
                       # charts tab
                       tabPanel(icon = icon("chart-bar"),
                                "Charts",
                                plotlyOutput("plot1")
                                # plotOutput("plot1",
                                           # width = "100%",
                                           # height = 500)#)
                       ),
                       # trends tab
                       tabPanel(icon = icon("chart-line"),
                                "Trends",
                                plotlyOutput("plot2")
                                # plotOutput("plot2",
                                # width = "100%",
                                # height = 500)
                                )
                       
                       ),
               # tabBox(title = "",
               #         width = 4,
               #         height = 500,
               #         tabPanel(icon = icon("chart-bar"),
               #                  "Species",
               #                  plotOutput("plot1")),
               #         tabPanel(icon = icon("chart-line"),
               #                  "Temporal",
               #                  plotOutput("plot2"))
               #    
               #  ),
               # box(width =2,
               #     height = 30,
               #     solidHeader = F),
                
                
                       ))
             
              )
      # tabItem()grid 
    # )
    
  # )
  
# )

# define server; if page is refreshed, reset everything
server = function(input, output, session) {
  
  observeEvent(input$reset, {
    shinyjs::reset("year")
    shinyjs::reset("host")
    shinyjs::reset("specie")
    shinyjs::reset("Phylotype")
    shinyjs::reset("country")
    shinyjs::reset("article")

    
  })
 
  
# these define the impact of filtering by the sidebar drop-down menus
filtered_year_type <- eventReactive(input$search,{
      # no impact right now, this seems to be turned off
      rssc_data #%>% 
           # filter(Year >= input$year[1],
                  # Year <= input$year[2])
  })

#filtered_specie_type <- eventReactive(input$search,{
  
 # filtered_year_type() %>%
  #  filter(FGSC3 %in% input$specie)
  
# })

filtered_Phylotype_type <- eventReactive(input$search,{
  
  filtered_specie_type() %>%
    filter(Phylotype2 %in% input$Phylotype)
  
})

#filtered_host_type <- eventReactive(input$search,{
  
#  filtered_Phylotype_type() %>%
 #   filter(Host %in% input$host)
  
# })

#filtered_country_type <- eventReactive(input$search,{
  
 # filtered_host_type() %>%
  #  filter(Country %in% input$country)
  
# })

#filtered_article_type <- eventReactive(input$search,{

 # filtered_country_type() %>%
  #    filter(Pub1 %in% input$article)

#})




     
  
# code for the species-level map
#output$map_specie =  renderLeaflet({
  
  
 # if(input$search == 0){
  #  data_leaflet = rssc_data
#  }else{
#    data_leaflet = filtered_article_type()
#  }
#  if(nrow(data_leaflet) == 0){
#    leaflet(a)%>%
#      addTiles()
#  }else{
#    
#    factpal <- colorFactor(palette = "Set2", domain = unique(rssc_data$FGSC2))
    
#    leaflet(data_leaflet,
#            width = "100%",
#            height = 15) %>%
#      setView(-0, 15, zoom = 2) %>%
#      addTiles() %>%
      
#      addCircleMarkers(
#        radius = 3,
#        stroke = FALSE,
#        lng = ~Longitude,
#        lat = ~Latitude,
#        color = ~factpal(FGSC2),
#        fillOpacity = 1,
#        label = paste(data_leaflet$FGSC, "- click for details"),
#        labelOptions = labelOptions(style = list("font-style" = "italic")),
#        popup = paste("Collection code:<b>",
#                      data_leaflet$`Collection code`,
#                      "</b>", "<br><i>",
#                      data_leaflet$FGSC, "</i>(", data_leaflet$Phylotype2,
#                      ")<br>", "Host:", data_leaflet$Host, "<br>",
#                      "Location:", data_leaflet$Country, "<br>",
#                      "Year of collection:",
#                      data_leaflet$Year, "<br>", "Reference:", data_leaflet$Pub1)
#      ) %>%
#      addLegend("bottomright",
#                pal = factpal,
#                values = ~FGSC2,
#                title = "Species",
#                opacity = 1
#      ) %>%
#      addLayersControl(
#        baseGroups = c("Default", "Aerial", "Terrain"),
#       overlayGroups = "FGSC",
#        options = layersControlOptions(collapsed = TRUE)
#      ) %>%
#      addEasyButton(easyButton(
#        icon="fa-globe", title="Back to initial view",
#        onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
#      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
#      addProviderTiles("OpenTopoMap", group = "Terrain") %>%
#      addScaleBar("bottomleft") %>%
#      addProviderTiles(providers$CartoDB.Positron, group = "Default")
      # addProviderTiles(providers$CartoDB.Voyager, group = "Default")
    
    
#  }
# })

# code for Phylotype level map
output$map_myco = renderLeaflet({

  
if(input$search == 0){
  data_leaflet = rssc_data
}else{
  data_leaflet = filtered_article_type()
}
  
  
    if(nrow(data_leaflet) == 0){
      mico_map = leaflet(a) %>%
        addTiles()
    }else{

    factpal <- colorFactor(palette = "Set2", domain = unique(rssc_data$Phylotype))

    leaflet(data_leaflet,
          width = "100%",
          height = 15) %>%
    setView(-0, 15, zoom = 2) %>%
    addTiles() %>%

    addCircleMarkers(
      radius = 3,
      lng = ~Longitude,
      lat =~Latitude,
      color = ~factpal(Phylotype),
      fillOpacity = 1,
      stroke = FALSE,
      label = paste(data_leaflet$RSSC," - click for details"),
      labelOptions = labelOptions(style = list("font-style" = "italic")),
      popup = paste("Collection code:<b>",
                    data_leaflet$`Collection code`,
                    "</b>", "<br><i>",
                    data_leaflet$RSSC,"</i>(", data_leaflet$Phylotype2,
                    ")<br>", "Host:", data_leaflet$Host,"<br>",
                    "Location:", data_leaflet$Country, "<br>",
                    "Year of collection:",
                    data_leaflet$Year, "<br>","Reference:", data_leaflet$Pub1)
  ) %>%
    addLegend("bottomright",
             pal = factpal,
             values = ~Phylotype,
             title = "Phylotype",
             opacity = 1
  ) %>%
    addLayersControl(
      baseGroups = c("Default", "Aerial", "Terrain"),
      overlayGroups = "RSSC",
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

 

  # table code
  output$grid = DT::renderDataTable({
    
    if(input$search == 0){
      data_leaflet = rssc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    data_leaflet %>% 
      mutate(Article = Pub1) %>% 
      dplyr::select(`Collection.code`, Host, Country, Year, RSSC, Phylotype, Article )
    
  },options = list(autoWidth = F,autoHeight = F, scrollX = TRUE))
  
  
  # chart code
#  output$plot1 = renderPlotly({
#    
#    if(input$search == 0){
#      data_leaflet = rssc_data
#    }else{
#      data_leaflet = filtered_article_type()
#    }
    
#    g_fgsc = data_leaflet %>% 
#      group_by(RSSC,Phylotype) %>%  
#      summarise(count = n()) %>% 
#     ggplot(aes(reorder(RSSC,count),count, fill = Phylotype))+
#      geom_col()+
      # scale_y_log10()+
#      theme_minimal_vgrid(font_size = 10)+
#      scale_fill_viridis_d(na.value = "grey50")+
#      labs(x = "Species",
#           fill = "Phylotype",
#           title = "Frequency of isolates by species and Phylotype")+
#      theme(axis.text.y = element_text(face = "italic"),
#            panel.background = element_rect(color = "gray"),
#            legend.position = "bottom")+
#      coord_flip()  

#    ggplotly(g_rssc) 

   
#  })
  
  # trends code
#  output$plot2 = renderPlotly({
    
#    if(input$search == 0){
#      data_leaflet = rssc_data
#    }else{
#      data_leaflet = filtered_article_type()
#    }
    
#    g2 = data_leaflet %>% 
#      group_by(Year, RSSC2) %>%  
#      summarise(count_name_occurr = n()) %>% 
#       ungroup() %>% 
#       group_by(RSSC2) %>% 
#       mutate(count = cumsum(count_name_occurr)) %>% 
#      ggplot(aes(Year, count, color= RSSC2))+
      # geom_col(size = .1, color= "white", alpha =0.9)+
#      geom_line(size = 1) +
#      geom_point(size=2)+
      # scale_y_log10()+
#      theme_minimal_vgrid(font_size = 10)+
#      scale_color_colorblind()+
#      labs(x = "Year",
#           y = "Cumulative number of isolates",
#           color = "Species",
#           title = "Frequency of isolates over time")+
#      guides(color=guide_legend(nrow=2, byrow=TRUE))+
#      theme(axis.text.y = element_text(face = "italic"),
#            panel.background = element_rect(color = "gray"),
#            legend.text =  element_text(face = "italic"),
#            legend.position = "bottom")
    
#    ggplotly(g2)
    
    
#  })
  
  # enable data download
  output$download <- downloadHandler(
    filename = function(){"RSSC.csv"}, 
    content = function(fname){
      
      if(input$search == 0){
        data_leaflet = rssc_data
      }else{
        data_leaflet = filtered_article_type()
      }
      
      
      write.csv(data_leaflet, fname)
    })
  
  # Isolates info box
  output$n_isolados = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = rssc_data
    }else{
      data_leaflet = filtered_article_type()
    }
 
    n =nrow(data_leaflet)
    if(n>1){sub = "Isolates"}else{sub = "Isolate"}
    infoBox(title = "Collection",
            value = n,
            subtitle = sub,
            color= "blue",
            icon = icon("list")
    )  
    
  })
  
  # Articles info box
  output$n_artigos = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = rssc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    
    n = length(unique(data_leaflet$Pub1)[unique(data_leaflet$Pub1) != "Unpublished"])
    
    if(n>1){sub = "Articles"}else{sub = "Article"}
    infoBox(title = "Literature",
      value = n,
      subtitle = sub,
      color= "olive",
      icon = icon("newspaper")
    )
  })
  
  # countries info box
  output$n_countries = renderInfoBox({

    if(input$search == 0){
      data_leaflet = rssc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    n = length(unique(data_leaflet$Country))
    if(n>1){sub = "Countries"}else{sub = "Country"}
    infoBox(title = "Distribution",
             value = n,
             subtitle = sub,
             color= "navy",
             icon = icon("globe")
    )
  })

  # Species info box
  output$n_species = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = rssc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    
    n = length(unique(data_leaflet$FGSC)[unique(data_leaflet$FGSC) != "Undetermined"])
    if(n>1){sub = "Species"}else{sub = "Species"}
    infoBox(title = "Diversity",
            value = n,
            subtitle = sub,
            color= "red",
            icon = icon("chart-pie")
    )
  })
  output$n_hosts = renderInfoBox({
    
    if(input$search == 0){
      data_leaflet = rssc_data
    }else{
      data_leaflet = filtered_article_type()
    }
    n = length(unique(data_leaflet$Host))
    if(n>1){sub = "Hosts"}else{sub = "Host"}
    infoBox(title = "Host range",
            value = n,
            subtitle = sub,
            color= "olive",
            icon = icon("leaf")
    )
  })
  
}

# launch app using ui and server components defined above
shinyApp(ui, server)