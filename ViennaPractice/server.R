#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

server <- function(input, output, session) {
  
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
          color= "pink",
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
          color= "hotpink",
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
          color= "pink",
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
          color= "hotpink",
          icon = icon("leaf")
  )
})

}
