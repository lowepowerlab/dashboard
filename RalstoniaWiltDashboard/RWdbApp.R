# load packages needed for Shiny App
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
library(sp)
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
library(usethis)

# create color theme for dashboard
mytheme <- create_theme(adminlte_color(light_blue = "#022851"),
                        adminlte_sidebar(width = "300px",dark_color = "#444",
                                         dark_bg = "#022851",dark_hover_bg = "#444",
                                         dark_submenu_color = "#444"),
                        adminlte_global(content_bg = "#B3C1D1",box_bg = "#FFFFFF",
                                        info_box_bg = "#FFFFFF"))

# load map layer for main plot
world <- ne_countries(scale = "medium", returnclass = "sf") 

# load data from csv
#RSSC <- read_csv("RSSC_Final.csv")

# load data from Google sheets direct link
RSSC <- gsheet2tbl("https://docs.google.com/spreadsheets/d/19Osv46GZUz0wYaHa6hf2HBqbm9ScafID_5tGVWJMlX8/edit?gid=1405797353#gid=1405797353")

# define data groups
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
# data transformations
RSSC1 = RSSC %>% 
  mutate(Phylotype2 = Phylotype) %>%
  mutate(Phylotype2 = case_when(!is.na(Phylotype2) ~ Phylotype2,
                                is.na(Phylotype2) ~ "Unknown")) %>% 
  mutate(Sequevar2 = Sequevar) %>%
  mutate(Sequevar2 = case_when(!is.na(Sequevar2) ~ Sequevar2,
                               is.na(Sequevar2) ~ "Unknown")) %>%
  mutate(Sequevar2 = case_when(Sequevar2 %in% PandemicLineage_selected ~ Sequevar2,
                               is.na(Sequevar2) ~ "Unknown",
                               !is.na(Sequevar2) & !Sequevar2 %in% PandemicLineage_selected ~ "Non pandemic lineage")) %>%
  mutate(Sequevar3 = Sequevar) %>%
  mutate(Sequevar3 = case_when(!is.na(Sequevar3) ~ Sequevar3,
                               is.na(Sequevar3) ~ "Unknown")) %>%
  mutate(Genome2 = `Genome Accession`) %>%
  mutate(Genome2 = case_when(!is.na(Genome2) ~ "Yes",
                             is.na(Genome2) ~ "No")) %>% 
  mutate(Latitude2 = `Lat (AI)`) %>%
  mutate(Latitude2 = case_when(!is.na(Latitude2) ~ Latitude2,
                               is.na(Latitude2) ~ Latitude2)) %>%
  mutate(Longitude2 = `Long (AI)`) %>%
  mutate(Longitude2 = case_when(!is.na(Longitude2) ~ Longitude2,
                                is.na(Longitude2) ~ Longitude2)) %>%
  unite("latlong", Latitude2, Longitude2, sep ="/", remove = F ) %>% 
  group_by(latlong) %>% 
  mutate(n = n(),
         Latitude2 = case_when(n > 1 ~ rnorm(n, Latitude2,0.01),n == 1 ~ Latitude2),
         Longitude2 = case_when(n > 1 ~ rnorm(n, Longitude2,0.01),n == 1 ~ Longitude2)) %>%
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
  mutate(`Host Species` = case_when(!is.na(`Host Species`) ~ `Host Species`,
                                    is.na(`Host Species`) ~ "Unknown")) %>%   
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
  mutate(n_iso_per_Phylotype = n(),Phylotype3 = paste(Phylotype2," (",n_iso_per_Phylotype,")", sep = ""),) %>% 
  ungroup() %>% 
  dplyr::select(-n, -latlong)

# ui
ui <- dashboardPage(
        dashboardHeader(title = "Ralstonia Wilt Dashboard", titleWidth = 250),
        dashboardSidebar(collapsed = F, width = 250,
        br(),
        div(style = "display:inline-block;width:80%;margin-left:18px;text-align: left;",
        "A georeferenced database of isolates of the", em("Ralstonia solanacearum"), "Species Complex.
        Use the filters below to refine your search. Filtration works sequentially. Each filter refines 
        the data and passes it to the next filter. If a filter is completely deselected, all data from 
        that filter will be removed."),
          sidebarMenu(id = "sidebarid", 
            # picker inputs for filter function drop-down menus
             pickerInput(inputId = "publication_year",
                         label = "Publication Year",
                         choices = sort(unique(RSSC1$`Year published`)),
                         options = list(`live-search` = T,`actions-box` = T,size = 10,`selected-text-format` = "count > 1"),
                         selected = sort(unique(RSSC1$`Year published`)),
                         multiple = T),
             pickerInput(inputId = "isolation_year",
                         label = "Isolation Year",
                         choices = sort(unique(RSSC1$`Year isolated`)),
                         options = list(`live-search` = T,`actions-box` = T,size = 10,`selected-text-format` = "count > 1"),
                         selected = sort(unique(RSSC1$`Year isolated`)),
                         multiple = T),
             pickerInput(inputId = "phylo",
                         label = "Phylotype",
                         choices = sort(unique(RSSC1$Phylotype3)),
                         options = list(`actions-box` = T,size = 10,`selected-text-format` = "count > 1"),
                         selected = unique(RSSC1$Phylotype3),
                         multiple = T),
             pickerInput(inputId = "sequevar",
                         label = "Sequevar",
                         choices = sort(unique(RSSC1$Sequevar3)),
                         options = list(`actions-box` = T,size = 10,`selected-text-format` = "count > 1"),
                         selected = unique(RSSC1$Sequevar3),multiple = T),
             pickerInput(inputId = "pandemic_lineage",
                         label = "Pandemic Lineages",
                         choices = c("Sequevar 1"="1", "Sequevar 2"="2", "Non pandemic lineage", "Unknown"),
                         options = list(`actions-box` = T,size = 10,`selected-text-format` = "count > 1"),
                         selected = c("Sequevar 1"="1", "Sequevar 2"="2", "Non pandemic lineage", "Unknown"),
                         multiple = T),
             pickerInput(inputId = "host_family",
                         label = "Host Family",
                         choices = sort(unique(RSSC1$`Host Family`)),
                         options = list(`live-search` = T,`actions-box` = T,size = 10,`selected-text-format` = "count > 1"),
                         selected = sort(unique(RSSC1$`Host Family`)),
                         multiple = T),
             pickerInput(inputId = "host_species",
                         label = "Host Species",
                         choices = sort(unique(RSSC1$`Host Species (Common name)`)),
                         options = list(`live-search` = T,`actions-box` = T,size = 10,`selected-text-format` = "count > 1"),
                         selected = sort(unique(RSSC1$`Host Species (Common name)`)),
                         multiple = T), 
             pickerInput(inputId = "vegprophost",
                         label = "Vegetatively Propagated (VP) Hosts",
                         choices = list(Araceae = c("Anthurium sp. (Laceleaf)", "Epipremnum aureum (Pothos)"),
                                        Geraniacea = list("Geranium spp."),
                                        Musaceae = list("Banana/Plantain spp."),
                                        Rosaceae = list("Rosa sp. (Rose)"),
                                        Solanaceae = list("Solanum tuberosum (Potato)"),
                                        Zingiberaceae = c("Curcuma aromatica (Wild Turmeric)", "Curcuma longa (Turmeric)",
                                         "Curcuma zedoaria (White Turmeric)", "Curcuma aeruginoa (Blue and Pink Ginger)",
                                         "Kaempferia galanga (Aromatic Ginger)", "Zingiber cassumunar (Cassumunar Ginger)",
                                         "Curcuma mangga (Mango Ginger)", "Zingiber mioga (Myoga Ginger)", "Zingiber officinale (Ginger)"),
                                        Other = c("Non VP Host", "Unknown Host")),
                         options = list(`live-search` = T,`actions-box` = T,size = 10,`selected-text-format` = "count > 1"),
                         selected = c("Anthurium sp. (Laceleaf)","Epipremnum aureum (Pothos)","Geranium spp.","Banana/Plantain spp.",
                                     "Rosa sp. (Rose)","Solanum tuberosum (Potato)","Curcuma aromatica (Wild Turmeric)",
                                     "Curcuma longa (Turmeric)","Curcuma zedoaria (White Turmeric)","Curcuma aeruginoa (Blue and Pink Ginger)",
                                     "Kaempferia galanga (Aromatic Ginger)","Zingiber cassumunar (Cassumunar Ginger)",
                                     "Curcuma mangga (Mango Ginger)","Zingiber mioga (Myoga Ginger)","Zingiber officinale (Ginger)",
                                     "Non VP Host", "Unknown Host"),
                         multiple = T),
             pickerInput(inputId = "continent",
                         label = "Continent",
                         choices = sort(unique(RSSC1$`Location (continent)`)),
                         options = list(`live-search` = T,`actions-box` = T,size = 10,`selected-text-format` = "count > 1"),
                         selected = sort(unique(RSSC1$`Location (continent)`)),
                         multiple = T),
             pickerInput(inputId = "country",
                         label = "Country or Territory",
                         choices = sort(unique(RSSC1$`Location (Country or Territory)`)),
                         options = list(`live-search` = T,`actions-box` = T,size = 10,`selected-text-format` = "count > 1"),
                         selected = sort(unique(RSSC1$`Location (Country or Territory)`)),
                         multiple = T),
             pickerInput(inputId = "genome",
                         label = "Genome Available on NCBI?",
                         choices = c("Yes", "No"),
                         options = list(`live-search` = F,`actions-box` = T,size = 10,`selected-text-format` = "count > 1"),
                         selected = c("Yes", "No"),
                         multiple = T),
        # buttons below filter options in sidebar
          div(style="display:inline-block;width:25%;text-align:center;",
              actionButton(inputId = "search",label = "Filter",icon =icon("filter"))),
          div(style="display:inline-block;width:25%;text-align:center;",
              actionButton(inputId = "reset",label = "Select All",icon =icon("retweet"))),
          br(),
          div(style="display:inline-block;width:25%;text-align:center;",
              downloadButton("downloadfiltered", "Download Your Filtered Dataset")),
          br(), 
          div(style="display:inline-block;width:25%;text-align:center;",
              downloadButton("downloadentire", "Download Entire RSSC Dataset")))
        ), #close of sidebar
        dashboardBody(use_theme(mytheme),
                      tags$head(tags$link(rel = "stylesheet", type = "text/css", href="style.css")),
                      shinyjs::useShinyjs(),
                      div(id = "my app",
                    #row 
                      box(htmltools::includeMarkdown("GoogleForm.Rmd"), width = 12),
                    #row  
                      fluidRow(
                        valueBoxOutput("n_Isolates", width = 3),
                        infoBoxOutput("n_Citations", width = 3),
                        infoBoxOutput("n_Distribution", width = 3),
                        infoBoxOutput("n_Hosts", width = 3)),
                    #tab box with first tab content
                    tabBox(title = "",
                           width = 12,
                           height = "100%",
                           tabPanel(icon = icon("disease"),
                                    "RSSC Visualizations",
                    #row  
                      fluidRow(
                        box(title = "Geographic Distribution of Reported RSSC Isolates",
                            solidHeader = T,
                            width = 12,
                            collapsible = T,
                            plotlyOutput("map_phylo", width="100%", height="400px"),
                            br(),
                            strong("How to Interpret this Map"),
                            p("This map shows the reported isolation locations of", em("Ralstonia"), "and 
                                    each datapoint should be regarded with healthy skepticism. Isolation of ", 
                                    em("Ralstonia"), "at a location does not mean it is currently established at that
                                    location; eradication has been successful in certain cases (e.g. in Sweden) 
                                    and some isolations might be from imported plants that were quarantined/destroyed. 
                                    Additionally, our meta-analysis database likely contains a low incidence of 
                                    errors from the primary literature, from our data entry, or from the geocoding 
                                    algorithm that assigned latitude/longitude coordinates to written locations."))),
                    # row
                      fluidRow(
                       # box for host species chart
                        box(title = "Phylotype Abundance by Host Species",
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            collapsed = F,
                            uiOutput("Host_chart_species"),
                            p("Plot by Count:"),
                            actionButton("host_linear_species","Top Host Species"),
                            actionButton("No_Unknown_host_linear_species","Remove Unknown Hosts"),
                            actionButton("No_Unknown_phylo_linear_species","Remove Unknown Phylotypes"),
                            br(),
                            br(),
                            p("Plot by Proportion:"),
                            actionButton("host_log_species","Top Host Species"),
                            actionButton("No_Unknown_host_log_species","Remove Unknown Hosts"),
                            actionButton("No_Unknown_phylo_log_species","Remove Unknown Phylotypes")),
                       # box for host family chart  
                        box(title = "Phylotype Abundance by Host Family",
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            collapsed = F,
                            uiOutput("Host_chart"),
                            p("Plot by Count:"),
                            actionButton("host_linear","Top Host Families"),
                            actionButton("No_Unknown_host_linear","Remove Unknown Hosts"),
                            actionButton("No_Unknown_phylo_linear","Remove Unknown Phylotypes"),
                            br(),
                            br(),
                            p("Plot by Proportion:"),
                            actionButton("host_log","Top Host Families"),
                            actionButton("No_Unknown_host_log","Remove Unknown Hosts"),
                            actionButton("No_Unknown_phylo_log","Remove Unknown Phylotypes"))),
                  # row
                    fluidRow(
                     # box for country chart  
                      box(title = "Phylotype Abundance by Country",
                          solidHeader = T,
                          width = 6,
                          collapsible = T,
                          collapsed = F,
                          uiOutput("Country_chart"),
                          p("Plot by Count:"),
                          actionButton("country_linear","Top Countries"),
                          actionButton("No_Unknown_country_linear","Remove Unknown Locations"),
                          actionButton("No_Unknown_phylo_country_linear","Remove Unknown Phylotypes"),
                          br(),
                          br(),
                          p("Plot by Proportion:"),
                          actionButton("country_log","Top Countries"),
                          actionButton("No_Unknown_country_log","Remove Unknown Locations"),
                          actionButton("No_Unknown_phylo_country_log","Remove Unknown Phylotypes")),
                     # box for continent chart
                      box(title = "Phylotype Abundance by Continent",
                          solidHeader = T,
                          width = 6,
                          collapsible = T,
                          collapsed = F,
                          uiOutput("Continent_chart"),
                          p("Plot by Count:"),
                          actionButton("continent_linear","Continents"),
                          actionButton("No_Unknown_continent_linear","Remove Unknown Locations"),
                          actionButton("No_Unknown_phylo_continent_linear","Remove Unknown Phylotypes"),
                          br(),
                          br(),
                          p("Plot by Proportion:"),
                          actionButton("continent_log","Continents"),
                          actionButton("No_Unknown_continent_log","Remove Unknown Locations"),
                          actionButton("No_Unknown_phylo_continent_log","Remove Unknown Phylotypes"))),
                    # row
                      fluidRow(
                       # box for metadata table
                        box(title = "Metadata Table",
                            solidHeader = T,
                            width = 12, 
                            collapsible = T,
                            collapsed = F,
                            DT::dataTableOutput("Metadata_table")))
                  ), #close of first tab panel
                    tabPanel(icon = icon("circle-info"),
                             "About Page",
                     #row
                      fluidRow(
                      #box for about page
                      box(htmltools::includeMarkdown("AboutPage.Rmd"),width = 12))
        ) #close of second tab panel
      ) #close of tab box
    ) #close of div
  ) #close of dashboard body
) #close of dashboard page

# server
server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    shinyjs::reset("publication_year")
    shinyjs::reset("isolation_year")
    shinyjs::reset("phylo")
    shinyjs::reset("sequevar")
    shinyjs::reset("pandemic_lineage")
    shinyjs::reset("host_family")
    shinyjs::reset("host_species")
    shinyjs::reset("vegprophost")
    shinyjs::reset("continent")
    shinyjs::reset("country")
    shinyjs::reset("genome")
  })
  
  filtered_PublicationYear_type <- eventReactive(input$search,{
    req(input$publication_year)
    RSSC1 %>%
    filter(`Year published` %in% input$publication_year)
  })
  
  filtered_IsolationYear_type <- eventReactive(input$search,{
    req(input$isolation_year)
    filtered_PublicationYear_type() %>%
    filter(`Year isolated` %in% input$isolation_year)
  })
  
  filtered_Phylotype_type <- eventReactive(input$search,{
    req(input$phylo)
    filtered_IsolationYear_type() %>%
    filter(Phylotype3 %in% input$phylo)
  })
  
  filtered_Sequevar_type <- eventReactive(input$search,{
    req(input$sequevar)
    filtered_Phylotype_type() %>%
    filter(Sequevar3 %in% input$sequevar)
  })
  
  filtered_PandemicLineage_type <- eventReactive(input$search,{
    req(input$pandemic_lineage)
    filtered_Sequevar_type() %>%
    filter(Sequevar2 %in% input$pandemic_lineage)
  })
  
  filtered_HostFamily_type <- eventReactive(input$search,{
    req(input$host_family)
    filtered_PandemicLineage_type() %>%
      filter(`Host Family` %in% input$host_family)
  })
  
  filtered_HostSpecies_type <- eventReactive(input$search,{
    req(input$host_species)
    filtered_HostFamily_type() %>%
    filter(`Host Species (Common name)` %in% input$host_species)
  })
  
  filtered_VegProp_type <- eventReactive(input$search,{
    req(input$vegprophost)
    filtered_HostSpecies_type() %>%
    filter(VPH %in% input$vegprophost)
  })
  
  filtered_Continent_type <- eventReactive(input$search,{
    req(input$continent)
    filtered_VegProp_type() %>%
    filter(`Location (continent)` %in% input$continent)
  })
  
  filtered_Country_type <- eventReactive(input$search,{
    req(input$country)
    filtered_Continent_type() %>%
    filter(`Location (Country or Territory)` %in% input$country)
  })
  
  filtered_Genome_type <- eventReactive(input$search,{
    req(input$genome)
    filtered_Country_type() %>%
    filter(Genome2 %in% input$genome)
  })
  
# Output ggplot map
    output$map_phylo <- renderPlotly({
      data_leaflet <- if (input$search == 0) {
        data_leaflet <- RSSC1
      } else {
        data_leaflet <- filtered_Genome_type()
      }
      
      p <- ggplot(data = world)+
          geom_sf(fill = "white", color = "darkgrey", linewidth = 0.25) +
          geom_jitter(data = data_leaflet %>% filter(Phylotype2 == "Unknown"), aes(x = Longitude2, y = Latitude2, color = Phylotype2), size = 0.25, alpha = 0.5) +
          geom_jitter(data = data_leaflet %>% filter(Phylotype2 != "Unknown"), aes(x = Longitude2, y = Latitude2, color = Phylotype2, 
                                                                                   text = paste0("</br> Strainname: ", Strainname,
                                                                                                 "</br> Phylotype: ", Phylotype,
                                                                                                 "</br> Location: ", `Location Isolated`)), size = 0.25, alpha = 0.5) +
          coord_sf(ylim = c(-70,90), expand = FALSE)+
          scale_y_continuous(breaks = c(-60, -40, -20, 0, 20, 40, 60, 80))+
          scale_x_continuous(breaks = c(-150, -120, -90, -60, -30, 0, 30, 60, 90, 120, 150))+
          labs(x = "Longitude", y = "Latitude", color = "Phylotype")+
          theme(panel.grid.major = element_line(color = "grey", linewidth = 0.5),
                panel.background = element_rect(fill = "lightgrey"),
                panel.border = element_rect(fill = NA))+
          scale_color_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))
      ggplotly(p, tooltip = "text") %>%
        config(p, displaylogo = FALSE, 
                  toImageButtonOptions = list(format= 'svg', scale= 1),
                  modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 'hoverCompareCartesian'))
    })

# Output Host Charts  
    
    # Set a default view on page load
    output$Host_chart <- renderUI({
      plotlyOutput("plot_linear")  # Default plot when the page loads
    })
    
   # Button Observations
    observeEvent(input$host_linear, { 
      req(input$host_linear)
      output$Host_chart <- renderUI({ plotlyOutput("plot_linear") })
    })
    observeEvent(input$host_log, { 
      req(input$host_log)
      output$Host_chart <- renderUI({ plotlyOutput("plot_log") })
    })
    observeEvent(input$No_Unknown_host_linear, {
      req(input$No_Unknown_host_linear)
      output$Host_chart <- renderUI({ plotlyOutput("No_Unknown_plot_linear") })
    })
    observeEvent(input$No_Unknown_host_log, { 
      req(input$No_Unknown_host_log)
      output$Host_chart <- renderUI({ plotlyOutput("No_Unknown_plot_log") })
    })
    observeEvent(input$No_Unknown_phylo_linear, { 
      req(input$No_Unknown_phylo_linear)
      output$Host_chart <- renderUI({ plotlyOutput("No_Unknown_phylo_linear") })
    })
    observeEvent(input$No_Unknown_phylo_log, { 
      req(input$No_Unknown_phylo_log)
      output$Host_chart <- renderUI({ plotlyOutput("No_Unknown_phylo_log") })
    })
    
# Output host family plots  
 # Output plot by count
      output$plot_linear = renderPlotly({
      data_leaflet <- if (input$search == 0) {
        data_leaflet <- RSSC1
        } else {
          data_leaflet <- filtered_Genome_type()
        }
        
      topten <- data_leaflet %>%
       count(`Host Family`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=10, with_ties = FALSE) %>%
        as.data.frame()
        
      host_phylo = data_leaflet %>%
        filter(`Host Family` %in% topten$`Host Family`) %>%
        group_by(`Host Family`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Family based on total count
        mutate(`Host Family` = fct_reorder(`Host Family`, count, .fun = sum)) %>%
        ggplot(aes(`Host Family`, count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Family",
             y = "Isolations Reported (#)",
             fill = "Phylotype",
             title = "Plotted by Count") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()
      
      ggplotly(host_phylo) %>%
        layout(
          yaxis = list(title = list(text = "Host Family", standoff = 10)),
          xaxis = list(title = list(text = "Isolations Reported (#)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
   
 # Output plot by proportion
    output$plot_log = renderPlotly({
      data_leaflet <- if (isolate(input$search) == 0) {
        data_leaflet <- RSSC1
      } else {
        data_leaflet <- filtered_Genome_type()
      }
      
      topten2 <- data_leaflet %>%
        count(`Host Family`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=10, with_ties = FALSE) %>%
        as.data.frame()
      
      host_phylo = data_leaflet %>% 
        filter(`Host Family` %in% topten2$`Host Family`) %>%
        group_by(`Host Family`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Family based on total count
        mutate(`Host Family` = fct_reorder(`Host Family`, count, .fun = sum)) %>%
        ggplot(aes(`Host Family`, count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Family",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype",
             title = "Plotted by Proportion") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()
      
      ggplotly(host_phylo) %>%
        layout(
          yaxis = list(title = list(text = "Host Family", standoff = 10)),
          xaxis = list(title = list(text = "Relative Reporting Frequency (%)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
   
 # Output plot by count no unknown host  
    output$No_Unknown_plot_linear = renderPlotly({
      data_leaflet <- if (isolate(input$search) == 0) {
        data_leaflet <- RSSC1
      } else {
        data_leaflet <- filtered_Genome_type()
      }
      
      topten3 <- data_leaflet %>%
        filter(`Host Family` != "Unknown") %>%
        count(`Host Family`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=10, with_ties = FALSE) %>%
        as.data.frame()
    
     host_phylo = data_leaflet %>%
        filter(`Host Family` != "Unknown") %>%
        filter(`Host Family` %in% topten3$`Host Family`) %>%
        group_by(`Host Family`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
       # Explicitly reorder Host Family based on total count
       mutate(`Host Family` = fct_reorder(`Host Family`, count, .fun = sum)) %>%
       ggplot(aes(`Host Family`, count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Family",
             y = "Isolations Reported (#)",
             fill = "Phylotype",
             title = "Count: Unknown Hosts Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
    
      ggplotly(host_phylo) %>%
        layout(
          yaxis = list(title = list(text = "Host Family", standoff = 10)),
          xaxis = list(title = list(text = "Isolations Reported (#)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })

 # Output plot by proportion no unknown host
    output$No_Unknown_plot_log = renderPlotly({
      data_leaflet <- if (isolate(input$search) == 0) {
        data_leaflet <- RSSC1
      } else {
        data_leaflet <- filtered_Genome_type()
      }
      
      topten4 <- data_leaflet %>%
        filter(`Host Family` != "Unknown") %>%
        count(`Host Family`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=10, with_ties = FALSE) %>%
        as.data.frame()
      
      host_phylo = data_leaflet %>%
        filter(`Host Family` != "Unknown") %>%
        filter(`Host Family` %in% topten4$`Host Family`) %>%
        group_by(`Host Family`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Family based on total count
        mutate(`Host Family` = fct_reorder(`Host Family`, count, .fun = sum)) %>%
        ggplot(aes(`Host Family`, count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Family",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype",
             title = "Proportion: Unknown Hosts Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()
      
      ggplotly(host_phylo) %>%
        layout(
          yaxis = list(title = list(text = "Host Family", standoff = 10)),
          xaxis = list(title = list(text = "Relative Reporting Frequency (%)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by count no unknown phylotype  
    output$No_Unknown_phylo_linear = renderPlotly({
      data_leaflet <- if (isolate(input$search) == 0) {
        data_leaflet <- RSSC1
      } else {
        data_leaflet <- filtered_Genome_type()
      }
      
      topten5 <- data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        count(`Host Family`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=10, with_ties = FALSE) %>%
        as.data.frame()
      
      host_phylo = data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        filter(`Host Family` %in% topten5$`Host Family`) %>%
        group_by(`Host Family`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Family based on total count
        mutate(`Host Family` = fct_reorder(`Host Family`, count, .fun = sum)) %>%
        ggplot(aes(`Host Family`, count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Family",
             y = "Isolations Reported (#)",
             fill = "Phylotype",
             title = "Count: Unknown Phylotypes Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(host_phylo) %>%
        layout(
          yaxis = list(title = list(text = "Host Family", standoff = 10)),
          xaxis = list(title = list(text = "Isolations Reported (#)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by proportion no unknown phylotypes
    output$No_Unknown_phylo_log = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topten6 <- data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        count(`Host Family`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=10) %>%
        as.data.frame()
      
      host_phylo = data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        filter(`Host Family` %in% topten6$`Host Family`) %>%
        group_by(`Host Family`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Family based on total count
        mutate(`Host Family` = fct_reorder(`Host Family`, count, .fun = sum)) %>%
        ggplot(aes(`Host Family`, count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Family",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype",
             title = "Proportion: Unknown Phylotypes Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()
      
      ggplotly(host_phylo) %>%
        layout(
          yaxis = list(title = list(text = "Host Family", standoff = 10)),
          xaxis = list(title = list(text = "Relative Reporting Frequency (%)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
# Output host Species Chart
 # Set a default view on page load
    output$Host_chart_species <- renderUI({
    plotlyOutput("plot_linear_species")  # Default plot when the page loads
    })
    
 # Button Observations
    observeEvent(input$host_linear_species, { 
      req(input$host_linear_species)
      output$Host_chart_species <- renderUI({ plotlyOutput("plot_linear_species") })
    })
    observeEvent(input$host_log_species, { 
      req(input$host_log_species)
      output$Host_chart_species <- renderUI({ plotlyOutput("plot_log_species") })
    })
    observeEvent(input$No_Unknown_host_linear_species, { 
      req(input$No_Unknown_host_linear_species)
      output$Host_chart_species <- renderUI({ plotlyOutput("No_Unknown_plot_linear_species") })
    })
    observeEvent(input$No_Unknown_host_log_species, { 
      req(input$No_Unknown_host_log_species)
      output$Host_chart_species <- renderUI({ plotlyOutput("No_Unknown_plot_log_species") })
    })
    observeEvent(input$No_Unknown_phylo_linear_species, { 
      req(input$No_Unknown_phylo_linear_species)
      output$Host_chart_species <- renderUI({ plotlyOutput("No_Unknown_phylo_linear_species") })
    })
    observeEvent(input$No_Unknown_phylo_log_species, { 
      req(input$No_Unknown_phylo_log_species)
      output$Host_chart_species <- renderUI({ plotlyOutput("No_Unknown_phylo_log_species") })
    })
    
# Output host species plots  
 # Output plot by count
    output$plot_linear_species = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topten7 <- data_leaflet %>%
        count(`Host Species`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=20) %>%
        as.data.frame()
      
      host_phylo_species = data_leaflet %>%
        filter(`Host Species` %in% topten7$`Host Species`) %>%
        group_by(`Host Species`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Family based on total count
        mutate(`Host Species` = fct_reorder(`Host Species`, count, .fun = sum)) %>%
        ggplot(aes(`Host Species`, count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Species",
             y = "Isolations Reported (#)",
             fill = "Phylotype",
             title = "Plotted by Count") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()
      
      ggplotly(host_phylo_species) %>%
        layout(
          yaxis = list(title = list(text = "Host Species", standoff = 10)),
          xaxis = list(title = list(text = "Isolations Reported (#)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by proportion
    output$plot_log_species = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topten8 <- data_leaflet %>%
        count(`Host Species`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=20) %>%
        as.data.frame()
      
      host_phylo_species = data_leaflet %>% 
        filter(`Host Species` %in% topten8$`Host Species`) %>%
        group_by(`Host Species`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Species based on total count
        mutate(`Host Species` = fct_reorder(`Host Species`, count, .fun = sum)) %>%
        ggplot(aes(`Host Species`, count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Species",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype",
             title = "Plotted by Proportion") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()
      
      ggplotly(host_phylo_species) %>%
        layout(
          yaxis = list(title = list(text = "Host Species", standoff = 10)),
          xaxis = list(title = list(text = "Relative Reporting Frequency (%)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by count no unknown host  
    output$No_Unknown_plot_linear_species = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      topten9 <- data_leaflet %>%
        filter(`Host Species` != "Unknown") %>%
        count(`Host Species`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=20) %>%
        as.data.frame()
      
      host_phylo_species = data_leaflet %>%
        filter(`Host Species` != "Unknown") %>%
        filter(`Host Species` %in% topten9$`Host Species`) %>%
        group_by(`Host Species`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Species based on total count
        mutate(`Host Species` = fct_reorder(`Host Species`, count, .fun = sum)) %>%
        ggplot(aes(`Host Species`, count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Species",
             y = "Isolations Reported (#)",
             fill = "Phylotype",
             title = "Count: Unknown Hosts Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(host_phylo_species) %>%
        layout(
          yaxis = list(title = list(text = "Host Species", standoff = 10)),
          xaxis = list(title = list(text = "Isolations Reported (#)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by proportion no unknown host
    output$No_Unknown_plot_log_species = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topten10 <- data_leaflet %>%
        filter(`Host Species` != "Unknown") %>%
        count(`Host Species`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=20) %>%
        as.data.frame()
      
      host_phylo_species = data_leaflet %>%
        filter(`Host Species` != "Unknown") %>%
        filter(`Host Species` %in% topten10$`Host Species`) %>%
        group_by(`Host Species`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Species based on total count
        mutate(`Host Species` = fct_reorder(`Host Species`, count, .fun = sum)) %>%
        ggplot(aes(`Host Species`, count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Species",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype",
             title = "Proportion: Unknown Hosts Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()
      
      ggplotly(host_phylo_species) %>%
        layout(
          yaxis = list(title = list(text = "Host Species", standoff = 10)),
          xaxis = list(title = list(text = "Relative Reporting Frequency (%)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by count no unknown phylotype  
    output$No_Unknown_phylo_linear_species = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      topten11 <- data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        count(`Host Species`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=20) %>%
        as.data.frame()
      
      host_phylo_species = data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        filter(`Host Species` %in% topten11$`Host Species`) %>%
        group_by(`Host Species`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Species based on total count
        mutate(`Host Species` = fct_reorder(`Host Species`, count, .fun = sum)) %>%
        ggplot(aes(`Host Species`, count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Species",
             y = "Isolations Reported (#)",
             fill = "Phylotype",
             title = "Count: Unknown Phylotypes Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(host_phylo_species) %>%
        layout(
          yaxis = list(title = list(text = "Host Species", standoff = 10)),
          xaxis = list(title = list(text = "Isolations Reported (#)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by proportion no unknown phylotypes
    output$No_Unknown_phylo_log_species = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topten12 <- data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        count(`Host Species`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=20) %>%
        as.data.frame()
      
      host_phylo_species = data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        filter(`Host Species` %in% topten12$`Host Species`) %>%
        group_by(`Host Species`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Species based on total count
        mutate(`Host Species` = fct_reorder(`Host Species`, count, .fun = sum)) %>%
        ggplot(aes(`Host Species`, count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Species",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype",
             title = "Prop.: Unknown Phylotypes Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()
      
      ggplotly(host_phylo_species) %>%
        layout(
          yaxis = list(title = list(text = "Host Species", standoff = 10)),
          xaxis = list(title = list(text = "Relative Reporting Frequency (%)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
# Output Continent Chart    
    # Set a default view on page load
    output$Continent_chart <- renderUI({
      plotlyOutput("cont_linear")  # Default plot when the page loads
    })
    # Button Observations
    observeEvent(input$continent_linear, { 
      req(input$continent_linear)
      output$Continent_chart <- renderUI({ plotlyOutput("cont_linear") })
    })
    observeEvent(input$continent_log, { 
      req(input$continent_log)
      output$Continent_chart <- renderUI({ plotlyOutput("cont_log") })
    })
    observeEvent(input$No_Unknown_continent_linear, { 
      req(input$No_Unknown_continent_linear)
      output$Continent_chart <- renderUI({ plotlyOutput("No_Unknown_cont_linear") })
    })
    observeEvent(input$No_Unknown_continent_log, { 
      req(input$No_Unknown_continent_log)
      output$Continent_chart <- renderUI({ plotlyOutput("No_Unknown_cont_log") })
    })
    observeEvent(input$No_Unknown_phylo_continent_linear, { 
      req(input$No_Unknown_phylo_continent_linear)
      output$Continent_chart <- renderUI({ plotlyOutput("No_Unknown_phylo_cont_linear") })
    })
    observeEvent(input$No_Unknown_phylo_continent_log, { 
      req(input$No_Unknown_phylo_continent_log)
      output$Continent_chart <- renderUI({ plotlyOutput("No_Unknown_phylo_cont_log") })
    })
    
 # Output plot by count    
    output$cont_linear = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topcont <- data_leaflet %>%
        count(`Location (continent)`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=7) %>%
        as.data.frame()
      
      cont_phylo = data_leaflet %>% 
        filter(`Location (continent)` %in% topcont$`Location (continent)`) %>%
        group_by(`Location (continent)`,Phylotype2) %>%  
        summarise(count = n()) %>% 
        ggplot(aes(reorder(`Location (continent)`,count), count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Continent",
             y = "Isolations Reported (#)",
             fill = "Phylotype",
             title = "Plotted by Count") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(cont_phylo) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by proportion  
    output$cont_log = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topcont2 <- data_leaflet %>%
        count(`Location (continent)`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=7) %>%
        as.data.frame()
      
      cont_phylo = data_leaflet %>%
        filter(`Location (continent)` %in% topcont2$`Location (continent)`) %>%
        group_by(`Location (continent)`,Phylotype2) %>%  
        summarise(count = n()) %>% 
        ggplot(aes(reorder(`Location (continent)`,count), count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Continent",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype",
             title = "Plotted by Proportion") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(cont_phylo) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by count no unknown location
    output$No_Unknown_cont_linear = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topcont3 <- data_leaflet %>%
        filter(`Location (continent)` != "Unknown") %>%
        count(`Location (continent)`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=7) %>%
        as.data.frame()
      
      cont_phylo = data_leaflet %>% 
        filter(`Location (continent)` != "Unknown") %>%
        filter(`Location (continent)` %in% topcont3$`Location (continent)`) %>%
        group_by(`Location (continent)`,Phylotype2) %>%  
        summarise(count = n()) %>% 
        ggplot(aes(reorder(`Location (continent)`,count), count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Continent",
             y = "Isolations Reported (#)",
             fill = "Phylotype",
             title = "Count: Unknown Locations Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(cont_phylo) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by proportion no unknown location 
    output$No_Unknown_cont_log = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topcont4 <- data_leaflet %>%
        filter(`Location (continent)` != "Unknown") %>%
        count(`Location (continent)`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=7) %>%
        as.data.frame()
      
      cont_phylo = data_leaflet %>% 
        filter(`Location (continent)` != "Unknown") %>%
        filter(`Location (continent)` %in% topcont4$`Location (continent)`) %>%
        group_by(`Location (continent)`,Phylotype2) %>%  
        summarise(count = n()) %>% 
        ggplot(aes(reorder(`Location (continent)`,count), count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Continent",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype",
             title = "Proportion: Unknown Locations Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(cont_phylo) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by count no unknown phylotype
    output$No_Unknown_phylo_cont_linear = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topcont5 <- data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        count(`Location (continent)`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=7) %>%
        as.data.frame()
      
      cont_phylo = data_leaflet %>% 
        filter(Phylotype2 != "Unknown") %>%
        filter(`Location (continent)` %in% topcont5$`Location (continent)`) %>%
        group_by(`Location (continent)`,Phylotype2) %>%  
        summarise(count = n()) %>% 
        ggplot(aes(reorder(`Location (continent)`,count), count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Continent",
             y = "Isolations Reported (#)",
             fill = "Phylotype",
             title = "Count: Unknown Phylotypes Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(cont_phylo) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by proportion no unknown phylotype 
    output$No_Unknown_phylo_cont_log = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topcont6 <- data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        count(`Location (continent)`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=7) %>%
        as.data.frame()
      
      cont_phylo = data_leaflet %>% 
        filter(Phylotype2 != "Unknown") %>%
        filter(`Location (continent)` %in% topcont6$`Location (continent)`) %>%
        group_by(`Location (continent)`,Phylotype2) %>%  
        summarise(count = n()) %>% 
        ggplot(aes(reorder(`Location (continent)`,count), count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Continent",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype",
             title = "Proportion: Unknown Phylotypes Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(cont_phylo) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })

# Output Country Chart
    output$Country_chart <- renderUI({
      plotlyOutput("country_linear")  # Default plot when the page loads
    })
  # Button Observations
    observeEvent(input$country_linear, { 
      req(input$country_linear)
      output$Country_chart <- renderUI({ plotlyOutput("country_linear") })
    })
    observeEvent(input$country_log, { 
      req(input$country_log)
      output$Country_chart <- renderUI({ plotlyOutput("country_log") })
    })
    observeEvent(input$No_Unknown_country_linear, { 
      req(input$No_Unknown_country_linear)
      output$Country_chart <- renderUI({ plotlyOutput("No_Unknown_country_linear") })
    })
    observeEvent(input$No_Unknown_country_log, { 
      req(input$No_Unknown_country_log)
      output$Country_chart <- renderUI({ plotlyOutput("No_Unknown_country_log") })
    })
    observeEvent(input$No_Unknown_phylo_country_linear, { 
      req(input$No_Unknown_phylo_country_linear)
      output$Country_chart <- renderUI({ plotlyOutput("No_Unknown_phylo_country_linear") })
    })
    observeEvent(input$No_Unknown_phylo_country_log, { 
      req(input$No_Unknown_phylo_country_log)
      output$Country_chart <- renderUI({ plotlyOutput("No_Unknown_phylo_country_log") })
    })
  
# Output plot by count
    output$country_linear = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topten13 <- data_leaflet %>%
        count(`Location (Country or Territory)`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=20) %>%
        as.data.frame()
      
      country_phylo = data_leaflet %>%
        filter(`Location (Country or Territory)` %in% topten13$`Location (Country or Territory)`) %>%
        group_by(`Location (Country or Territory)`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Country based on total count
        mutate(`Location (Country or Territory)` = fct_reorder(`Location (Country or Territory)`, count, .fun = sum)) %>%
        ggplot(aes(`Location (Country or Territory)`, count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Country",
             y = "Isolations Reported (#)",
             fill = "Phylotype",
             title = "Plotted by Count") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()
      
      ggplotly(country_phylo) %>%
        layout(
          yaxis = list(title = list(text = "Host Country", standoff = 10)),
          xaxis = list(title = list(text = "Isolations Reported (#)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by proportion
    output$country_log = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topten14 <- data_leaflet %>%
        count(`Location (Country or Territory)`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=20) %>%
        as.data.frame()
      
      country_phylo = data_leaflet %>% 
        filter(`Location (Country or Territory)` %in% topten14$`Location (Country or Territory)`) %>%
        group_by(`Location (Country or Territory)`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Country based on total count
        mutate(`Location (Country or Territory)` = fct_reorder(`Location (Country or Territory)`, count, .fun = sum)) %>%
        ggplot(aes(`Location (Country or Territory)`, count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Country",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype",
             title = "Plotted by Proportion") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()
      
      ggplotly(country_phylo) %>%
        layout(
          yaxis = list(title = list(text = "Host Country", standoff = 10)),
          xaxis = list(title = list(text = "Relative Reporting Frequency (%)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by count no unknown location  
    output$No_Unknown_country_linear = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      topten15 <- data_leaflet %>%
        filter(`Location (Country or Territory)` != "Unknown") %>%
        count(`Location (Country or Territory)`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=20) %>%
        as.data.frame()
      
      country_phylo = data_leaflet %>%
        filter(`Location (Country or Territory)` != "Unknown") %>%
        filter(`Location (Country or Territory)` %in% topten15$`Location (Country or Territory)`) %>%
        group_by(`Location (Country or Territory)`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Country based on total count
        mutate(`Location (Country or Territory)` = fct_reorder(`Location (Country or Territory)`, count, .fun = sum)) %>%
        ggplot(aes(`Location (Country or Territory)`, count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Country",
             y = "Isolations Reported (#)",
             fill = "Phylotype",
             title = "Count: Unknown Locations Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(country_phylo) %>%
        layout(
          yaxis = list(title = list(text = "Host Country", standoff = 10)),
          xaxis = list(title = list(text = "Isolations Reported (#)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by proportion no unknown location
    output$No_Unknown_country_log = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topten16 <- data_leaflet %>%
        filter(`Location (Country or Territory)` != "Unknown") %>%
        count(`Location (Country or Territory)`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=20) %>%
        as.data.frame()
      
      country_phylo = data_leaflet %>%
        filter(`Location (Country or Territory)` != "Unknown") %>%
        filter(`Location (Country or Territory)` %in% topten16$`Location (Country or Territory)`) %>%
        group_by(`Location (Country or Territory)`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Country based on total count
        mutate(`Location (Country or Territory)` = fct_reorder(`Location (Country or Territory)`, count, .fun = sum)) %>%
        ggplot(aes(`Location (Country or Territory)`, count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Country",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype",
             title = "Prop.: Unknown Locations Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()
      
      ggplotly(country_phylo) %>%
        layout(
          yaxis = list(title = list(text = "Host Country", standoff = 10)),
          xaxis = list(title = list(text = "Relative Reporting Frequency (%)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
# Output plot by count no unknown phylotype  
    output$No_Unknown_phylo_country_linear = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      topten17 <- data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        count(`Location (Country or Territory)`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=20) %>%
        as.data.frame()
      
      country_phylo = data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        filter(`Location (Country or Territory)` %in% topten17$`Location (Country or Territory)`) %>%
        group_by(`Location (Country or Territory)`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Family based on total count
        mutate(`Location (Country or Territory)` = fct_reorder(`Location (Country or Territory)`, count, .fun = sum)) %>%
        ggplot(aes(`Location (Country or Territory)`, count, fill = Phylotype2))+
        geom_col()+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Country",
             y = "Isolations Reported (#)",
             fill = "Phylotype",
             title = "Count: Unknown Phylotypes Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()  
      
      ggplotly(country_phylo) %>%
        layout(
          yaxis = list(title = list(text = "Host Country", standoff = 10)),
          xaxis = list(title = list(text = "Isolations Reported (#)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
    })
    
 # Output plot by proportion no unknown phylotypes
    output$No_Unknown_phylo_country_log = renderPlotly({
      if(input$search == 0){
        data_leaflet = RSSC1
      }else{
        data_leaflet = filtered_Genome_type()
      }
      
      topten18 <- data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        count(`Location (Country or Territory)`, sort = T, name = "myCount") %>%
        slice_max(myCount, n=20) %>%
        as.data.frame()
      
      country_phylo = data_leaflet %>%
        filter(Phylotype2 != "Unknown") %>%
        filter(`Location (Country or Territory)` %in% topten18$`Location (Country or Territory)`) %>%
        group_by(`Location (Country or Territory)`,Phylotype2) %>%  
        summarise(count = n(), .groups = "drop") %>%
        # Explicitly reorder Host Country based on total count
        mutate(`Location (Country or Territory)` = fct_reorder(`Location (Country or Territory)`, count, .fun = sum)) %>%
        ggplot(aes(`Location (Country or Territory)`, count, fill = Phylotype2))+
        geom_bar(position="fill", stat="identity")+
        theme_minimal_vgrid(font_size = 10)+
        scale_fill_manual(values = c("I" = "#ffaf37", "II" = "#007ba5", "III" = "#f24000", "IV" = "#00b67e", "Unknown" = "grey50"))+
        labs(x = "Host Country",
             y = "Relative Reporting Frequency (%)",
             fill = "Phylotype",
             title = "Prop.: Unknown Phylotypes Removed") +
        theme(panel.background = element_rect(color = "gray"),
              legend.position = "bottom")+
        coord_flip()
      
      ggplotly(country_phylo) %>%
        layout(
          yaxis = list(title = list(text = "Host Country", standoff = 10)),
          xaxis = list(title = list(text = "Relative Reporting Frequency (%)"))
        ) %>%
        style(hoverinfo = 'none') %>%
        config(p, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', scale= 1),
               modeBarButtonsToRemove = c('autoScale', 'lasso2d', 'select', 
                                          'hoverCompareCartesian', 'hoverClosestCartesian',
                                          'zoom', 'pan', 'zoomIn', 'zoomOut', 'resetScale'))
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
                      `Host Family`, `Year isolated`, `Location Isolated`, `Genome Accession`, Publication)
    },options = list(autoWidth = F,autoHeight = F, scrollX = TRUE))  
  
# Output download filtered data button    
  output$downloadfiltered <- downloadHandler(filename = function(){"RSSCdb_filtered_data.csv"}, 
                                             content = function(fname){
                                               if(input$search == 0){
                                                 data_leaflet = RSSC1
                                                 }else{
                                                   data_leaflet = filtered_Genome_type()
                                                   }
                                               write.csv(data_leaflet, fname)
                                               })
  
# Output download entire data button    
  output$downloadentire <- downloadHandler(filename = function(){"RSSCdb_data.csv"},
                                           content = function(fname){
                                             if(input$search == 0){
                                               data_leaflet = RSSC1
                                               }else{
                                                 data_leaflet = RSSC1
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
            color= "light-blue",
            icon = icon("bacteria"))
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
            color= "light-blue",
            icon = icon("newspaper"))
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
            color= "light-blue",
            icon = icon("earth-americas"))
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
            color= "light-blue",
            icon = icon("plant-wilt"))
  })
  
}

shinyApp(ui, server)