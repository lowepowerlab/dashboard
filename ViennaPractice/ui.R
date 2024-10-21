#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

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

# load data from local CSV file
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


ui <- dashboardPage(skin = "black",
  
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
        #        list("Vegetatively Propagated Hosts" = list("Potato" = "RSSC$`Host Species`~RSSC$`Solanum tuberosum`", 
        #                                                      "Banana/Plantain = "Musa", 
         #                                                     "Ginger/Turmeric" = "Zingiber", 
          #                                                    "Geranium" = "G",
          #                                                     "Pothos" = "p",
           #                                                    "Anthurium" = "a",
            #                                                   "Rose" = "r")),
             #                                          multiple = T
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
      # br(),
      div(style="display:inline-block;width:60%;text-align: center;",
          downloadButton("download",
                         "Get Data"))
         )
      ),

  dashboardBody(use_theme(mytheme),
    #row 
    shinyjs::useShinyjs(),
    div(id = "my app",
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
          leafletOutput("Isolate_map",
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