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

ui <- dashboardPage(skin = "black",
  
  dashboardHeader(title = "RSSC Db", titleWidth = 250),
  
  dashboardSidebar(collapsed = F, width = 250,
     br(),
     div(style = "display:inline-block;width:80%;margin-left:18px;text-align: left;",
     "A georeferenced database of isolates of the", em("Ralstonia solanacearum"), "Species Complex.
     Use the filters below to refine your search, visualize data, and download metadata."),
    sidebarMenu(id = "sidebarid",
                 
        # filter drop-down menus
      selectInput(inputId = "Publication_Year",
                label = "Publication Year",
                choices = unique(RSSC$`Year published`),
                multiple = T
                ),
      selectInput(inputId = "Phylotype",
                label = "Phylotype",
                choices = unique(RSSC$Phylotype),
                
                selected = unique(RSSC$Phylotype),
                multiple = T
                ),  
      selectInput(inputId = "Host_Species",
                label = "Host Species",
                choices = unique(RSSC$`Host Species (Common name)`),
                multiple = T
                ), 
      selectInput(inputId = "Host_Family",
                label = "Host Family",
                choices = unique(RSSC$`Host Family`),
                multiple = T
                ), 
      #selectInput(inputId = "VegProp_Host",
       #         label = "Select Vegetatively Propagated Hosts)",
        #        list("Potato" = "st", "Musa spp." = "m", "Ginger spp." = "Z", "Tumeric spp." = "t", "Geranium spp." = "g", "Pothos spp." = "p", "Anthurium spp." = "a", "Rose spp." = "r",),
         #       multiple = T
          #      ), 
      selectInput(inputId = "Country",
                label = "Country or Territory",
                choices = unique(RSSC$`Location (Country or Territory)`),
                multiple = T
                ),
      selectInput(inputId = "Continent",
              label = "Continent",
              choices = unique(RSSC$`Location (continent)`),
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
     #fluidRow(
            #valueBoxOutput("Isolates", width = 3),
            #infoBoxOutput("Citations", width = 3),
            #infoBoxOutput("Distribution", width = 3)
            #infoBoxOutput("Hosts", width = 3)
            #),
    #row
    fluidRow(
        box(title = "Isolate Map",
          solidHeader = T,
          width = 12,
          collapsible = T,
          #plotOutput("Isolate_map")
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
