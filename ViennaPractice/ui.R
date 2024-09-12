#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("*Ralstonia solanacearum* Species Complex (RSSC) Dashboard"),

    # Navset with options
    navset_pill_list( 
      nav_panel("Home", "The *Ralstonia solanacearum* Species Complex (RSSC)"), 
      nav_panel("Global", "Global Browser"), 
      nav_panel("Continent", "Continent Browser"), 
      nav_panel("Country", "Country Browser"),
      nav_panel("Phylotype", "Phylotype Browser"),
      nav_panel("Host", "Host Browser"),
      nav_menu( 
        "Other Resources",
        nav_item( 
          a("Biorxiv 2023", href = "https://www.biorxiv.org/content/10.1101/2020.07.13.189936v4.full", target = "_blank") 
        ), 
      ), 
    ), 
    id = "tab" 
)
