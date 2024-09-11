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
      nav_panel("Home", "Page A content"), 
      nav_panel("Global", "Page B content"), 
      nav_panel("Continent", "Page C content"), 
      nav_panel("Country", "Page C content"),
      nav_menu( 
        "Other links", 
        nav_panel("D", "Panel D content"), 
        "----", 
        "Description:", 
        nav_item( 
          a("Shiny", href = "https://shiny.posit.co", target = "_blank") 
        ), 
      ), 
    ), 
    id = "tab" 
)
