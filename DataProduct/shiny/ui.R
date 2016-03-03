library(shiny)
library(leaflet)
library(markdown)

shinyUI(navbarPage("Locations of Earthquakes off Fiji",
                   tabPanel("Map",
                            # Sidebar
                            sidebarLayout(
                                    sidebarPanel(
                                            helpText("Locations of Earthquakes off Fiji"),
                                            numericInput('stations', 'Number of Stations:', 50, min = 10, max = 132),
                                            sliderInput('depth', 'Depth (km)', min=40, max=680, value=c(40,680), step=10),
                                            sliderInput('mag', 'Magnitude', min=4, max=6.4, value=c(4,6.4), step=0.1)
                                    ),
                                    mainPanel(
                                            leafletOutput("mymap")
                                    )
                            )
                   ),
                   tabPanel("About",
                            mainPanel(
                                    includeMarkdown("about.md")
                            )
                   )
)
)   