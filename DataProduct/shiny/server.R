library(shiny)
library(leaflet)
library(datasets)
library(dplyr)

data("quakes")

shinyServer(function(input, output) {
        output$mymap <- renderLeaflet({
                depth_seq <- seq(from = input$depth[1], to = input$depth[2], by = 1)
                mag_seq <- seq(from = input$mag[1], to = input$mag[2], by = 0.1)
                quakes <- filter(quakes, stations >= input$stations, 
                               depth %in% depth_seq, mag %in% mag_seq)
                map <- leaflet(height = 650, width = 900)
                map <- addTiles(map, group = "OpenStreetMap")
                map <- addProviderTiles(map, provider = "MapQuestOpen.Aerial", group = "MapQuestOpen")
                map <- addCircles(map,
                                  lng = quakes$long,
                                  lat = quakes$lat,
                                  radius = 5 ^ quakes$mag,
                                  popup = paste0('Magnitude: ', quakes$mag),
                                  group = "Magnitude",
                                  fillColor = "green",
                                  fill = TRUE,
                                  stroke = FALSE,
                                  fillOpacity = 0.5
                )
                map <- addCircles(map,
                                  lng = quakes$long,
                                  lat = quakes$lat,
                                  radius = quakes$depth ^ 1.2,
                                  popup = paste0('Depth: ', quakes$depth, ' km'),
                                  group = "Depth",
                                  fillColor = "Red",
                                  fill = TRUE,
                                  stroke = FALSE,
                                  fillOpacity = 0.5
                )
                map <- addCircles(map,
                                  lng = quakes$long,
                                  lat = quakes$lat,
                                  radius = quakes$stations ^ 1.5,
                                  popup = paste0('Number of stations: ', quakes$stations),
                                  group = "Number of stations",
                                  fillColor = "Yellow",
                                  fill = TRUE,
                                  stroke = FALSE,
                                  fillOpacity = 0.5
                )
                map <- addLayersControl(map, baseGroups = c("OpenStreetMap",
                                                            "MapQuestOpen"),
                                        overlayGroups = c("Magnitude","Depth","Number of Stations"))
        })
})



