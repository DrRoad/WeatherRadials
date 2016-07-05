library(httr)
library(jsonlite)
library(leaflet)
library(reshape2)
library(shiny)
library(sp)

shinyUI(fluidPage(sidebarLayout(
  sidebarPanel(
    h3("Click on a site"),
    leafletOutput("leafmap"),
    h3("Choose a year"),
    selectInput("year", "Year:", 
                choices=2015),
    actionButton("getdat","Vizualize")
  ),
  mainPanel(
    plotOutput("plt",width="600px",height="600px")
  )

)))
