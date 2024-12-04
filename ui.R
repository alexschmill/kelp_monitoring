# KELP MONITORING PROGRAM

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(reactable)
library(leaflet)
library(ggplot2)
library(ggtext)
library(shinyjs)


site_coords <- readRDS("data/site_coords.rds")
full_spp_list <- readRDS("data/hakai_full_spp_list.rds")
site_spp_list <- readRDS("data/hakai_site_spp_list.rds")

# Dashboard header (disabled)
header <- dashboardHeader(disable = TRUE)

# Dashboard sidebar (disabled)
sidebar <- dashboardSidebar(disable = TRUE)

# Dashboard body
body <- dashboardBody(
  useShinyjs(),
  tags$head(includeCSS(path = "www/style.css")),
  
  leafletOutput("map", width = "100%", height = "100vh"),
  
  absolutePanel(
    id = "controls",
    class = "panel panel-default",
    fixed = TRUE,
    draggable = TRUE,
    top = 10, left = 60, right = "auto", bottom = "auto",
    width = 550, height = "auto",  
    style = "overflow-y: scroll; max-height: 95vh;",  # Add scrollability and max height
    
    h2("HAKAI KELP MONITORING PROGRAM"),
    
    sliderInput("year", "Select Year:", 
                min = min(site_coords$year, na.rm = TRUE), 
                max = max(site_coords$year, na.rm = TRUE), 
                value = max(site_coords$year, na.rm = TRUE),
                step = 1,
                sep = ""),
    radioButtons("season", "Select Season:", 
                 choices = c("Spring" ,"Summer", "Winter"), 
                 selected = "Summer"),
    pickerInput(
      inputId = "selectspp",
      label = "Species Select", 
      choices = full_spp_list,
      multiple = TRUE,
      options = pickerOptions(liveSearch = TRUE),
      choicesOpt = list(content = full_spp_list)
    ),
    
    radioGroupButtons(
      inputId = "colorCode", label = "Color Points",
      choices = list("Default" = "default", "Habitat" = "habitat", "Fish Richness" = "fish_richness", "Invertebrate Richness" = "invert_richness"),
      selected = "default"
    ),
    
    checkboxInput("scaleByBiomass", "Scale Points by Biomass", value = TRUE),
    
    p("Select a site on the map for species detected"),
    reactableOutput("clickInfo", height = '400px')
  )
)

shinyUI(fluidPage(
  tags$head(tags$style(HTML("
                            #controls {
                              background-color: rgba(255,255,255,0.8);
                              padding: 20px;
                              border-radius: 10px;
                            }
                            "))),
  header,
  body
))


