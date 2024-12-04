library(shiny)
library(dplyr)
library(leaflet)
library(reactable)
library(stringr)
library(MetBrewer)
library(shinyalert)
library(purrr)

# Load data
site_coords <- readRDS("data/site_coords.rds")

# Calculate maximum biomass for each site-year combination
site_coords <- site_coords %>%
  group_by(year) %>%
  mutate(max_biomass = max(biomass, na.rm = TRUE)) %>%
  mutate(scaled_biomass = biomass/max_biomass) %>%
  select(-max_biomass) %>%
  ungroup()


full_spp_list <- readRDS("data/hakai_full_spp_list.rds")
site_spp_list <- readRDS("data/hakai_site_spp_list.rds")


pal_habitat <- colorFactor(palette = c(met.brewer("Johnson", 4)), domain = site_coords$dominant_habitat)
pal_fish <- colorNumeric(palette = "viridis", domain = site_coords$fish_richness, na.color = "transparent")
pal_invert <- colorNumeric(palette = "viridis", domain = site_coords$invert_richness, na.color = "transparent")


# Define server logic
shinyServer(function(input, output, session) {
  
  # Update the year sliderInput choices based on the available years in the data
  observe({
    updateSliderInput(session, "year", 
                      min = min(site_coords$year, na.rm = TRUE), 
                      max = max(site_coords$year, na.rm = TRUE), 
                      value = max(site_coords$year, na.rm = TRUE))
  })
  
  # Reactive expression to filter data based on the user input
  filtered_data <- reactive({
    req(input$year, input$season)
    site_coords %>%
      filter(year == input$year & season == input$season)
  })
  
  # Reactive radius calculation based on the scaleByBiomass input
  point_radius <- reactive({
    data <- filtered_data()
    if (input$scaleByBiomass) {
      return(data$biomass*0.003)
    } else {
      return(rep(5, nrow(data)))  # Default radius if not scaling by biomass
    }
  })
  
  # Reactive expression to get sites containing all selected species
  sites_containing_spp <- reactive({
    req(input$selectspp)
    selected_species <- input$selectspp
    
    # Check if all selected species are in any site
    sites_with_species <- names(site_spp_list)[sapply(site_spp_list, function(site) {
      all(selected_species %in% site$species)
    })]
    
    # Debug print statement
    print(paste("Sites containing species", paste(selected_species, collapse = ", "), ":", paste(sites_with_species, collapse = ", ")))
    
    return(sites_with_species)
  })
  
  # Render map
  output$map <- renderLeaflet({
    data <- filtered_data()
    leaflet(options = leaflet::leafletOptions()) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lng = -128.50, lat = 51.75, zoom = 10) %>%
      addMapPane("yellow outlines", zIndex = 600) %>%
      addMapPane("habitat points", zIndex = 500) %>%
      addMapPane("fish points", zIndex = 450) %>%
      addMapPane("invert points", zIndex = 400) %>%
      addCircleMarkers(data = data,
                       lat = ~lat,
                       lng = ~long,
                       weight = 0,
                       radius = ~point_radius(),
                       layerId = ~sample,
                       fillColor = "white",
                       color = "black",
                       popup = ~paste0("<b>Site: </b>", site, "<br>
                                       <b>Habitat: </b>", dominant_habitat, "<br>
                                       <b>Average Kelp Density: </b>", mean_density , " stipe or frond/m2", "<br>
                                       <b>Month: </b>", month, "<br>
                                       <b>Year: </b>", year, "<br>
                                       <b>Total Fish Species: </b>", fish_richness, "<br>
                                       <b>Total Invertebrate Species: </b>", invert_richness),
                       fillOpacity = .7,
                       group = "all_sites") %>%
      
      addCircleMarkers(data = data,
                       lat = ~lat,
                       lng = ~long,
                       weight = 0,
                       radius = ~point_radius(),
                       fillColor = ~pal_habitat(dominant_habitat),
                       color = "black",
                       popup = ~paste0("<b>Site: </b>", site, "<br>
                                       <b>Habitat: </b>", dominant_habitat, "<br>
                                       <b>Average Kelp Density: </b>", mean_density , " stipe or frond/m2", "<br>
                                       <b>Month: </b>", month, "<br>
                                       <b>Year: </b>", year, "<br>
                                       <b>Total Fish Species: </b>", fish_richness, "<br>
                                       <b>Total Invertebrate Species: </b>", invert_richness),
                       fillOpacity = .7,
                       options = markerOptions(interactive = FALSE, pane = "habitat points"),
                       group = "habitat") %>%
      hideGroup("habitat") %>%
      
      addCircleMarkers(data = data,
                       lat = ~lat,
                       lng = ~long,
                       weight = 0,
                       radius = ~point_radius(),
                       fillColor = ~pal_fish(fish_richness),
                       color = "black",
                       popup = ~paste0("<b>Site: </b>", site, "<br>
                                       <b>Habitat: </b>", dominant_habitat, "<br>
                                       <b>Average Kelp Density: </b>", mean_density , " stipe or frond/m2", "<br>
                                       <b>Month: </b>", month, "<br>
                                       <b>Year: </b>", year, "<br>
                                       <b>Total Fish Species: </b>", fish_richness, "<br>
                                       <b>Total Invertebrate Species: </b>", invert_richness),
                       fillOpacity = .7,
                       options = markerOptions(interactive = FALSE, pane = "fish points"),
                       group = "fish_richness") %>%
      hideGroup("fish_richness") %>%
      
      addCircleMarkers(data = data,
                       lat = ~lat,
                       lng = ~long,
                       weight = 0,
                       radius = ~point_radius(),
                       fillColor = ~pal_invert(invert_richness),
                       color = "black",
                       popup = ~paste0("<b>Site: </b>", site, "<br>
                                       <b>Habitat: </b>", dominant_habitat, "<br>
                                       <b>Average Kelp Density: </b>", mean_density , " stipe or frond/m2", "<br>
                                       <b>Month: </b>", month, "<br>
                                       <b>Year: </b>", year, "<br>
                                       <b>Total Fish Species: </b>", fish_richness, "<br>
                                       <b>Total Invertebrate Species: </b>", invert_richness),
                       fillOpacity = .7,
                       options = markerOptions(interactive = FALSE, pane = "invert points"),
                       group = "invert_richness") %>%
      hideGroup("invert_richness") 
    
    
  })  # end render leaflet
  
  myLeafletProxy <- leafletProxy(mapId = "map", session)
  
  # add habitat layer when group button is clicked
  observe({
    req("habitat" %in% input$colorCode)
    leafletProxy("map") %>%
      hideGroup(c("fish_richness","invert_richness")) %>%
      clearControls() %>%
      showGroup("habitat") %>%
      leaflet::addLegend(data = site_coords,
                         pal = pal_habitat,
                         title = paste0("Habitat"),
                         values = ~dominant_habitat,
                         layerId = "dominant_habitat",
                         opacity = 1)
  })
  
  # add fish_richness layer when group button is clicked
  observe({
    req("fish_richness" %in% input$colorCode)
    leafletProxy("map") %>%
      hideGroup(c("habitat","invert_richness")) %>%
      clearControls() %>%
      showGroup("fish_richness") %>%
      leaflet::addLegend(data = site_coords,
                         pal = pal_fish,
                         title = paste0("Fish Richness"),
                         values = ~fish_richness,
                         layerId = "fish_richness",
                         opacity = 1)
  })
  
  # add invert_richness layer when group button is clicked
  observe({
    req("invert_richness" %in% input$colorCode)
    leafletProxy("map") %>%
      hideGroup(c("habitat","fish_richness")) %>%
      clearControls() %>%
      showGroup("invert_richness") %>%
      leaflet::addLegend(data = site_coords,
                         pal = pal_invert,
                         title = paste0("Invertebrate Richness"),
                         values = ~invert_richness,
                         layerId = "invert_richness",
                         opacity = 1)
  })
  
  # when button is default, remove all others options
  observe({
    req("default" %in% input$colorCode)
    leafletProxy("map") %>%
      clearControls() %>%
      hideGroup(c("habitat","fish_richness","invert_richness"))
  })
  
  # Highlight sites with the selected species
  observeEvent(input$selectspp, {
    leafletProxy("map") %>%
      clearGroup("selected_sites")
    
    selected_sites <- filtered_data()[filtered_data()$sample %in% sites_containing_spp(), ]
    # Debug print statement for selected sites
    print(selected_sites)
    
    if (nrow(selected_sites) > 0) {
      leafletProxy("map") %>%
        addCircleMarkers(data = selected_sites,
                         lat = ~lat,
                         lng = ~long,
                         weight = 3,
                         radius = 5,
                         fillColor = 'transparent',
                         color = "yellow",
                         opacity = 1,
                         fillOpacity = .8,
                         options = markerOptions(interactive = FALSE, pane = "yellow outlines"),
                         group = "selected_sites")
    } else {
      print("No sites contain all selected species.")
    }
  }, ignoreNULL = FALSE)
  
  
  
  
  # Track clicked dot
  # Make an empty reactive object for the clicked dot
  data <- reactiveValues(clickedMarker = NULL)
  
  # When a marker is clicked, store the marker ID
  observeEvent(input$map_marker_click, {
    data$clickedMarker <- input$map_marker_click
    print(data$clickedMarker)  # Debug: Print the clicked marker details
  })
  
  # When the map is clicked, clear the clicked marker
  observeEvent(input$map_click, {
    data$clickedMarker <- NULL
  })
  
  # Create a table to display survey information on the clicked dot
  output$clickInfo <- renderReactable({
    req(data$clickedMarker)
    
    # Retrieve the sample ID from the clicked marker
    clicked_sample <- data$clickedMarker$id
    print(paste("Clicked sample ID:", clicked_sample))  # Debug: Print the clicked sample ID
    
    # Retrieve the data for the clicked sample from site_spp_list
    site_data <- site_spp_list[[clicked_sample]]
    print(site_data)  # Debug: Print the site data
    
    if (is.null(site_data)) {
      print("Site data is NULL.")  # Debug: Print if site data is NULL
      return(NULL)
    }
    
    bio_columns <- site_data %>%
      select(Group, species, Abundance, `Average Size (cm)`)
    print(bio_columns)  # Debug: Print the bio columns
    
    if (nrow(bio_columns) == 0) {
      print("No bio columns data.")  # Debug: Print if no bio columns data
      return(NULL)
    }
    
    columns <- c("Group", "species", "Abundance") %>%
      set_names() %>%
      purrr::map(~ colDef(
        name = .x,
        minWidth = 100,
        align = "center"
      ))
    
    columns <- as.list(columns)
    
    reactable(bio_columns,
              defaultExpanded = TRUE,
              pagination = FALSE,
              compact = TRUE,
              borderless = TRUE,
              groupBy = "Group",
              columns = modifyList(
                list(
                  Group = colDef(minWidth = 70),
                  species = colDef(name = "Species", minWidth = 150, html = TRUE)
                ), columns),
              rowStyle = function(index) {
                if (bio_columns[index, "species"] %in% input$selectspp) {
                  list(backgroundColor = scales::alpha("yellow", .5), fontWeight = "bold")
                }
              },
              theme = reactableTheme(
                cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")
              )
    )
  })
})
