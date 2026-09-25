##### Boxwood blight risk mapping app for western Oregon and Washington #####
# Purpose: display outputs forecasts of 3-day, 4-day, and cumulative  risk 
# Designed to run from day 1 of year through 4 days past current date
# For example, if today is 8/1/23, then runs for 1/1/23 to 8/5/23

options(shiny.sanitize.errors = FALSE)

# Last update: Sep 2026 - CARTO key for map tiles, made maps faster by 
# adjusting RiskMap code (factorization), and added labels plus base map with
# a z-score to have layers (base, raster, labels, etc.)

## Setup ----

# Packages
library(tidyverse) # Data wrangling/manipulation
library(terra) # Import model outputs / work with rasters
library(raster) # TO DO: hopefully can remove this
library(sf) # Spatial features
library(mapview) # Open access street maps
library(tigris) # County and state boundaries
library(leafem) # Query map values
library(leaflet) # Interactive maps
library(leaflegend) # Extra legend features
library(leaflet.extras) 
library(lubridate) # Working with dates
library(tidygeocoder) # Obtain coordinates from address
library(shiny) # Web app 
library(shinyWidgets)
library(shinydashboard)
library(shinyBS) # Info tabs next to risk map menu items
library(shinycssloaders) # "Loading" animation for risk maps (waiting)
library(shinyjs) # For "delay" function to causes error messages to disappear
library(bslib)
library(fresh) # Color theme for web app page
library(htmlwidgets)

# Last update - 11/2/23 
# Updated code so that last year map wouldn't zoom if coords fell outside W OR/WA

## Setup ----

carto_key <- Sys.getenv("CARTO_KEY")
mapquest_key <- Sys.getenv("MAPQUEST_API_KEY")

if (!nzchar(carto_key)) {
  stop("CARTO_KEY is not set.")
}

if (!nzchar(mapquest_key)) {
  stop("MAPQUEST_API_KEY is not set.")
}
#Sys.setenv(MAPQUEST_API_KEY = "5vjLXIpEjMHpANFr4Ok2BNxpuQPrsGQP")

#### * Dates 
# Used in map titles
# Current dates and year
current_date <- Sys.Date()
current_year <- as.numeric(format(current_date, format = "%Y"))
#current_year <- 2021
#current_date <- as.Date(paste0("Jun-11-", current_year), format = "%b-%d-%Y") 
last_year <- current_year - 1

# Must deal with leap day or app will crash
# Get data for March 1 for last year if today is a leap day
if (current_year %% 4 == 0) {
#if (current_year == as.Date(paste0(current_year, "-02-29"))) {
  lastYr_date <- as.Date(gsub(current_year, last_year, current_date + 1))
} else {
  lastYr_date <- as.Date(gsub(current_year, last_year, current_date))
}

# Spatial features to add to map (all have CRS = WGS 84)
# State boundaries
state_sf <- st_read("./features/states_OR_WA.shp")

# County boundaries
county_sf <- st_read("./features/counties_OR_WA.shp") 

# Functions ----

# Function that removes leading 0s in map title dates
DateFormat <- function(dat) {
  str_glue("{month(dat)}/{day(dat)}/{year(dat)}")
}

# Function to import outputs (rasters)
# Raster with total cumulative DDs has multiple layers so need only last layer, 
# which corresponds to cumulative DDs on last sampling day (= 4 days from current date)
# Rasters with 3- and 4-day risk have only 1 layer
RastImport <- function(file_name) {
  rast_stack <- rast(file_name)
  
  # Use the final layer
  rast <- rast_stack[[nlyr(rast_stack)]]
  
  crs(rast) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  return(rast)
}

# Function to factorize a raster (translate numbers to a category)
FactorizeRast <- function(r) {
  
  # Round to the model's 0.5 risk increments
  r <- round(r * 2) / 2
  
  # Convert continuous risk values to categorical values
  r[r >= 2] <- 4
  r[r == 1.5] <- 3
  r[r == 1] <- 2
  r[r == 0.5] <- 1
  r[r < 0.5] <- 0
  
  return(r)
}
# Produce a leaflet map showing risk of infection
RiskMap <- function(rast, pal, map_title, lgd_title, unique_vals, last_year) {
  
  rast_cat <- FactorizeRast(rast)
  
  if (last_year == 1) {
    layerID <- "Value (last year)"
    
    map <- leaflet(
      options = leafletOptions(
        attributionControl = FALSE,
        zoomControl = FALSE,
        dragging = FALSE,
        doubleClickZoom = FALSE,
        touchZoom = FALSE,
        boxZoom = FALSE,
        scrollWheelZoom = FALSE,
        minZoom = 6
      )
    )
    
  } else {
    layerID <- "Value"
    
    map <- leaflet(
      options = leafletOptions(
        attributionControl = FALSE,
        zoomControl = FALSE,
        minZoom = 6
      )
    ) %>%
      htmlwidgets::onRender(
        "function(el, x) {
          L.control.zoom({ position: 'topright' }).addTo(this)
        }"
      )
  }
  
map <- map %>%
  
  # Basemap without labels
  addTiles(
    urlTemplate = paste0(
      "https://basemaps.cartocdn.com/rastertiles/light_nolabels/",
      "{z}/{x}/{y}{r}.png?key=", carto_key
    ),
    attribution = paste0(
      "&copy; <a href='https://www.openstreetmap.org/copyright'>",
      "OpenStreetMap</a> contributors, ",
      "&copy; <a href='https://carto.com/attribution/'>CARTO</a>"
    ),
    options = tileOptions(maxZoom = 20)
  ) %>%
  
  # Risk raster
  addRasterImage(
    rast_cat,
    colors = pal,
    opacity = 0.65,
    group = layerID,
    layerId = layerID
  ) %>%
  
  # Labels on top of raster
  addTiles(
    urlTemplate = paste0(
      "https://basemaps.cartocdn.com/rastertiles/light_only_labels/",
      "{z}/{x}/{y}{r}.png?key=", carto_key
    ),
    options = tileOptions(
      maxZoom = 20,
      pane = "overlayPane"
    )
  ) %>%
  
  # State boundaries
  addPolylines(
    data = state_sf,
    group = "States",
    opacity = 0.25,
    color = "black",
    weight = 1.75
  ) %>%
  
  # County boundaries
  addPolylines(
    data = county_sf,
    group = "Counties",
    opacity = 0.15,
    color = "black",
    weight = 1.25
  ) %>%
  
  setMaxBounds(
    lng1 = -127,
    lat1 = 41.966,
    lng2 = -120.5,
    lat2 = 49.1664
  ) %>%
  
  addControl(
    map_title,
    position = "bottomleft",
    className = "map-title"
  ) %>%
  
  addLegend(
    position = "bottomright",
    colors = pal,
    labels = unique_vals,
    title = lgd_title,
    opacity = 0.65
  )

  # Raster value display - current year map only
  if (last_year == 0) {
    map <- map %>%
      addImageQuery(
        raster(rast),
        project = TRUE,
        prefix = "",
        digits = 2,
        layerId = layerID,
        position = "topleft",
        type = "mousemove"
      )
  }
  return(map)
}

# Import and process model outputs ----

# File names
fls <- c("Cum_Inf_Risk_1day.tif", "Cum_Inf_Risk_2day.tif","Cum_Inf_Risk_3day.tif", "Cum_Inf_Risk_4day.tif")
outdir_current <- paste0("./rasters/today_maps/Misc_output")
outdir_lastYr <- paste0("./rasters/today_maps/Misc_output")
#outdir_current <- "/srv/shiny-server/boxb/rasters/today_maps/Misc_output"
#outdir_lastYr <-  "/srv/shiny-server/boxb/rasters/today_lastYr_maps/Misc_output"

# Model outputs for current run 
rasts_current <- map(
  fls, function(file_name) {
    RastImport(paste0(outdir_current, "/", file_name))
  }
)

# Model outputs for same time last year
rasts_lastYr <- map(
  fls, function(file_name) {
    RastImport(paste0(outdir_lastYr, "/", file_name))
  }
)

# Custom map title CSS specs ----
# border-radius makes rounded edges
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    width: 130px;
    padding-left: 3px; 
    padding-right: 3px; 
    padding-top: 2px; 
    padding-bottom: 2px;
    border-radius: 2px;
    background: rgba(255,255,255,.75);
    font-size: 16px;
    font-weight: bold;
    text-align: left;
    color: rgb(51, 51, 51);
  }
"))

# TO DO: Figure out way to control leaflet legend background opacity
# border-radius makes rounded edges
# tag.map.legend <- tags$style(HTML("
#   .leaflet-control.legend { 
#     background: rgba(255,255,255,.75);
#   }
# "))

# UI: User interface ------

#### * Color themes ####
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_global(
    content_bg = "#FAFCFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

#### * Fluid page layout ####
ui <- fluidPage(
  
  useShinyjs(), # For making error message disappear with "delay"
  
  tags$style(".container-fluid {
             background-color: #d4ddc0;
             }"),
  
  #  Title and windowTitle (this is not a dashboard header, which has a dropdown menu)
  titlePanel(div("Boxwood Blight Risk Mapping Tool for Western OR and WA",
                 style = "color: black; 
                 font-size: 28px; 
                 font-weight: bold;
                 margin-left: 15px;
                 font-family: 'Source Sans Pro', 'Helvetica Neue', Helvetica, Arial, sans-serif;"),
             windowTitle = "Boxwood Blight Risk Mapping"),
  
  dashboardPage(
    
    # Dashboard header disabled (used titlePanel instead)
    dashboardHeader(disable = TRUE),
    
    # No dashboard sidebar 
    dashboardSidebar(minified = FALSE, disable = TRUE, collapsed = TRUE),
    
    # Main body of dashboard
    dashboardBody(
      
      use_theme(mytheme),
      
      tags$head(tags$style(HTML(".popover-title{ font-weight: bold;}"))),
      
      # Background info 
      fluidRow(
        style = "font-size:19px;",
        box(title = strong("Overview", style = "font-size:22px"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            color = "light-blue",
            fluidRow(
              style = "font-size:19px;",
              column(width = 12, 
                     offset = 0, 
                     p("The boxwood blight infection risk mapping tool produces forecasts of the risk of boxwood being infected by boxwood blight in western Oregon and Washington. This information may help with planning scouting activities and with efforts to prevent or mitigate infections (e.g., with fungicide treatments). Forecasts are available for each day between tomorrow and four days from today. Climate data are derived from the", a(href = "https://www.prism.oregonstate.edu", "PRISM", target = "_blank", style="text-decoration-line: underline;"), "database at a 800 m", tags$sup(2, .noWS = "before"), " resolution and from the", a(href = "https://vlab.noaa.gov/web/mdl/ndfd", "NDFD", target = "_blank", style = "text-decoration-line: underline;"), "database (downscaled from a 2.5 km", tags$sup(2, .noWS = "before"), "to an 800 m", tags$sup(2, .noWS = "before"), "resolution). Presently models are run only for areas west of the Cascades (approximately west of \u2013120.5\u00B0W). Please see a", a(href = "BOXB_webapp_tutorial.pdf", "tutorial", target = "_blank", style="text-decoration-line: underline;"), "for details on tool use and map interpretation. Expand the Introduction below to learn more about boxwood blight and risk models for this disease."))))),
      
      # Background info 
      fluidRow(
        style = "font-size:19px;",
        box(title = strong("About", style = "font-size:22px"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            color = "light-blue",
            fluidRow(
              style = "font-size:19px;",
              column(width = 2, align = "center", style='padding:0px;font-size:14px;',
                     img(src = "boxb-infected-shrubs2.png", width = "155px", style = "max-height: 240px;"),
                     img(src = "boxb-infected-leaves2.png", width = "155px", style = "max-height: 240px;"),
                     img(src = "boxb-infected-stems2.png", width = "160px", style = "max-height: 240px;")),
              column(width = 10, offset = 0, 
                     p(strong("Introduction: "), "Boxwood blight caused by the fungus ", em("Calonectria pseudonaviculata"), " can result in defoliation, decline, and death of susceptible varieties of boxwood, including most varieties of ", em("Buxus sempervirens"), " such as \u0022Suffruticosa\u0022  (English boxwood) and \u0022Justin Brouwers\u0022. Images show diagnostic symptoms of boxwood blight including", strong("(A)"),  "defoliation,", strong("(B)"), "leaf spots, and", strong("(C)"), "black streaks on stems (courtesy of Chuan Hong). The fungus has been detected at several locations (mostly in nurseries) in at least six different counties in Oregon and is thought to be established in some areas. Previous", a(href = "https://doi.org/10.3390/biology11060849", "research", target = "_blank", style="text-decoration-line: underline;"), "indicates that western Oregon and Washington have highly suitable climates for establishment of", em("C. pseudonaviculata"),  ". Tools are therefore needed to inform growers and gardeners about when environmental conditions are conducive to boxwood blight infection and establishment."),
                     p("Generally, it should be very humid or raining and at moderately warm temperatures (60\u201385\u00B0F) for a couple days for boxwood blight infection risk to be high. An inoculum source must be present nearby for infection to occur. Overhead irrigation facilitates outbreaks because it creates higher relative humidity and exposes leaf surfaces to longer periods of leaf wetness. For more information on preventing and managing boxwood blight, see the ", a(href = " https://pnwhandbooks.org/plantdisease/host-disease/boxwood-buxus-spp-boxwood-blight", "Pacific Northwest Pest Management Handbook", target = "_blank", style="text-decoration-line: underline;"), " and a ", a(href = " https://www.pubs.ext.vt.edu/content/dam/pubs_ext_vt_edu/PPWS/PPWS-29/PPWS-29-pdf.pdf", "publication", target = "_blank", style="text-decoration-line: underline;"),"by Virginia Cooperative Extension."),
                     p(strong("Tool description: "), "The risk mapping tool is similar to the ", a(href = "https://uspest.org/risk/boxwood_app", "boxwood blight model app", target = "_blank", style="text-decoration-line: underline;"), "and the", a(href = "https://uspest.org/risk/boxwood_map", "synoptic map-view of risk", target = "_blank", style="text-decoration-line: underline;"), "available at", a(href = "https://uspest.org", "USPest.org", target = "_blank", style="text-decoration-line: underline;"), "except that it uses daily gridded climate data instead of hourly climate data from single weather stations.  The spatial model is run using a modified version of a platform known as", a(href = "https://uspest.org/CAPS/", "DDRP", target = "_blank", .noWS = "after", style="text-decoration-line: underline;"), ", which provides real-time forecasts of phenology and establishment risk of 16 species of invasive insects in the contiguous US. It will likely need to be fine-tuned as more infection incidence data become available. Technical information on the station-based (hourly) model can be found at", a(href = "https://uspest.org/wea/Boxwood_blight_risk_model_summaryV3.pdf", "USPest.org", target = "_blank", .noWS = "after", style="text-decoration-line: underline;"),"."),
                     p(strong("Suggested citation: "), "Barker, B. S., and L. Coop. 2023. Boxwood blight risk mapping app for western Oregon and Washington. Oregon IPM Center, Oregon State University.", a(href = "https://riskmaps.oregonstate.edu/boxb/", "https://riskmaps.oregonstate.edu/boxb/", .noWS = c("after"), style="text-decoration-line: underline;"), "."),
                     p(strong("Source code and feedback: "), "To view the source code, visit the", a(href = "https://github.com/bbarker505/boxb-webapp", "GitHub repository", target = "_blank", .noWS = c("after"), style="text-decoration-line: underline;"), ". To report bugs or provide feedback, please e-mail Brittany Barker at", a(href = "mailto:brittany.barker@oregonstate.edu", "brittany.barker@oregonstate.edu", .noWS = c("after"), style="text-decoration-line: underline;"), "."),
                     p(strong("Disclaimer: "), "The risk index is intended to inform your decisions about management actions, such as choice and timing of control measures and intensity of scouting. It should supplement, not replace, the other factors you consider in making these decisions. Use at your own risk."))))),
      
      # Risk map menu
      fluidRow(
        style = "font-size:19px;",
        box(title = strong("Risk Map", style = "font-size:22px"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            color = "light-blue",
            ## Risk map selection
            fluidRow(style = "font-size:19px;",
                     column(width = 1),
                     column(
                       width = 3,
                       offset = 0,
                       radioButtons("risk",
                                    label = tags$span("Select risk map", 
                                                      bsButton("info_maptype", 
                                                               label = "", 
                                                               icon = icon("info"), 
                                                               style = "info", 
                                                               size = "extra-small")),
                                    choices = c("One Day", "Two Day", "Three Day", "Four Day"),
                                    selected = "Three Day"),
                       # Popover info box
                       shinyBS::bsPopover(
                         id = "info_maptype",
                         title = "Select risk map",
                         content = paste0("Risk maps show forecasts of infection risk for ", DateFormat(current_date + 1), ", ", DateFormat(current_date + 2), ", ", DateFormat(current_date + 3), ", and ",  DateFormat(current_date + 4), ", respectively. Possible levels of infection risk are: Very Low Risk, Low Risk, 1st Infection of Susceptible Varieties, Up to 1-6 Lesions, and Up to 5-18 Lesions. Hover the mouse over a location on the map to see a numerical risk value."),
                         placement = "right",
                         trigger = "hover",
                         options = list(container = "body"))),
                     ## Address selection and entry
                     column(
                       width = 3,
                       offset = 0,
                       checkboxInput("address_checkbox",
                                     value = FALSE,
                                     label = tags$span("Locate an address",
                                                       bsButton("info_address",
                                                                label = "",
                                                                icon = icon("info"),
                                                                style = "info",
                                                                size = "extra-small"))),
                       
                       # Popover info box
                       shinyBS::bsPopover(
                         id = "info_address",
                         title = "Locate an address",
                         content = "Marks, zooms to, and extracts risk information for a specified location. After checking the box, enter a city name, address, or place in the text box and press Submit.",
                         placement = "right",
                         trigger = "hover",
                         options = list(container = "body")),
                       # Conditional panel of address box is selected
                       conditionalPanel(
                         condition = "input.address_checkbox == 1",
                         textInput("address", "Enter an address, city, or place",
                                   value = ""),
                         actionButton("address_submit", "Submit"))),
                     column(
                       width = 3, 
                       checkboxInput("lastYr_checkbox",
                                     value = FALSE,
                                     label = tags$span("Compare to last year",
                                                       bsButton("info_lastYr",
                                                                label = "",
                                                                icon = icon("info"),
                                                                style = "info",
                                                                size = "extra-small"))),
                       shinyBS::bsPopover(
                         id = "info_lastYr",
                         title = "Compare to last year",
                         content = "Produces a second map that shows risk for the same time last year. Comparing th current year vs. last year map may provide insight into how climate differences between years affects infection risk. The two risk maps are synced, so panning and zooming one map will do the same for the other.",
                         placement = "right",
                         trigger = "hover",
                         options = list(container = "body"))),
            )),
        
        # Risk map value for geocoded address
        fluidRow(style="padding-left:15px;margin-top:1em;font-size:19px",
                 conditionalPanel(
                   condition = "input.address_checkbox == 1",
                   # The "zooming to location" message is not fast enough
                   #column(style ="color:#000000;",
                   #     width = 12, uiOutput("search_message")),
                   column(style ="color:#f56954;",
                          width = 12, uiOutput("error_message")))),
        
        # Risk maps
        # For map heights or the entire region won't be shown (cuts off So. OR)
        tags$style(type = "text/css", "#riskmap1 {height: calc(500px) !important;}"),
        tags$style(type = "text/css", "#riskmap2 {height: calc(500px) !important;}"),
        fluidRow(style = "padding-left:15px;padding-right:15px;",
                 column(width = 6, 
                        leafletOutput("riskmap1") %>% withSpinner(color="#0dc5c1")),
                 conditionalPanel(
                   condition =  "input.lastYr_checkbox == 1", 
                   column(width = 6, leafletOutput("riskmap2") 
                          %>% withSpinner(color="#0dc5c1")))),
        
        
        # Legend title
        fluidRow(style = "margin-bottom:0em;padding-left:15px;padding-top:5px",
                 column(width = 6, style = "font-size:22px;margin-bottom:-0.5em;", align = "center", p(strong("Legend"))),
        ),
        
        # Legend
        fluidRow(style = "font-size:19px;margin-top:0em;padding-left:15px;",
                 column(width = 7, align = "align-items:center; justify-content: center;", style = "font-size:16px;padding-left:20px;",
                        column(width = 2, align = "center",  p(strong(div(icon("fa-solid fa-square", style = "background-color:#DCFFE6;color:#656565;")), HTML(paste("0:", "Very Low Risk", sep="<br/>"))))),
                        column(width = 2, align = "center",  p(strong(div(icon("fa-solid fa-square", style = "background-color:#B0FFB0;color:#656565;")), HTML(paste("1:", "Low Risk", sep = "<br/>"))))),
                        column(width = 2, align = "center", p(strong(div(icon("fa-solid fa-square", style = "background-color:#FAFFD0;color:#656565;")), HTML(paste("2:", "1st Infec. Susc. Vars.", sep = "<br/>"))))),
                        column(width = 2, align = "center", p(strong(div(icon("fa-solid fa-square", style = "background-color:#FFE9A6;color:#656565;")), HTML(paste("3:", "Up to 1\u20136 Lesions", sep = "<br/>"))))),
                        column(width = 2, align = "center",  p(strong(div(icon("fa-solid fa-square", style = "background-color:#FFD0E6;color:#656565;")), HTML(paste("4:", "Up to 5\u201318 Lesions", sep = "<br/>")))))
                 )),
        
        # Acknowledgements
        fluidRow(style="padding-left:15px;margin-top:1.5em;margin-right:5px;font-size:19px;",
                 box(title = strong("Acknowledgements", style = "font-size:22px"),
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     collapsed = FALSE,
                     width = 12,
                     color = "light-blue",
                     fluidRow(
                       style = "font-size:19px;padding:0px;",
                       column(width = 12, offset = 0, 
                              p("Creation of this app was funded by", a(href = "https://www.oregon.gov/ODA", "Oregon Department of Agriculture", target = "_blank", style="text-decoration-line: underline;"), "Nursery Research Grant no. ODA-4310-A. Boxwood blight risk modeling work was funded by the US Farm Bill FY17,", a(href = "https://www.aphis.usda.gov/aphis/", "USDA APHIS PPQ", target = "_blank", style="text-decoration-line: underline;"), "agreement number 20-8130-0282-CA, and", a(href = "https://www.nifa.usda.gov/grants/programs/crop-protection-pest-management-program", "USDA NIFA CPPM", target = "_blank", style="text-decoration-line: underline;"), "Extension Implementation Program grant no. 2021-70006-35581. Many collaborators from OSU, USDA ARS, Virginia Tech, and North Carolina State University helped improve the model."))))),
        
        # Logos
        fluidRow(
          column(width = 3, align = "center", offset = 0,
                 img(src = "OIPMC.png", width = "75%", style = "max-width: 200px;")),
          column(width = 3, align = "center", offset = 0,
                 img(src = "Oregon-Department-of-Agriculture-logo.png", width = "75%", style = "max-width: 200px;")),
          column(width = 3, align = "center", offset = 0,
                 img(src = "PRISM.png", width = "55%", style = "max-width: 200px;")),
          column(width = 3,
                 align = "center", offset = 0,
                 img(src = "usda-logo_original.png", width = "45%", style = "max-width: 200px;max-height: 100px;")))))))

# Server ----
server <- function(input, output, session) {
  
  
  #### * Import rasters ####
  # Different maps are rendered depending on radioButton inputs
  observeEvent(input$risk, {
    
    #### * Import rasters ####
    
    # Rasters for this year
    raster_current <- switch(
      input$risk,
      "One Day" = rasts_current[[1]],
      "Two Day" = rasts_current[[2]],
      "Three Day" = rasts_current[[3]],
      "Four Day" = rasts_current[[4]]
    )
    
    # Rasters for last year
    raster_lastYr <- switch(
      input$risk,
      "One Day" = rasts_lastYr[[1]],
      "Two Day" = rasts_lastYr[[2]],
      "Three Day" = rasts_lastYr[[3]],
      "Four Day" = rasts_lastYr[[4]]
    )
    
    # FIX ME: why are extents suddenly different
    if (ncol(raster_lastYr) < ncol(raster_current)) {
      raster_current <- raster_current[1:nrow(raster_current), 1:507, drop = FALSE]
      raster_current <- crop(raster_current, raster_lastYr)
    }
    
    #### * Map titles with dates ####
    
    title_current <- switch(
      input$risk,
      "One Day" = paste0(DateFormat(current_date), " \u2013", DateFormat(current_date + 1)),
      "Two Day" = paste0(DateFormat(current_date), " \u2013", DateFormat(current_date + 2)),
      "Three Day" = paste0(DateFormat(current_date), " \u2013", DateFormat(current_date + 3)),
      "Four Day" = paste0(DateFormat(current_date), " \u2013", DateFormat(current_date + 4))
    )
    
    title_current <- tags$div(tag.map.title, HTML(title_current))
    
    title_lastYr <- switch(
      input$risk,
      "One Day" = paste0(DateFormat(lastYr_date), " \u2013", DateFormat(lastYr_date + 1)),
      "Two Day" = paste0(DateFormat(lastYr_date), " \u2013", DateFormat(lastYr_date + 2)),
      "Three Day" = paste0(DateFormat(lastYr_date), " \u2013", DateFormat(lastYr_date + 3)),
      "Four Day" = paste0(DateFormat(lastYr_date), " \u2013", DateFormat(lastYr_date + 4))
    )
    
    title_lastYr <- tags$div(tag.map.title, HTML(title_lastYr))
    
    #### * Define categorical rasters for palette/legend only ####
    
    # Convert to categorical rasters only for map display
    raster_current_cat <- FactorizeRast(raster_current)
    raster_lastYr_cat <- FactorizeRast(raster_lastYr)
    
    # Combine rasters to determine all categories present
    both_rasters_cat <- c(raster_current_cat, raster_lastYr_cat)
    max_rast <- app(both_rasters_cat, max)
    
    # Number of categories present
    vals <- sort(unique(values(max_rast)))
    vals <- vals[!is.na(vals)]
    ncols <- min(length(vals), 5)
    
    # Risk palette
    pal_risk <- c("#DCFFE6", "#B0FFB0", "#FAFFD0", "#FFE9A6","#FFD0E6")
    pal_risk <- pal_risk[1:ncols]
    
    # Risk labels
    risk_labels <- c(
      "0: Very Low Risk",
      "1: Low Risk",
      "2: 1st Infec. Susc. Vars.",
      "3: Up to 1-6 Lesions",
      "4: Up to 5-18 Lesions"
    )
    
    # Labels for categories actually present
    vals_current <- sort(unique(values(raster_current_cat)))
    vals_current <- vals_current[!is.na(vals_current)]
    
    vals_lastYr <- sort(unique(values(raster_lastYr_cat)))
    vals_lastYr <- vals_lastYr[!is.na(vals_lastYr)]
    
    unique_vals_current <- risk_labels[vals_current + 1]
    unique_vals_lastYr <- risk_labels[vals_lastYr + 1]
    
    pal_risk_current <- pal_risk[1:length(unique_vals_current)]
    pal_risk_lastYr <- pal_risk[1:length(unique_vals_lastYr)]
    
    #### * Legend title ####
    
    lgd_title <- switch(
      input$risk,
      "One Day" = "One Day Risk",
      "Two Day" = "Two Day Risk",
      "Three Day" = "Three Day Risk",
      "Four Day" = "Four Day Risk"
    )
    
    #### * Render Leaflet maps ####
    
    output$riskmap1 <- renderLeaflet({
      
      RiskMap(
        raster_current, 
        pal_risk_current, 
        title_current,
        lgd_title, 
        unique_vals_current, 
        last_year = 0
      ) %>%
        fitBounds(
          lng1 = -127,
          lat1 = 41,
          lng2 = -120.5,
          lat2 = 49.1664
        )
    })
    
    output$riskmap2 <- renderLeaflet({
      
      RiskMap(
        raster_lastYr, 
        pal_risk_lastYr, 
        title_lastYr,
        lgd_title, 
        unique_vals_lastYr, 
        last_year = 1
      )
    })
    
    #### * Re-do last year map so bounds match current year map ####
    
    observeEvent(input$riskmap1_bounds, {
      
      bounds <- input$riskmap1_bounds
      
      if (!is.null(bounds)) {
        
        output$riskmap2 <- renderLeaflet({
          
          RiskMap(
            raster_lastYr, 
            pal_risk_lastYr, 
            title_lastYr,
            lgd_title, 
            unique_vals_lastYr, 
            last_year = 1
          ) %>%
            fitBounds(
              bounds$west,
              bounds$south,
              bounds$east,
              bounds$north
            )
        })
      }
      
    }, once = TRUE)
    
    #### * Bounds: current year map ####
    
    observeEvent(input$riskmap1_bounds, {
      
      bounds <- input$riskmap1_bounds
      mapzoom <- input$riskmap1_zoom
      
      if (mapzoom > 6) {
        
        leafletProxy("riskmap1") %>%
          fitBounds(
            bounds$west,
            bounds$south,
            bounds$east,
            bounds$north
          )
      }
    })
    
    #### * Maps with location ####
    
    observeEvent(input$address_submit, {
      
      location <- input$address
      
      coords <- tribble(~addr, location) %>%
        geocode(addr, method = "mapquest")
      
      output$error_message <- renderText({
        
        if (coords$addr == "") {
          
          "Please enter a location."
          
        } else if (is.na(coords$lat & coords$addr != "")) {
          
          "Sorry, this location could not be geocoded."
          
        } else if (!is.na(coords$lat)) {
          
          xy <- data.frame(
            x = coords$long,
            y = coords$lat
          )
          
          rast_val <- terra::extract(raster_current, xy)[1, 2]
          
          if (is.na(rast_val)) {
            "No risk forecast for this location."
          }
        }
      })
      
      delay(4000, output$error_message <- renderText(""))
      
      if (!is.na(coords$lat)) {
        
        if (
          coords$lat > 41.7 &
          coords$lat < 49.1664 &
          coords$long > -127 &
          coords$long < -120.5
        ) {
          
          output$search_message <- renderText({
            "Zooming to location"
          })
          
          delay(2000, output$search_message <- renderText(""))
          
          leafletProxy("riskmap1") %>%
            removeMarker(layerId = "Address marker") %>%
            addCircleMarkers(
              lat = coords$lat,
              lng = coords$long,
              opacity = 0.75,
              color = "blue",
              weight = 3,
              layerId = "Value",
              fill = FALSE
            ) %>%
            setView(
              lng = coords$long,
              lat = coords$lat,
              zoom = 11
            )
        }
      }
      
      observeEvent(input$lastYr_checkbox, {
        
        bounds <- input$riskmap1_bounds
        
        if (!is.na(coords$lat)) {
          
          if (
            coords$lat > 41.700 &
            coords$lat < 49.1664 &
            coords$long > -127 &
            coords$long < -120.5
          ) {
            
            output$riskmap2 <- renderLeaflet({
              
              RiskMap(
                raster_lastYr,
                pal_risk_lastYr,
                title_lastYr,
                lgd_title,
                unique_vals_lastYr,
                last_year = 1
              ) %>%
                fitBounds(
                  bounds$west,
                  bounds$south,
                  bounds$east,
                  bounds$north
                )
            })
          }
        }
        
      }, once = TRUE)
    })
    
    #### * Maps with no location ####
    
    observeEvent(input$address_checkbox, {
      
      if (input$address_checkbox == 0) {
        updateTextInput(
          session = session,
          inputId = "address",
          value = ""
        )
        
        leafletProxy("riskmap1") %>%
          fitBounds(
            lng1 = -127,
            lat1 = 41.7,
            lng2 = -120.5,
            lat2 = 49.1664
          ) %>%
          removeMarker(layerId = "Value")
        
        leafletProxy("riskmap2") %>%
          fitBounds(
            lng1 = -127,
            lat1 = 41.7,
            lng2 = -120.5,
            lat2 = 49.1664
          ) %>%
          removeMarker(layerId = "Value")
      }
    })
  })
  
  #### * Sync last year map ####
  # Must be outside of "observeEvent' for risk or won't always work correctly
  # Also must use different name for bounds object than above
  observe({
    bounds2 <- input$riskmap1_bounds
    if (!is.null(bounds2)) {
      leafletProxy("riskmap2") %>% 
        fitBounds(bounds2$west, bounds2$south, bounds2$east, bounds2$north)
    }
  })
  
}

# Run app ----
shinyApp(ui = ui, server = server)
