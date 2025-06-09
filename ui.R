ui = dashboardPage(skin="black",
                   title = "BEACONs Disturbance Explorer",
                   dashboardHeader(title = tags$div(
                     tags$img(
                       src = "logoblanc.png",  # Replace with your logo file name
                       height = "50px",   # Adjust the height of the logo
                       style = "margin-right: 10px;"  # Add some spacing around the logo
                     ), "BEACONs Disturbance Explorer" ), titleWidth = 400,
                    
                     # Add Reload Button Next to Sidebar Toggle
                     tags$li(
                       class = "dropdown",
                       actionButton(
                         "reload_btn",
                         label = "Reload",
                         icon = icon("refresh"),
                         style = "color: black; background-color: orange; border: none; font-size: 16px;"
                       ),
                       style = "position: absolute; left: 50px; top: 10px;"  # Adjust margin for placement next to the toggle
                     ),
                     tags$li(
                       class = "dropdown",  # Required for dropdown functionality
                       dropdownMenu(
                         type = "tasks", 
                         badgeStatus = NULL,
                         icon = icon("life-ring"),  # Life-ring icon triggering dropdown
                         headerText = "",  # No header text in dropdown
                         menuItem("Website", href = "https://beaconsproject.ualberta.ca/", icon = icon("globe")),
                         menuItem("GitHub", href = "https://github.com/beaconsproject/", icon = icon("github")),
                         menuItem("Contact us", href = "mailto: beacons@ualberta.ca", icon = icon("address-book"))
                       ),
                       # Plain Text "About Us" Positioned Next to Dropdown
                       tags$span(
                         "About Us", 
                         style = "font-size: 16px; position: relative; top: 15px; right: 10px; white-space: nowrap; color: white;"
                       )
                     )
                   ),
                   
                   dashboardSidebar(
                     width = 325,
                     sidebarMenu(id="tabs",
                                 menuItem("Welcome", tabName = "overview", icon = icon("th")),
                                 menuItem("Select study area", tabName = "select", icon = icon("arrow-pointer")),
                                 menuItem("Buffer features", tabName = "buffer", icon = icon("arrow-pointer")),
                                 menuItem("Download data", tabName = "download", icon = icon("th")),
                                 hr()
                     ),
                     conditionalPanel(
                       condition="input.tabs=='select'",
                       radioButtons("selectInput", "Select source dataset:",
                                    choices = list("Use demo dataset" = "usedemo", 
                                                   "Upload a GeoPackage (gpkg)" = "usegpkg"),
                                    selected = character(0), 
                                    inline = FALSE),
                       conditionalPanel(
                         condition="input.selectInput=='usegpkg'",
                         fileInput(inputId = "upload_gpkg", label = "Upload a GeoPackage:", multiple = FALSE, accept = ".gpkg"),
                         div(
                           style = "margin-top: -40px;",  # move it up
                           checkboxInput("createMatrix", label = "Define classification per disturbance type", value = F)
                         ),
                         conditionalPanel(
                           condition="input.createMatrix==true",
                           div(
                             style = "margin-left: 10px; margin-top: 30px;",
                             h5(strong("Classify Disturbance Layer"))
                           ),
                           div(
                             style = "margin-left: 10px;",
                             h5("Select linear disturbance attributes that describe:")
                           ),
                           uiOutput("lineIndustryUI"),
                           uiOutput("lineDistTypeUI"),
                           div(
                             style = "margin-left: 10px;",
                             h5("Select areal distrubance attributes that describe:")
                           ),
                           uiOutput("polyIndustryUI"),
                           uiOutput("polyDistTypeUI")
                         ),
                         br(),
                         fileInput(inputId = "upload_lineothers", label = "Upload other linear disturbances (shp):", multiple = TRUE, accept = c(".shp", ".shx", ".dbf", ".prj", ".cpg")),
                         fileInput(inputId = "upload_polyothers", label = "Upload other areal disturbances (shp):", multiple = TRUE, accept = c(".shp", ".shx", ".dbf", ".prj", ".cpg")),
                         hr()
                       ),
                       actionButton("distType", "Confirm", class = "btn-warning", style='color: #000')
                     ),
                     conditionalPanel(
                       condition = "input.tabs == 'buffer'",
                       radioButtons("selectBuffer", "Select buffer type:",
                                    choices = list("Use custom buffers" = "custom_buffers", 
                                                   "Use overall buffers" = "slider_buffers"),
                                    selected = character(0), 
                                    inline = FALSE),
                       conditionalPanel(
                         condition="input.selectBuffer=='slider_buffers'",
                         sliderInput("buffer1", label="Set linear buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
                         sliderInput("buffer2", label="Set areal buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE)
                       ),
                       hr(),
                       sliderInput("area1", label="Set minimum undisturbed patch size (km2):", min=0, max=2000, value = 0, step=10, ticks=FALSE),
                       checkboxInput("includeOthers", "Include other disturbances", value=FALSE),
                       conditionalPanel(
                         condition="input.includeOthers",
                         sliderInput("otherlinesize", label="Set buffer size (m) on other linear disturbances:", min=0, max=2000, value = 500, step=50, ticks=FALSE),
                         sliderInput("otherpolysize", label="Set a buffer size (m)on other areal disturbances:", min=0, max=2000, value = 500, step=50, ticks=FALSE)
                         
                       ),
                       checkboxInput("forceclaims", "Include mining claims", value=FALSE),
                       conditionalPanel(
                         condition="input.forceclaims",
                         sliderInput("minesize", label="Set mining claims buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE)
                       ),
                       checkboxInput("forcefire", "Include fires", value=FALSE),
                       conditionalPanel(
                         condition="input.forcefire",
                         sliderInput("firesize", label="Set minimum fire size (ha):", min=0, max=2000, value = 0, step=50, ticks=FALSE),
                         sliderInput("fireyear", label="Set range of years to include:", min=1900, max=2024, value = c(1960, 2023), sep = "")
                       ),
                       actionButton("goButton", "Generate undisturbed areas", class = "btn-warning", style='color: #000')
                     ),
                     conditionalPanel(
                       condition="input.tabs=='download'",
                       div(style="position:relative; left:calc(6%);", downloadButton("downloadData", "Download data", style='color: #000'))
                     )
                   ),
                   dashboardBody(
                     useShinyjs(),
                    # Link to custom CSS for the orange theme
                     tags$head(tags$link(rel = "icon", type = "image/png", href = "logoblanc.png"),
                               tags$link(rel = "stylesheet", type = "text/css", href = "green-theme.css")),
                     tabItems(
                       tabItem(tabName = "overview",
                               fluidRow(
                                 column(width = 8,  # Adjusted from 6 to 8 for better alignment
                                        tabBox(id = "landing", width = 12,
                                               tabPanel("Overview", includeMarkdown("docs/overview.md")),
                                               tabPanel("User Guide", includeMarkdown("docs/user_guide.md")),
                                               tabPanel("Datasets Requirements", includeMarkdown("docs/datasets.md"))
                                        )
                                 ),
                                 absolutePanel(
                                   right = 0, top = 0, width = 250, height = "100%",
                                   #style = "background-color: white; padding: 0px; overflow-y: auto; z-index: 1000;",
                                   style = "background-color: white; padding: 0;margin: 0;border: none; right: 0;overflow: hidden;z-index: 1000;",
                                   tags$img(src = "intact.jpg",width = "100%", style = "display: block;")
                                 )
                               )
                       ),
                       tabItem(tabName="select",
                               fluidRow(
                                 tabBox(id = "one", width="8",
                                        tabPanel("Mapview", leafletOutput("map1", height=900) %>% withSpinner()),
                                        tabPanel("Custom buffers",
                                                 tags$h4("Define linear buffer sizes:"),
                                                 uiOutput("linear_matrix_ui"),
                                                 tags$h4("Define areal buffer sizes:"),
                                                 uiOutput("areal_matrix_ui")
                                        ),
                                        tabPanel("User guide", uiOutput("guide_ui"))
                                 ),
                                 tabBox(
                                   id = "two", width="4",
                                   tabPanel("Statistics", tableOutput("tab1"),
                                   br(),  
                                   uiOutput("acronym_definitions"))
                                 )
                               )
                       )
                     )
                   )
)
