ui = dashboardPage(skin="black",
                   dashboardHeader(title = tags$div(
                     tags$img(
                       src = "logoblanc.png",  # Replace with your logo file name
                       height = "50px",   # Adjust the height of the logo
                       style = "margin-right: 10px;"  # Add some spacing around the logo
                     ),"BEACONs Disturbance Explorer"), titleWidth = 400,
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
                     sidebarMenu(id="tabs",
                                 menuItem("Overview", tabName = "overview", icon = icon("th")),
                                 menuItem("Select study area", tabName = "select", icon = icon("arrow-pointer")),
                                 menuItem("Buffer features", tabName = "buffer", icon = icon("arrow-pointer")),
                                 menuItem("Download data", tabName = "download", icon = icon("th")),
                                 hr()
                     ),
                     conditionalPanel(
                       condition="input.tabs=='select'",
                       radioButtons("selectInput", "Select your input gpkg:",
                                    choices = list("Use demo dataset" = "usedemo", 
                                                   "Upload a geopackage (gpkg)" = "usegpkg"),
                                    selected = character(0), 
                                    inline = FALSE),
                       conditionalPanel(
                         condition="input.selectInput=='usegpkg'",
                         fileInput(inputId = "upload_gpkg", label = "Upload a geopackage:", multiple = FALSE, accept = ".gpkg"),
                         hr(),
                         div(style = "margin: 15px; font-size:13px; font-weight: bold", "Specify attributes that describe disturbances"),
                         div(style = "margin-top: -20px;",selectInput("lineindustry", label = div(style = "font-size:13px;margin-top: -10px;", "Select linear industry attribute"), choices = NULL)),
                         div(style = "margin-top: -20px;",selectInput("linedisttype", label = div(style = "font-size:13px;", "Select linear disturbance type attribute"), choices = NULL)),
                         div(style = "margin: 15px; font-size:13px; font-weight: bold", "Set the attributes that describe areal disturbances"),
                         tags$br(),
                         div(style = "margin-top: -20px;",selectInput("polyindustry", label = div(style = "font-size:13px;", "Select areal industry attribute"), choices =  NULL)),
                         div(style = "margin-top: -20px;",selectInput("polydisttype", label = div(style = "font-size:13px;margin: 0px;", "Select areal disturbance type attribute"), choices = NULL))
                       ),
                       actionButton("distType", "Confirm", class = "btn-warning", style='color: #000')
                     ),
                     conditionalPanel(
                       condition = "input.tabs == 'buffer'",
                       HTML("<h4>&nbsp; &nbsp; Select buffer type</h4>"),
                       #div(style = "top: 50px; margin: 15px; font-size:13px; font-weight: bold", "Select buffer type"),
                       checkboxInput("custom_buffers", "Use custom buffers", value=FALSE),
                       HTML("<h5>&nbsp; &nbsp; &nbsp;&nbsp;&nbsp;-- OR --</h4>"),
                       sliderInput("buffer1", label="Set linear buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
                       sliderInput("buffer2", label="Set areal buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
                       hr(),
                       sliderInput("area1", label="Min intact patch size (km2):", min=0, max=2000, value = 0, step=50, ticks=FALSE),
                       checkboxInput("forceclaims", "Include mining claims", value=FALSE),
                       conditionalPanel(
                         condition="input.forceclaims",
                         sliderInput("minesize", label="Mine claims buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE)
                       ),
                       checkboxInput("forcefire", "Include fires", value=FALSE),
                       conditionalPanel(
                         condition="input.forcefire",
                         sliderInput("firesize", label="Include minimum fire size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE)
                       ),
                       actionButton("goButton", "Generate intactness map", style='color: #000')
                     ),
                     conditionalPanel(
                       condition="input.tabs=='download'",
                       div(style="position:relative; left:calc(6%);", downloadButton("downloadData", "Download data", style='color: #000'))
                     )
                   ),
                   dashboardBody(
                     useShinyjs(),
                    # Link to custom CSS for the orange theme
                     tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "green-theme.css")),
                     tabItems(
                       tabItem(tabName="overview",
                               fluidRow(
                                 tabBox(id = "landing", width="12",
                                        tabPanel("Overview", includeMarkdown("docs/overview.md")),
                                        tabPanel("User guide", includeMarkdown("docs/quick_start.md")),
                                        tabPanel("Datasets", includeMarkdown("docs/datasets.md"))
                                 )            )
                       ),
                       tabItem(tabName="select",
                               fluidRow(
                                 tabBox(id = "one", width="8",
                                        tabPanel("Mapview", leafletOutput("map1", height=900) %>% withSpinner()),
                                        tabPanel("Custom buffers", tags$h4("Define linear buffer sizes:"), matrixInput("linear_buffers", value=m1, rows=list(names=FALSE, extend=TRUE), cols=list(names=TRUE)), tags$h4("Define areal buffer sizes:"), matrixInput("areal_buffers", value=m2, rows=list(names=FALSE, extend=TRUE), cols=list(names=TRUE)))
                                 ),
                                 tabBox(
                                   id = "two", width="4",
                                   tabPanel("Statistics", tableOutput("tab1"))
                                 )
                               )
                       )
                     )
                   )
)
