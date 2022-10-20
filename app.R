library(sf)
library(dplyr)
library(terra)
library(leaflet)
library(shinydashboard)

ui = dashboardPage(skin="blue",
  dashboardHeader(title = "RDM Explorer"),
  dashboardSidebar(
    sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("th")),
        menuItem("Footprint/intactness", tabName = "fri", icon = icon("th")),
        menuItem("Effects on landcover", tabName = "land", icon = icon("th")),
        menuItem("Effects on hydrology", tabName = "hydro", icon = icon("th"))
        #menuItem("Sensitivity analysis", tabName = "sa", icon = icon("th"))
    ),
    hr(),
    selectInput("fda", label="Select FDA:", choices=c("10AB","09EA")),
    sliderInput("buffer1", label="Linear buffer size (m):", min=0, max=2000, value = 1000, step=100, ticks=FALSE),
    sliderInput("buffer2", label="Areal buffer size (m):", min=0, max=2000, value = 1000, step=100, ticks=FALSE),
    sliderInput("area1", label="Minimum size of intact areas (km2):", min=0, max=5000, value = 1000, step=500, ticks=FALSE),
    br(),
    actionButton("goButton", "Generate intactness map"),
    br(),
    sliderInput("alpha", label="Map transparency:", min=0, max=1, value = 0.5, step=0.05, ticks=FALSE),
    hr(),
    downloadButton("downloadMap","Download intactness map")
  ),
  dashboardBody(
    tabItems(
       tabItem(tabName="overview",
        fluidRow(
          tabBox(
            id = "zero", width="12",
            tabPanel("Welcome!", htmlOutput("help")),
            #tabPanel("Footprint", includeMarkdown("../docs/footprint.md")),
            #tabPanel("Documentation", htmlOutput("datasets"))
          )
        )
      ),
       tabItem(tabName="fri",
            fluidRow(
                tabBox(
                    id = "one", width="8",
                    tabPanel("Map viewer", leafletOutput("map", height=750)),
                ),
                tabBox(
                    id = "two", width="4",
                    tabPanel("Intactness", tableOutput("tab1"))
                ),
                tabBox(
                    id="three", width="4",
                    tabPanel("Linear disturbances", tableOutput("tab2")),
                    tabPanel("Areal disturbances", tableOutput("tab3"))
                ),
            )
        ),
       tabItem(tabName="land",
            fluidRow(
                tabBox(
                    id = "one", width="8",
                    tabPanel("Landcover", 
                    plotOutput("map2", height=750))
                ),
                tabBox(
                    id="two", width="4",
                    #selectInput("year", label="Select year:", choices=c(2019,1984)),
                    selectInput("type", label="Select class:", choices=c("all classes","bryoids","shrubs","wetland","wetland_treed","herbs","coniferous","broadleaf","mixedwood"),selected="all classes")
                ),
                tabBox(
                    id = "two", width="4",
                    tabPanel("Intact", tableOutput("tab4"))
                ),
            )
        ),
       tabItem(tabName="hydro",
            fluidRow(
                tabBox(
                    id = "one", width="8",
                    tabPanel("Hydrology", 
                    leafletOutput("map3", height=750))
                ),
                tabBox(
                    id = "two", width="4",
                    tabPanel("Percent within disturbed area", tableOutput("tab5"))
                ),
            )
        )
    )
  )
)


server = function(input, output) {

    output$help <- renderText({
        includeMarkdown("docs/overview.md")
    })

    fda <- reactive({
        fda <- paste0('data/fda_',input$fda,'.gpkg')
    })

    bnd <- reactive({
        bnd <- st_read(fda(), 'FDA', quiet=T)
    })

    #lakesrivers <- reactive({
    #    lakesrivers <- st_read(fda(), 'lakes_rivers', quiet=T)
    #})

    fires <- reactive({
        fires <- st_read(fda(), 'Fire_History', quiet=T)
    })

    ifl2000 <- reactive({
        ifl2000 <- st_read(fda(), 'IFL_2000', quiet=T)
    })

    ifl2020 <- reactive({
        ifl2020 <- st_read(fda(), 'IFL_2020', quiet=T)
    })

    linear <- reactive({
        if (input$fda=='10AB') {
            linear <- st_read(fda(), 'Linear_Features+', quiet=T)
        } else {
            linear <- st_read(fda(), 'Linear_Features', quiet=T)
        }
    })

    quartz <- reactive({
        quartz <- st_read(fda(), 'Quartz_Claims', quiet=T)
    })

    areal <- reactive({
        if (input$fda=='10AB') {
            areal <- st_read(fda(), 'Areal_Features+', quiet=T)
        } else {
            areal <- st_read(fda(), 'Areal_Features', quiet=T)
        }
    })

    vv <- eventReactive(input$goButton, {
        v1 <- st_union(st_buffer(linear(), input$buffer1))
        v2 <- st_union(st_buffer(areal(), input$buffer2))
        hfp <- st_union(v1, v2)
    })

    v <- eventReactive(input$goButton, {
        ifl <- st_difference(bnd(), vv())
    })

    output$map <- renderLeaflet({
        bnd <- st_transform(bnd(), 4326)
        ifl2000 <- st_transform(ifl2000(), 4326)
        ifl2020 <- st_transform(ifl2020(), 4326)
        fires <- st_transform(fires(), 4326)
        quartz <- st_transform(quartz(), 4326)
        areal <- st_transform(areal(), 4326)
        linear <- st_transform(linear(), 4326)
        pal <- colorBin("YlOrRd", domain = fires$FIRE_YEAR, bins = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020, Inf))
        labels <- sprintf("Fire year: %s<br/>Fire cause: %s", fires$FIRE_YEAR, fires$GENERAL_FIRE_CAUSE) %>% lapply(htmltools::HTML)
        m <- leaflet(bnd) %>% 
    		addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
	    	addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
            addPolygons(data=bnd, color='black', fill=F, weight=2, group="FDA") %>%
            #addPolygons(data=fires, fillColor = ~pal(FIRE_YEAR), color='grey', weight=1, group="Fires", opacity=1, fillOpacity = 0.7,
            addPolygons(data=fires, fillColor="red", color='grey', weight=1, group="Fires", opacity=1, fillOpacity=0.5,
              highlightOptions = highlightOptions(weight=2, color="black", bringToFront=T),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%
            #addLegend(pal = pal, values = ~fires$FIRE_YEAR, opacity = 0.7, title = NULL, position = "bottomright") %>%
            addPolygons(data=quartz, color='yellow', fill=F, weight=1, group="Quartz") %>%
            addPolylines(data=linear, color='red', group="Linear features") %>%
            addPolygons(data=areal, color='black', fill=T, stroke=F, group="Areal features", fillOpacity=input$alpha) %>%
            addPolygons(data=ifl2020, color='darkgreen', fillOpacity=input$alpha, group="IFL 2020") %>%
            addPolygons(data=ifl2000, color='darkgreen', fillOpacity=input$alpha, group="IFL 2000")
            if (input$goButton) {
                v <- st_transform(v(), 4326)
                vv <- st_transform(vv(), 4326)
                m <- m %>% addPolygons(data=v, color='blue', stroke=F, fillOpacity=input$alpha, group='Intactness') %>%
                    addPolygons(data=vv, color='black', stroke=F, fillOpacity=input$alpha, group='Footprint')
            }
        #pal <- colorBin("PuOr", fires$GENERAL_FIRE_CAUSE, bins = c(0, .1, .4, .9, 1))
        m <- m %>% addLayersControl(position = "topleft",
                baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                overlayGroups = c("IFL 2020","IFL 2000","Intactness","Footprint","Fires","Quartz","Areal features","Linear features"),
            options = layersControlOptions(collapsed = TRUE)) %>%
                hideGroup(c("IFL 2020","IFL 2000","Intactness","Footprint","Fires","Quartz","Areal features","Linear features"))
        #m <- m %>% addLegend(pal=pal, values=~fires$GENERAL_FIRE_CAUSE, position=c("bottomright"), title="Fire cause", opacity=0.8)
        m
    })

    lcc2 <- reactive({
        dir1 <- substr(fda(),1,nchar(fda())-5)
        lcc <- rast(paste0(dir1,'/','lc_2019.tif'))
        lcc <- subst(lcc, 0, NA)
    })

    lcc_cols <- reactive({
        x <- read.csv('data/lc_cols.csv') %>%
            mutate(color=rgb(red,green,blue,maxColorValue=255)) %>%
            pull(color)
    })

    output$map2 <- renderPlot({
        m <- c(3.1, 3.1, 1.1, 1.1)
        if (input$type=='all classes') {
            r <- lcc2()
            val <- c(20,31,32,33,40,50,80,81,100,210,220,230)
            cls <- c("water","snow_ice","rock_rubble","exposed_barren_land","bryoids","shrubs","wetland","wetland_treed","herbs","coniferous","broadleaf","mixedwood")
            df <- data.frame(value=val, category=cls)
            set.cats(r, layer=1, value=df)
            plot(r, col=lcc_cols(), plg=list(x="topright"), mar=m)
        } else {
            #r <- r1 + r2
            r <- subst(lcc_rcl(), 0, NA)
            #val <- c(1,2,3)
            val <- 1
            #cls <- c('loss','gain','stable')
            cls <- input$type
            df <- data.frame(value=val, category=cls)
            set.cats(r, layer=1, value=df)
            #plotRGB(tiles())
            plot(r, col=c('red','green','darkgreen'), plg=list(x="topright"), mar=m)
            plot(r, col='darkgreen', plg=list(x="topright"), mar=m)
        }
        lines(vect(bnd()), lwd=2)
        if (input$goButton) {
            plot(vect(vv()), col='red', alpha=0.5, add=T)
        } else {
            lines(vect(linear()), lwd=1, col='red')
            plot(vect(areal()), col='red', add=T)
        }
    })

    output$map3 <- renderLeaflet({
        bnd <- st_transform(bnd(), 4326)
        leaflet(bnd) %>% 
    		addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
	    	addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
            addPolygons(data=bnd, color='black', fill=F, weight=2, group="FDA") %>%
            #addLegend(pal=pal, values(agePrj), position=c("bottomright"), title="Forest age", opacity=0.8) %>%
            addLayersControl(position = "topright",
                baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                overlayGroups = c("FDA"),
            options = layersControlOptions(collapsed = FALSE)) %>%
                hideGroup("")
        #if (input$goButton) {
        #    m <- m + tm_shape(vv()) + tm_fill(col='black', alpha=input$alpha, group="Footprint (2019)")
        #}
        #m <- m + #tm_shape(streams()) + tm_lines(col='blue', lwd=1, group="Streams") +
        #    tm_shape(lakesrivers()) + tm_fill(col='blue', alpha=input$alpha, group="LakesRivers")
    })

    dta1 <- reactive({
        x <- tibble(Map=c("FDA (km2)","IFL 2000 (%)","IFL 2020 (%)","Intactness (%)","Footprint (%)"), Area=NA)
        x$Area[x$Map=="FDA (km2)"] <- round(st_area(bnd())/1000000,0)
        x$Area[x$Map=="IFL 2000 (%)"] <- round(sum(st_area(ifl2000()))/st_area(bnd())*100,1)
        x$Area[x$Map=="IFL 2020 (%)"] <- round(sum(st_area(ifl2020()))/st_area(bnd())*100,1)
        if (input$goButton) {
            x$Area[x$Map=="Intactness (%)"] <- round(sum(st_area(v()))/st_area(bnd())*100,1)
            x$Area[x$Map=="Footprint (%)"] <- round(sum(st_area(vv()))/st_area(bnd())*100,1)
        }
        x
    })

	output$tab1 <- renderTable({
        dta1()
    })

    dta2 <- reactive({
        km <- group_by(linear(), TYPE_DISTURBANCE) %>%
            #mutate(Length_km = st_length(linear())) %>%
            #summarize(Length_km = sum(Shape_Length))
            summarize(Length_km = sum(Length_km))
        #x <- tibble(Disturbance_type=km$TYPE_DISTURBANCE, Length_km=round(km$Length_km/1000,1), Buffer_size=input$buffer1)
        x <- tibble(Disturbance_type=km$TYPE_DISTURBANCE, Length_km=km$Length_km, Buffer_size=input$buffer1)
        x
    })

	output$tab2 <- renderTable({
        dta2()
    })

    dta3 <- reactive({
        ha <- group_by(areal(), TYPE_DISTURBANCE) %>%
            #summarize(Area_m2 = sum(Shape_Area))
            summarize(Area_ha = sum(Area_ha))
        #x <- tibble(Disturbance_type=m2$TYPE_DISTURBANCE, Area_ha=round(m2$Area_m2/10000,1), Buffer_size=input$buffer1)
        x <- tibble(Disturbance_type=ha$TYPE_DISTURBANCE, Area_ha=ha$Area_ha, Buffer_size=input$buffer1)
        x
    })

	output$tab3 <- renderTable({
        dta3()
    })

    lcc_rcl <- reactive({
        if (input$type=='bryoids') {
            r <- subst(lcc2(), c(20,31,32,33,40,50,80,81,100,210,220,230), c(0,0,0,0,1,0,0,0,0,0,0,0))
        } else if (input$type=='shrubs') {
            r <- subst(lcc2(), c(20,31,32,33,40,50,80,81,100,210,220,230), c(0,0,0,0,0,1,0,0,0,0,0,0))
        } else if (input$type=='wetland') {
            r <- subst(lcc2(), c(20,31,32,33,40,50,80,81,100,210,220,230), c(0,0,0,0,0,0,1,0,0,0,0,0))
        } else if (input$type=='wetland_treed') {
            r <- subst(lcc2(), c(20,31,32,33,40,50,80,81,100,210,220,230), c(0,0,0,0,0,0,0,1,0,0,0,0))
        } else if (input$type=='herbs') {
            r <- subst(lcc2(), c(20,31,32,33,40,50,80,81,100,210,220,230), c(0,0,0,0,0,0,0,0,1,0,0,0))
        } else if (input$type=='coniferous') {
            r <- subst(lcc2(), c(20,31,32,33,40,50,80,81,100,210,220,230), c(0,0,0,0,0,0,0,0,0,1,0,0))
        } else if (input$type=='broadleaf') {
            r <- subst(lcc2(), c(20,31,32,33,40,50,80,81,100,210,220,230), c(0,0,0,0,0,0,0,0,0,0,1,0))
        } else if (input$type=='mixedwood') {
            r <- subst(lcc2(), c(20,31,32,33,40,50,80,81,100,210,220,230), c(0,0,0,0,0,0,0,0,0,0,0,1))
        }
    })

    #lcc_intact <- eventReactive(input$goButton, {
    lcc_intact <- reactive({
        if (input$type=="all classes") {
            v <- vect(vv())
            r2 <- crop(lcc2(), v)
            r2 <- mask(r2, v)
        } else {
            v <- vect(vv())
            r2 <- crop(lcc_rcl(), v)
            r2 <- mask(r2, v)
        }
    })

    lcc_one <- reactive({
        r_freq <- freq(lcc_rcl())
        if (input$goButton) {
            x <- tibble(value=input$type, count_fda=r_freq$count[2])
            r2 <- lcc_intact()
            r2_freq <- freq(r2)[2,2:3] %>%
                rename(count_2019=count)
            xx <- left_join(x, r2_freq) %>%
                mutate(Area_ha=round(count_fda*30*30/10000,1),
                    Intact_pct=round((count_fda-count_2019)/count_fda*100,1),
                    count_fda=NULL, count_2019=NULL) %>%
                rename(Class=value)
            xx
        } else {
            xx <- tibble(Class=input$type, Count=r_freq$count[2]) %>%
                mutate(Area_ha=round(Count*30*30/10000,1),
                Count=NULL)
        }
        xx
    })

    lcc_all <- reactive({
        r_freq <- freq(lcc2())
        if (input$goButton) {
            x <- tibble(value=r_freq$value, count_fda=r_freq$count)
            r2 <- lcc_intact()
            r2_freq <- freq(r2)[,2:3] %>%
                rename(count_2019=count)
            xx <- left_join(x, r2_freq) %>%
                mutate(Area_ha=round(count_fda*30*30/10000,1),
                    Intact_pct=round((count_fda-count_2019)/count_fda*100,1),
                    count_fda=NULL, count_2019=NULL) %>%
                rename(Class=value)
        } else {
            xx <- tibble(Class=r_freq$value, Count=r_freq$count) %>%
                mutate(Area_ha=round(Count*30*30/10000,1),
                Count=NULL)
        }
        xx
    })

	output$tab4 <- renderTable({
        if (input$type=='all classes') {
            lcc_all()
        } else {
            lcc_one()
        }
    })

    dta5 <- reactive({
        x <- readr::read_csv('data/hydro.csv')
    })

	output$tab5 <- renderTable({
        dta5()
    })

  output$downloadMap <- downloadHandler(
    filename = function() {'fda_intactness.gpkg'},
    content = function(file) {st_write(v(), dsn=file, layer=paste0('linear',input$buffer1,'_areal',input$buffer2,'.gpkg'))}
  )

}

shinyApp(ui, server)
