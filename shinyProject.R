library("dplyr")
library("shiny")
library('magrittr')
library('shinydashboard')
library('DT')
library('googleVis')
library('reshape2')
library('tidyr')

##############################################
#################### GLOBAL ##################
##############################################

# Import Data Set

fish_data <-
  read.csv("FISH_FLD_19022021172720481.csv", header = TRUE)
head(fish_data)

# fish_data2 <- fish_data %>%
#   filter(., Measure == "Tonnes") %>%
#   select(.,Country, Year, Species, Value) %>%
#   spread(Year, Value) %>%
#   mutate_if(is.numeric , replace_na, replace = 0)
# head(fish_data2)

# Generate totals per country, per year

by_years <- fish_data %>%
  filter(., Measure == "Tonnes") %>%
  select(., Country, Year, Value) %>%
  group_by(., Country) %>%
  group_by(., Year, .add = TRUE) %>%
  summarise(., total_yr = sum(Value)) %>%
  spread(Year, total_yr) %>%
  mutate_if(is.numeric , replace_na, replace = 0)
head(by_years)

# generate totals per country, per species

by_species <- fish_data %>%
  filter(., Measure == "Tonnes") %>%
  select(., Country, Species, Value) %>%
  group_by(., Country) %>%
  group_by(., Species, .add = TRUE) %>%
  summarise(., total_sp = sum(Value)) %>%
  spread(Species, total_sp) %>%
  mutate_if(is.numeric, replace_na, replace = 0)
head(by_species)

by_Belgium <- fish_data %>%
  filter(., Country == "Norway") %>%
  filter(., Measure == "Tonnes") %>%
  select(.,Species,Year, Value) %>% 
  group_by(., Species) %>% 
  spread(Species, Value) %>% 
  mutate_if(is.numeric, replace_na, replace = 0)
head(by_Belgium)

# countries <- unique(df[,2])
# fish <- unique(df[,4])

countries <- distinct(fish_data %>% select(., Country))
species <- distinct(fish_data %>% select(., Species))
years_a <- distinct(fish_data %>% select(., Year))

# belgium <- filter(fish_data, Country == "Belgium")
#
# tonnes <- filter(belgium, Measure == "Tonnes")
#
# tonnes %>% summarise(., total = sum(Value))

# fish_data_tonnes <- fish_data %>%
#   filter(., Measure == "Tonnes") %>%
#   summarise(., total = sum(Value)) %>%
#   spread(.,Year, Value)
# head(fish_data_tonnes)
#
# tots <- c()
#
# by_year <- group_by(fish_data, Year, Country)
# count_by_year = summarise(by_year, count=sum(Value))
#
# new <- fish_data %>%
#   group_by(., Country, Year) %>%
#   summarise(., total=sum(Value)) %>%
#   filter(., Year == 2003)
#
# per_year <- c()
#
# for(x in 1:length(years[,1])){
#
#   per_year[x] <- new$total
# }
#
#
# for(i in 1:length(countries[,1])) {
#   nat <- filter(fish_data, Country == countries[i,])
#   tonnes <- filter(nat, Measure == "Tonnes")
#   years <- filter(tonnes, Year == 2007)
#   total <- summarise(years, total = sum(Value))
#   tots[i] <- total[,1]
# }
#
# export_totals <- data.frame(countries, tots)
# choice <- years

ui <- dashboardPage(
  dashboardHeader(title = 'This is a Dashboard'),
  dashboardSidebar(
    sidebarUserPanel('Matt', image = 'https://pyxis.nymag.com/v1/imgs/dc5/011/2ea57ca9a7a5d9518b2f3cd94ccdde218f-25-emoji-subpoena.rsocial.w1200.jpg'),
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Species Map", tabName = "spec-map", icon  = icon("map")),
      menuItem(
        "Species Data",
        tabName = "spec-data",
        icon = icon("database")
      ),
      menuItem("Belgium",
               tabName = "bel",
               icon = icon('cog'))
    )
    # selectizeInput("selected",
    #                "Select Item to Display",
    #                years_a)
    # selectizeInput("selected",
    #                "Select Item to Display", q
    #                species)
    
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "map",
      
      # fluidRow(infoBoxOutput("maxBox"),
      #          infoBoxOutput("minBox"),
      #          infoBoxOutput("avgBox")),
      
      fluidRow(box(
        htmlOutput("map"), height = 500, width = 500
      )),
      fluidRow(box(
        htmlOutput("hist"), height = 500, width = 500
      )),
      fluidRow(wellPanel(
        selectizeInput("selected",
                       "Select Year to Display",
                       years_a)
      ))
    ),
    tabItem(tabName = "data",
            fluidRow(box(
              DT::dataTableOutput("table")
            ))),
    
    tabItem(tabName = "spec-map",
            fluidRow(box(
              htmlOutput("specGraph"), height = 500, width = 500
            )),
            fluidRow(box(
              htmlOutput("specHist"), height = 500, width = 500
            )),
            fluidRow(fluidRow(
              wellPanel(
                selectizeInput("spec_type",
                               "Select Species to Display",
                               species)
              )
            ))),
    
    tabItem(tabName = "spec-data",
            fluidRow(box(
              DT::dataTableOutput("dataTable")
            ))),
    tabItem(tabName = "bel",
            fluidRow(box(
              htmlOutput("scatter"), height=500, width=500
            )),
            fluidRow(fluidRow(
              wellPanel(
                selectizeInput("spec_type", "Select Species to Display", species)
              ))
            )
            )
  ))
)


server <- function(input, output) {
  output$map <- renderGvis({
    gvisGeoChart(
      by_years,
      "Country",
      input$selected,
      options = list(
        region = "world",
        
        # displayMode="regions",
        # resolution="provinces"
        width = "auto",
        height = "auto"
      )
    )
  })
  
  output$hist <- renderGvis(gvisHistogram(
    by_years[, input$selected, drop = FALSE],
    options = list(vAxes = "[{title:'Total Number of Countries'}]",
                   hAxes = "[{title:'Total Fish'}]")
  ))
  
  output$table <- DT::renderDataTable({
    datatable(by_years, rownames = FALSE) %>%
      formatStyle(input$selected,
                  background = "skyblue",
                  fontWeight = 'bold')
  })
  
  output$specGraph <- renderGvis(gvisGeoChart(
    by_species,
    "Country",
    input$spec_type,
    options = list(
      region = "world",
      width = "auto",
      height = "auto"
    )
  ))
  
  output$specHist <- renderGvis(gvisHistogram(by_species[, input$spec_type, drop =
                                                           FALSE]))
  
  output$dataTable <- DT::renderDataTable({
    datatable(by_species, rownames = FALSE) %>%
      formatStyle("Country",
                  input$spec_type,
                  background = "skyblue",
                  fontWeight = "bold")
  })
  
  output$scatter <- renderGvis(gvisLineChart(by_Belgium))
  
  #output$scatter <- renderGvis(gvisScatterChart(by_Belgium))
  
  # output$maxBox <- renderInfoBox({
  #   max_value <- max(export_totals[,input$selected])
  #   max_state <-
  #     state_stat$state.name[state_stat[,input$selected]==max_value]
  #   infoBox(max_state, max_value, icon = icon("hand-o-up"))
  # })
  #
  # output$minBox <- renderInfoBox({
  #   min_value <- min(export_totals[,input$selected])
  #   min_state <-
  #     state_stat$state.name[state_stat[,input$selected]==min_value]
  #   infoBox(min_state, min_value, icon = icon("hand-o-down"))
  # })
  #
  # output$avgBox <- renderInfoBox(
  #   infoBox(paste("AVG", input$selected),
  #           mean(state_stat[,input$selected]),
  #           icon = icon("calculator"), fill = TRUE)
  # )
  
}

shinyApp(ui, server)