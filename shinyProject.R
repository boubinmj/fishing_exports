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


countries <- distinct(fish_data %>% select(., Country))
species <- distinct(fish_data %>% select(., Species))
years_a <- distinct(fish_data %>% select(., Year))



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
    
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "map",
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
  
}

shinyApp(ui, server)