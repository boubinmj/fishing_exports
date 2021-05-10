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