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