# Define server logic required to draw a histogram
server <- function(output, input) {
  clean_data <- reactive({
    merged_data
  })
  
  world_stats_data <- reactive({
    clean_data() %>%
      filter(Date==max(Date)) %>%
      arrange(desc(New_cases)) %>%
      select(`Country/Region`, Date, Confirmed, New_cases, Deaths, New_deaths, Recovered, New_recovered, Active)
  })
  
  output$confirmed_box <- renderInfoBox(
    infoBox("Confirmed", value = format(round(as.numeric(sum(world_stats_data()$Confirmed)), 1), nsmall=0, big.mark=","), icon = icon("check-circle"), width = 4, color = "yellow", fill = T)
  )
  
  output$death_box <- renderInfoBox(
    infoBox("Death", value = format(round(as.numeric(sum(world_stats_data()$Deaths)), 1), nsmall=0, big.mark=","), icon = icon("skull"), width = 4, color = "red", fill = T)
  )
  
  output$recover_box <- renderInfoBox(
    infoBox("Recovered", value = format(round(as.numeric(sum(world_stats_data()$Recovered)), 1), nsmall=0, big.mark=","), icon = icon("recycle"), width = 4, color = "green", fill = T)
  )
  
  output$map_type_selector <- renderUI({
    if(input$side_bar == "map"){
      pickerInput(
        inputId = "map_type",
        label = "Select data to visualize", 
        choices = c("Total Confirmed Cases", "New Cases", "Active Cases", "Total Deaths", "New Deaths", "Total Recovered", "New Recovered"),
        options = list(
          style = "btn-primary")
      )
    }
  })
  
  output$world_stats <- renderDT(
    datatable(
      world_stats_data(),
      rownames = F,
      extensions = list("Buttons"=NULL, "Responsive"=NULL),
      colnames = c("Country", "Date", "Confirmed Cases", "New Cases", "Deaths", "New Deaths", "Recovered", "New Recovered", "Active Cases"),
      class = "display compact",
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('csv', 'excel', 'pdf', 'print'))
    ) %>%
      formatStyle(c("Confirmed", "New_cases"), color = "yellow") %>%
      formatStyle(c("Deaths", "New_deaths"), color = "red") %>%
      formatStyle(c("Recovered", "New_recovered"), color = "green") %>%
      formatStyle("Active", color = "orange")
  )
  
  colorCases <- colorNumeric(palette = c("#9ecae1", "#08306b"), domain = shapes$Confirmed, na.color = "white")
  colorNewCases <- colorNumeric(palette = c("#9ecae1", "#08306b"), domain = shapes$New_cases, na.color = "white")
  colorActive <- colorNumeric(palette = c("#9ecae1", "#08306b"), domain = shapes$Active, na.color = "white")
  colorDeaths <- colorNumeric(palette = c("#9ecae1", "#08306b"), domain = shapes$Deaths, na.color = "white")
  colorNewDeaths <- colorNumeric(palette = c("#9ecae1", "#08306b"), domain = shapes$New_deaths, na.color = "white")
  colorRecover  <- colorNumeric(palette = c("#9ecae1", "#08306b"), domain = shapes$Recovered, na.color = "white")
  colorNewRecover <- colorNumeric(palette = c("#9ecae1", "#08306b"), domain = shapes$New_recovered, na.color = "white")
  
  output$map_view <- renderLeaflet({
    if(input$map_type == "Total Confirmed Cases"){
      leaflet(shapes) %>%
        addPolygons(
          weight=1,
          fillColor = ~colorCases(Confirmed),
          fillOpacity = 1,
          label = ~lapply(pop_ups, HTML),
          highlightOptions = highlightOptions(color = "#ffffff", weight = 2, bringToFront = T)
        ) %>%
        addLegend(
          pal=colorCases,
          values=~Confirmed,
          opacity=0.9,
          title = "Confirmed Cases",
          position = "topright"
        )
    }
    
    else if(input$map_type == "New Cases"){
      leaflet(shapes) %>%
        addPolygons(
          weight=1,
          fillColor = ~colorNewCases(New_cases),
          fillOpacity = 1,
          label = ~lapply(pop_ups, HTML),
          highlightOptions = highlightOptions(color = "#ffffff", weight = 2, bringToFront = T)
        ) %>%
        addLegend(
          pal=colorNewCases,
          values=~New_cases,
          opacity=0.9,
          title = "New Cases",
          position = "topright"
        )
    }
    
    else if(input$map_type == "Active Cases"){
      leaflet(shapes) %>%
        addPolygons(
          weight=1,
          fillColor = ~colorActive(Active),
          fillOpacity = 1,
          label = ~lapply(pop_ups, HTML),
          highlightOptions = highlightOptions(color = "#ffffff", weight = 2, bringToFront = T)
        ) %>%
        addLegend(
          pal=colorActive,
          values=~Active,
          opacity=0.9,
          title = "Active Cases",
          position = "topright"
        )
    }
    
    else if(input$map_type == "Total Deaths"){
      leaflet(shapes) %>%
        addPolygons(
          weight=1,
          fillColor = ~colorDeaths(Deaths),
          fillOpacity = 1,
          label = ~lapply(pop_ups, HTML),
          highlightOptions = highlightOptions(color = "#ffffff", weight = 2, bringToFront = T)
        ) %>%
        addLegend(
          pal=colorDeaths,
          values=~Deaths,
          opacity=0.9,
          title = "Total Daths",
          position = "topright"
        )
    }
    
    else if(input$map_type == "New Deaths"){
      leaflet(shapes) %>%
        addPolygons(
          weight=1,
          fillColor = ~colorNewDeaths(New_deaths),
          fillOpacity = 1,
          label = ~lapply(pop_ups, HTML),
          highlightOptions = highlightOptions(color = "#ffffff", weight = 2, bringToFront = T)
        ) %>%
        addLegend(
          pal=colorNewDeaths,
          values=~New_deaths,
          opacity=0.9,
          title = "New Deaths",
          position = "topright"
        )
    }
    
    else if(input$map_type == "Total Recovered"){
      leaflet(shapes) %>%
        addPolygons(
          weight=1,
          fillColor = ~colorRecover(Recovered),
          fillOpacity = 1,
          label = ~lapply(pop_ups, HTML),
          highlightOptions = highlightOptions(color = "#ffffff", weight = 2, bringToFront = T)
        ) %>%
        addLegend(
          pal=colorRecover,
          values=~Recovered,
          opacity=0.9,
          title = "Total Recovered",
          position = "topright"
        )
    }
    
    else if(input$map_type == "New Recovered"){
      leaflet(shapes) %>%
        addPolygons(
          weight=1,
          fillColor = ~colorNewRecover(New_recovered),
          fillOpacity = 1,
          label = ~lapply(pop_ups, HTML),
          highlightOptions = highlightOptions(color = "#ffffff", weight = 2, bringToFront = T)
        ) %>%
        addLegend(
          pal=colorNewRecover,
          values=~New_recovered,
          opacity=0.9,
          title = "New Recovered",
          position = "topright"
        )
    }
  })
  
  output$worldwide_evolution <- renderPlotly({
    clean_data() %>% 
      group_by(Date) %>% 
      summarise(
        Confirmed = sum(Confirmed, na.rm = T),
        Active = sum(Active, na.rm = T),
        Deaths = sum(Deaths, na.rm = T),
        Recovered = sum(Recovered, na.rm = T)
      ) %>%
      gather(key = "Variable", value = "Freq", -Date) %>%
      plot_ly(
        type = "scatter",
        mode = "lines",
        x = ~Date,
        y = ~Freq,
        color = ~Variable,
        colors = "Set2"
      ) %>%
      layout(
        xaxis = list(title="", showgrid = F, color = "#ffffff"),
        yaxis = list(title="", color = "#ffffff"),
        paper_bgcolor = "#343e48",
        plot_bgcolor = "#343e48",
        legend = list(
          font = list(color = "#ffffff"),
          x = 0.1,
          y = 0.9
        ),
        hovermode = "compare"
      )
  })
  
  output$countrywise_evolution <- renderPlotly({
    clean_data() %>% 
      filter(`Country/Region`==input$country_selector) %>%
      group_by(`Country/Region`, Date) %>% 
      summarise(
        Confirmed = sum(Confirmed, na.rm = T),
        Active = sum(Active, na.rm = T),
        Deaths = sum(Deaths, na.rm = T),
        Recovered = sum(Recovered, na.rm = T)
      ) %>%
      gather(key = "Variable", value = "Freq", -Date, -`Country/Region`) %>%
      plot_ly(
        type = "scatter",
        mode = "lines",
        x = ~Date,
        y = ~Freq,
        color = ~Variable,
        colors = "Set2"
      ) %>%
      layout(
        xaxis = list(title="", showgrid = F, color = "#ffffff"),
        yaxis = list(title="", color = "#ffffff"),
        paper_bgcolor = "#343e48",
        plot_bgcolor = "#343e48",
        legend = list(
          font = list(color = "#ffffff"),
          x = 0.1,
          y = 0.9
        ),
        hovermode = "compare"
      )
  })
  
  selected_countries <- reactive({
    paste(input$country_selector2, collapse = "|")
  })
  
  output$compare_new_cases <- renderPlotly({
    clean_data() %>%
      filter(str_detect(`Country/Region`, pattern = regex(selected_countries()))) %>%
      plot_ly(
        type = "scatter",
        mode = "lines",
        x = ~Date,
        y = ~New_cases,
        color = ~`Country/Region`,
        colors = "Set2"
      )%>%
      layout(
        xaxis = list(title="", showgrid = F, color = "#ffffff"),
        yaxis = list(title="", color = "#ffffff"),
        paper_bgcolor = "#343e48",
        plot_bgcolor = "#343e48",
        legend = list(
          font = list(color = "#ffffff"),
          x = 0.1,
          y = 0.9
        ),
        hovermode = "compare"
      )
  })
  
}