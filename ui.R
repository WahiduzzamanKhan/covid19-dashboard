ui <- dashboardPage(
  header = dashboardHeader(
    title = tags$div(
      id = "title-div",
      tags$i(class = "fa fa-first-aid", id = "main-icon"),
      tags$span("COVID-19", id = "title-main-text"),
      tags$span("LIVE", id  = "title-sub-text")
    ),
    fixed = T
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "side_bar",
      menuItem("Overview", tabName = "overview", icon = icon("newspaper")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      uiOutput("map_type_selector"),
      menuItem("Graphs", tabName = "graphs", icon = icon("line-chart")),
      menuItem("About", tabName = "about", icon = icon("id-card"))
    )
  ),
  body = dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    tags$link(rel="stylesheet", href="style.css"),
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          infoBoxOutput("confirmed_box"),
          infoBoxOutput("death_box"),
          infoBoxOutput("recover_box")
        ),
        
        fluidRow(
          box(
            withSpinner(DTOutput("world_stats"), type=8, color = '#FFFFFF'),
            title = "Worldwide COVID-19 Stats",
            solidHeader = T,
            width = 12,
            collapsible = T,
            closable = F
          )
        )
      ),
      
      tabItem(
        tabName = "map",
        withSpinner(leafletOutput("map_view"), type=8, color = '#FFFFFF')
      ),
      
      tabItem(
        tabName = "graphs",
        fluidRow(
          box(
            height = 600,
            title = "Evolution of Cases (Worldwide)",
            solidHeader = T,
            withSpinner(plotlyOutput("worldwide_evolution", height = '480px'), type=8, color = '#FFFFFF'),
            width = 6
          ),
          
          box(
            height = 600,
            title = "Evolution of Cases (Country wise)",
            solidHeader = T,
            selectizeInput(
              inputId = "country_selector",
              label = "Select Country:",
              choices = unique(merged_data$`Country/Region`),
              selected = "Bangladesh"
            ),
            withSpinner(plotlyOutput("countrywise_evolution"), type=8, color = '#FFFFFF'),
            width = 6
          ),
        ),
        
        fluidRow(
          box(
            title = "Comparing Numer of New Cases Among Countries",
            selectizeInput(
              inputId = "country_selector2",
              label = "Select countries to compare:",
              choices = unique(merged_data$`Country/Region`),
              multiple = T,
              selected = c("Bangladesh", "India", "Pakistan", "China")
            ),
            withSpinner(plotlyOutput("compare_new_cases"), type=8, color = '#FFFFFF'),
            width = 12
          )
        )
      ),
      
      tabItem(
        tabName = "about",
        fluidRow(
          column(
            width = 8,
            offset = 2,
            tags$div(
              id = "about-content",
              tags$h2("About the Developer"),
              tags$p("I am Wahiduzzaman Khan. I have completed my B.Sc. on statistics and currently studying for my Master's degree at Shahjalal University of Science and Technology."),
              tags$p("I am a self-motivated data analyst. I am interested in Data Analysis and Machine Learning. Seeking a full-time position in the field of data science and research, where I can apply my knowledge and skill for continuous improvement and impact on the company's growth."),
              tags$h4("Find Me"),
              tags$div(
                id = "social-media-buttons",
                socialButton(
                  href = "https://www.linkedin.com/in/wahiduzzaman-khan-2014134007",
                  icon = icon("linkedin")
                ),
                socialButton(
                  href = "https://github.com/WahiduzzamanKhan",
                  icon = icon("github-square")
                ),
                socialButton(
                  href = "https://www.facebook.com/chisty.khan",
                  icon = icon("facebook-square")
                )
              ),
              tags$h2("About This Dashboard"),
              tags$p("This dashboard is created by me as a toy project. It can help someone to get an idea about the Corona virus situation worldwide at a glance."),
              tags$h4("Data Sources"),
              tags$p("All data for this dashboard are from the official github repository of the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University.", tags$br(), tags$a(href="https://github.com/CSSEGISandData", "Click here to go to the repository")),
              tags$h4("Tools Used"),
              tags$p(
                "To create this dashboard the following tools have been used:",
                tags$ul(
                  tags$li("R Studio"),
                  tags$li("Shiny"),
                  tags$li("HTML"),
                  tags$li("CSS"),
                  tags$li("Leaflet")
                )
              )
            )
          )
        )
      )
    )
  ),
  title = "COVID-19 Live!!"
)