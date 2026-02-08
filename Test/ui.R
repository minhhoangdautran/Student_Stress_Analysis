library(shiny)
library(shinydashboard)
library(plotly)
library(fresh)

# Set some colors for the app
my_theme <- create_theme(
  adminlte_color(
    green = "#C5D89D", # green pastel
    red = "#EA7B7B",
    blue = "#434E78",
    yellow = "#F8FAB4",
    orange = "#FFC7A7",
    purple = "#A2AADB"
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Student Stress Analysis"),
  dashboardSidebar(
    #size of sidebar
    width = 300,
    
    # Side Bar menu is for return to the previous page problem
    sidebarMenu(id = "menu_selection",
      
      # sidebarSearchForm("searchText","buttonSearch","Search"),
      menuItem("Overall", tabName="overall", icon=icon("star")),
      menuItemOutput("academic_item"),
      menuItemOutput("health_item"),
      menuItemOutput("social_envir_item"),
      menuItemOutput("raw_item"),
      # The Slicer
      selectInput("group_filter", "Choose clusters to see details:", 
                  choices = c("Group 0","Group 1", "Group 2", "Overall"), 
                  selected = "Overall")
    )),
  dashboardBody(
    use_theme(my_theme),
    tabItems(
      tabItem(tabName = "overall",
              fluidRow(
                valueBoxOutput("noClusters", width =3),
                valueBoxOutput("noTotal_Students", width =3),
                valueBoxOutput("avgDepression", width =3),
                valueBoxOutput("avgAcademicPerformance", width =3)
                
              ),
              fluidRow(
                box(width = 3, plotOutput("p_1_4", height = "250px")),
                box(width = 3, plotOutput("p_1_2", height = "250px")),
                box(width = 3, plotOutput("p_1_3", height = "250px")),
                box(width = 3, plotOutput("p_1_1", height = "250px"))
              ),
              fluidRow(
                box(width = 4, plotlyOutput("b_1_1", height = "350px")),
                box(width = 4, plotlyOutput("b_1_2", height = "350px")),
                box(width = 4, plotlyOutput("b_1_3", height = "350px"))
                
              ),
              hr(),
              wellPanel(
                h3("Dashboard Conclusions", style = "font-family: Verdana; font-weight: bold; color: #434E78;"),
                tags$ul(
                  tags$li(strong("Cluster 0:"), " This group shows the highest stress levels with high depression scores."),
                  tags$li(strong("Cluster 1:"), " Participants in this cluster have low stress levels along with the good academic standing, and other mental health issues."),
                  tags$li(strong("Cluster 2:"), " This is the largest group (44%), with average stress levels, which requires more attention to mental health problem than group 0, but less than group 1.")
                )
              )),
      tabItem(tabName = "health",
              fluidRow(
                valueBoxOutput("pct_ax_less_21", width =2),
                valueBoxOutput("pct_MHP", width=2))),
      tabItem(tabName = "academic",
              fluidRow(
                valueBoxOutput("academic_standing_box", width = 4),
                valueBoxOutput("pp_cc_box", width = 4),
                valueBoxOutput("teaStuRel_box", width = 4)
              ),
              fluidRow(
                box(width = 6, plotlyOutput("b_2_1"), height = "300px"),
                box(width = 6, plotlyOutput("b_2_2"), height = "300px"),
                
              ),
              fluidRow(
                box(width = 4, plotlyOutput("b_2_3"), height = "400px"),
                box(width = 8, plotlyOutput("box_2_4"), height = "400px")
              )),
      tabItem(tabName = "social_envir",
              fluidRow()),
      tabItem(tabName="raw",
              fluidRow()
      
      )
      
    ))
)