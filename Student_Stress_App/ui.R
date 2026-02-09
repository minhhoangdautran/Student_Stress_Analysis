library(shiny)
library(shinydashboard)
library(plotly)
library(fresh)
library(bslib)

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
    # Pre-set format
    tags$head(
      tags$style(HTML("
    /* Target BOTH old box titles and new card headers */
    .box-title, .card-header {
      display: flex !important;
      justify-content: center !important;
      align-items: center !important;
      font-weight: 800 !important; /* Extra bold */
      text-align: center !important;
      color: #434E78 !important;
      width: 100%;
      font-family: 'Verdana', sans-serif !important;
    }
    .small-box {
      background-color: #C5D89D !important;
      color: white !important;} /* Sets text color to your dark blue for contrast */
    
    .nav-tabs-custom > .nav-tabs > li > a {
      font-family: 'Verdana', sans-serif;
      font-size: 15px;
      color: #434E78;
      font-weight: 600;
    }

    .nav-tabs-custom > .nav-tabs > li.active > a {
      color: #A2AADB;
    }
}
  "))
    )
    
    ,
    
    tabItems(
      tabItem(tabName = "overall",
              fluidRow(
                valueBoxOutput("noClusters", width =3),
                valueBoxOutput("noTotal_Students", width =3),
                valueBoxOutput("avgDepression", width =3),
                valueBoxOutput("avgAcademicPerformance", width =3)
                
              ),
              fluidRow(
                box(title = "Number of student per cluster",
                    width = 3, 
                    plotOutput("p_1_1", height = "250px")),
                box(title = "Bullying among participants",
                    width = 3, 
                    plotOutput("p_1_2", height = "250px")),
                box(title = "Sleep Quality among participants", 
                    width = 3, 
                    plotOutput("p_1_3", height = "250px")),
                box(title = "Academic Performace",
                    width = 3, 
                    plotOutput("p_1_4", height = "250px"))
              ),
              fluidRow(
                box(width = 4, plotlyOutput("b_1_1", height = "350px"),
                    title = "Depression Level by Participants"),
                box(width = 4, plotlyOutput("b_1_2", height = "350px"),
                    title = "Stress Level Among participants"),
                box(width = 4, plotlyOutput("b_1_3", height = "350px"),
                    title = "Basic Need Effects on Stress Level")
                
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
                valueBoxOutput("pct_ax_less_21", width =4),
                valueBoxOutput("pct_MHP", width=4),
                valueBoxOutput("poor_sleep", width=4)),
              fluidRow(
                box(plotlyOutput("b_3_1"), width = 6, height = "450px",
                    title = "How past mental problem relates to depression level?"
                    ),
                box(plotlyOutput("b_3_2"), width = 6, height = "450px",
                    title = "How sleep quality relates to stress level?")
              ),
              fluidRow(
                box(plotlyOutput("b_3_3"), width = 6, height = "480px",
                    title = "Correlation between self-esteem and depression level"),
                tabBox(
                  title = "",
                  id = "tabset2",
                  width = 6, # This controls the outer size on the screen
                  tabPanel("Blood Pressure vs. Stress",
                           status = "primary",
                           solidHeader = T,
                           fluidRow(
                             column(
                               width = 12,
                               box(title = "Blood Pressure vs. Stress",
                                   width = 12,
                                   height = "350px",
                                   plotlyOutput("b_3_4"))
                             ))),
                  tabPanel("Breathing vs. Stress", 
                           solidHeader = T,
                           fluidRow(
                             column(
                               width = 12,
                               box(title = "Breathing problems relate to stress level?",
                                   width = 12, 
                                   height = "350px",
                                   plotlyOutput("b_3_5"))
                             ))),
                  tabPanel("Headache vs. Stress", 
                           solidHeader = T,
                           fluidRow(
                             column(
                               width = 12,
                               box(title = "Headache relates to stress level?",
                                   width = 12, 
                                   height = "350px",
                                   plotlyOutput("b_3_6"))
                             )))
                )
              )),
      tabItem(tabName = "academic",
              fluidRow(
                valueBoxOutput("academic_standing_box", width = 4),
                valueBoxOutput("pp_cc_box", width = 4),
                valueBoxOutput("teaStuRel_box", width = 4)
              ),
              fluidRow(
                box(width = 6, plotlyOutput("b_2_1"), height = "300px",
                    title = "Extracurricular Activities' impacts on Academic"),
                box(width = 6, plotlyOutput("b_2_2"), height = "300px",
                    title = "Study Load' impacts on Academic")
                
              ),
              fluidRow(
                box(width = 5, plotlyOutput("b_2_3"), height = "420px",
                    title = "How relationships with teachers stress students?"),
                box(width = 7, plotlyOutput("box_2_4"), height = "420px",
                    title = "Depression Score by Study Load")
              )),
      tabItem(tabName = "social_envir",
              fluidRow(
                valueBoxOutput("pct_bullied", width = 4),
                valueBoxOutput("bad_condition", width = 4),
                valueBoxOutput("low_safety", width = 4)
              ),
              fluidRow(
                box(title = "How bullied students get social support?",
                    plotlyOutput("b_4_1"),
                    height = "400px",
                    width = 5),
                tabBox(
                  title = "",
                  id = "tabset1",
                  width = 7, # This controls the outer size on the screen
                  tabPanel("Living Condition vs. Stress",
                           status = "primary",
                           solidHeader = T,
                           fluidRow(
                             column(
                               width = 12,
                               box(title = "How living condition correlates with stress level",
                                   width = 12, 
                                   plotlyOutput("b_4_2"))
                             ))),
                  tabPanel("Noise vs. Stress", 
                           solidHeader = T,
                           fluidRow(
                             column(
                               width = 12,
                               box(title = "How noise levels correlates with stress level",
                                   width = 12, 
                                   plotlyOutput("b_4_3"))
                             )))
                           )
                
              ))
      )
))