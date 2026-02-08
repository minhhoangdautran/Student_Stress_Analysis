library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)



shinyServer(function(input, output){
  df <- read.csv("https://raw.githubusercontent.com/minhhoangdautran/Student_Stress_Analysis/refs/heads/main/clustered_data/data_after_EDA.csv"
                                )

  # Manage Unread tabs
    # Track if the badge is visible, create a badge_status to track all badges
        badge_status <- reactiveValues(
          academic = TRUE, 
          social_envir  = TRUE, 
          health   = TRUE,
          raw = TRUE
        )
    # Watch the sidebar ID
        observeEvent(input$menu_selection, {
          # If the tabName clicked is 'academic', badge_status$academic becomes FALSE
          # This only works if tabName(s) match the names in reactiveValues (badge_status)!
          badge_status[[input$menu_selection]] <- FALSE
        })
    # Render Menu tab
        # Configuration for your 5 tabs
        tabs_config <- list(
          academic = list(label = "Academic", icon = "school"),
          health   = list(label = "Health",   icon = "heart-pulse"),
          social_envir   = list(label = "Social & Environmental",   icon = "bluesky"),
          raw   = list(label = "Raw Data",   icon = "table")
        )
        
        # Render menu output
        lapply(names(tabs_config), function(tab_id) {
          
          output[[paste0(tab_id,"_item")]] <- renderMenu ({
            conf <- tabs_config[[tab_id]]
            menuItem(tabName = tab_id, 
                     text = conf$label,
                     icon = icon(conf$icon),
                     badgeLabel = if (badge_status[[tab_id]]) "Unread" else NULL,
                     badgeColor = "red")
          })
          
        })
        
  # Manage Slicer
        filtered_df <- reactive({
          # 1. Start with the full dataset
          data <- df
          
          # 2. Check the input
          if (input$group_filter == "Overall") {
            # Do nothing, return the full dataset
            return(data)
          } else {
            # Extract the number (0, 1, or 2) from "Group X"
            group_num <- stringr::str_sub(input$group_filter, -1)
            
            # Return the filtered dataset
            return(data %>% filter(K3 == group_num))
          }
        })
        
       
  # --- Output for tab 1: overall ---      
    output$noClusters <- renderValueBox({ valueBox(
                                      value = length(unique(filtered_df()$K3)), 
                                      subtitle = "Number of clusters", 
                                      icon = icon("people-group"),
                                      color = "green")
                                      })
    output$noTotal_Students <- renderValueBox({
          valueBox(value=nrow(filtered_df()), subtitle = "Number of students participating", icon = icon("person"), color = "green")
                                              })
    output$avgDepression <- renderValueBox({
        valueBox(value = round(mean(filtered_df()$depression),2), 
                 subtitle = "Average depression level out of 27", 
                 icon = icon("face-sad-cry"),
                 color = "green")
                                            })
    output$avgAcademicPerformance <- renderValueBox({
        valueBox(value = round(mean(filtered_df()$academic_performance),2), subtitle="Average academic performance out of 5", icon = icon("school"),
                 color = "green")
                                                    })
    output$p_1_1 <- renderPlot({
      data_p11 <- filtered_df() %>%
        count(AcademicC) %>% # Count rows per cluster
        mutate(
          # Ensure ggplot follows this specific order
          prop = n / sum(n),
          # Calculate positions based on THIS new order
          percent = paste0(round(prop * 100), "%")
        ) 
      
      ggplot(data_p11, aes(x="", y = prop, fill = AcademicC )) +
        geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1.5) + 
        coord_polar("y", start = 0) +
        geom_text(aes(label = percent), 
                  position = position_stack(vjust=0.5), 
                  color = "black",
                  fontface = "bold",
                  size=5) +
        scale_fill_manual(values = c(purple, yellow, orange))+
        labs(title="Academic Performance", fill =NULL) +
        theme_void(base_family = "Verdana") +
        theme(
              plot.title = element_text(hjust = 0.5,  face="bold", color = "#434E78", size = 16))
    })
    output$p_1_2 <- renderPlot({
      data_p12 <- filtered_df() %>%
        count(BullyC) %>% # Count rows per cluster
        mutate(
          # Ensure ggplot follows this specific order
          prop = n / sum(n),
          # Calculate positions based on THIS new order
          percent = paste0(round(prop * 100), "%")
        ) 
      
      ggplot(data_p12, aes(x="", y = prop, fill = BullyC )) +
        geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1.5) + 
        coord_polar("y", start = 0) +
        geom_text(aes(label = percent), 
                  position = position_stack(vjust=0.5), 
                  color = "black",
                  fontface = "bold",
                  size=5) +
        scale_fill_manual(values = c(purple, yellow, orange,green))+
        labs(title="Bullying among participants", fill = "Bullying Level") +
        theme_void(base_family = "Verdana") +
        theme(
          plot.title = element_text(hjust = 0.5,  face="bold", color = blue, size = 16))
    })
    output$p_1_3 <- renderPlot({
      data_p13 <- filtered_df() %>%
        count(PoorSleepQuality) %>% # Count rows per cluster
        mutate(
          # Ensure ggplot follows this specific order
          prop = n / sum(n),
          # Calculate positions based on THIS new order
          percent = paste0(round(prop * 100), "%")
        ) 
      
      ggplot(data_p13, aes(x="", y = prop, fill = PoorSleepQuality )) +
        geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1.25) + 
        coord_polar("y", start = 0) +
        geom_text(aes(label = percent), 
                  position = position_stack(vjust=0.5), 
                  color = "black",
                  fontface = "bold",
                  size=5) +
        scale_fill_manual(values = c(purple, yellow, orange))+
        labs(title="Sleep Quality among participants", fill =NULL) +
        theme_void(base_family = "Verdana") +
        theme(
          plot.title = element_text(hjust = 0.5,  face="bold", color = blue, size = 16))
    })
    output$p_1_4 <- renderPlot({
      data_p14 <- filtered_df() %>%
        count(K3) %>% # Count rows per cluster
        mutate(
          # Ensure ggplot follows this specific order
          prop = n / sum(n),
          # Calculate positions based on THIS new order
          percent = paste0(round(prop * 100), "%")
        ) 
      ggplot(data_p14, aes(x="", y = prop, fill = factor(K3) )) +
        geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1.5) + 
        coord_polar("y", start = 0) +
        geom_text(aes(label = percent), 
                  position = position_stack(vjust=0.5), 
                  color = "black",
                  fontface = "bold",
                  size=5) +
        scale_fill_manual(values = c(purple, yellow, orange))+
        labs(title="# participants in each cluster", fill ="Group") +
        theme_void(base_family = "Verdana") +
        theme(
          plot.title = element_text(hjust = 0.5,  face="bold", color = blue, size = 16))
    })
    output$b_1_1 <- renderPlotly({
      data_b11 <- filtered_df() %>%
        count(depression) %>%
        rename(level = depression, count = n)
      
      plot_ly(data_b11, 
              x = ~level, 
              y = ~count, 
              type = 'bar', 
              # Conditional Formatting'
              marker = list(
                color = ~level,
                colorscale = list(c(0, green), c(1, red)),
                colorbar = list(title = "Intensity")
              )) %>% 
        layout(
          font = list(family="Verdana"),
          title = list(text = "<b>Depression level by participants</b>", font= list(color = blue)),
          xaxis = list(title = 'Depression level (0-27)', range(0,27), tickvals = seq(0,27,5)),
          yaxis = list(title = 'Total Count') # Adds some 'Padding' to the top
        )
    })
    output$b_1_2 <- renderPlotly({
      plot_ly(filtered_df(), x = ~stress_level, type = "histogram",
              marker = list(color = purple),
              texttemplate = "%{y}", 
              textposition = "outside") %>%
        layout(bargap = 0.3,
               margin = list(t=70),
               font = list(family = "Verdana"),
               title = list(text = "<b>Stress Level among participants</b>", y = 0.97, font = list(color = blue)),
               xaxis = list(title = "Stress Level", dtick = 1),
               yaxis = list(title = "Count", range = c(0,420)))
      
    })
    output$b_1_3 <- renderPlotly({
      # Group by two variables (basic needs and stress_level)
      data_b13 <- filtered_df() %>%
        count(basic_needs, stress_level) %>%
        rename(cluster = basic_needs, group = stress_level, count = n)
      
      plot_ly(data_b13, 
              x = ~cluster, 
              y = ~count, 
              color = ~as.factor(group), # creates the 'stacks'
              colors = c(purple, yellow, orange),
              type = 'bar') %>%
        layout(
          barmode = 'stack', # CRITICAL: This stacks the bars instead of clustering them
          title = list(text =  "<b>Basic Needs effect on Stress Level</b>", font = list(color = blue)),
          legend = list(title = list(text = "<b>Stress Level</b>")),
          xaxis = list(title = "Basic need level (worst at 0)", dtick =1),
          yaxis = list(title = "Total Count")
        )
    })
                                             
  # --- Output for tab 2: academic ---  
    output$academic_standing_box <- renderValueBox({
        valueBox(value = scales::percent(nrow(filtered_df()[filtered_df()$AcademicC == "Excellent Performance",])/nrow(filtered_df()), accuracy = 0.1), 
                 subtitle = "Students with Excellent Performace", 
                 icon = icon("trophy"),
                 color = "green")
                                            })
    output$pp_cc_box <- renderValueBox({
        valueBox(value = scales::percent(nrow(filtered_df()[filtered_df()$FuturecareerC=="High concern" & filtered_df()$PeerpressureC == "Intense Peer Pressure",])/nrow(filtered_df()), accuracy = 0.1), 
                 subtitle = "Students have intense peer pressure and high future career concerns",
                 icon = icon("briefcase"),
                 color = "green")
  })
    output$teaStuRel_box <- renderValueBox({
      valueBox(value = scales::percent(nrow(filtered_df()[filtered_df()$teacher_student_relationship >= 3,])/nrow(filtered_df()), accuracy = 0.1), 
               subtitle = "Students having Good/Great relationship with their teachers/professors", 
               icon = icon("chalkboard-user"),
               color = "green")
    })
    
    output$b_2_1 <- renderPlotly({
      # Group by two variables (performance and activities)
      data_b21 <- filtered_df() %>%
        count(extracurricular_activities, AcademicC) %>%
        rename(cluster = extracurricular_activities , group = AcademicC, count = n) %>%
        group_by(cluster) %>%
        mutate(
          percent = count/sum(count) *100
        ) %>%
        ungroup()
      
      plot_ly(data_b21, 
              y = ~cluster, 
              x = ~percent,
              orientation = "h",
              color = ~as.factor(group), # creates the 'stacks'
              colors = c(purple, yellow, orange),
              type = 'bar',
              height = 250) %>%
        layout(
          margin = list(t=60),
          barmode = 'stack', 
          title = list(text =  "<b>Extracurricular Activities' impacts on Academic</b>", font = list(color = blue)),
          legend = list(title = list(text = "<b>Academic</b>")),
          yaxis = list(title = "Activities involved (least at 0)", dtick =1),
          xaxis = list(
            title = "Percentage",
            ticksuffix = "%",
            range = c(0, 100)
          )
        )
    })
    
    output$b_2_2 <- renderPlotly({
      # Group by two variables (study load and academic)
      data_b22 <- filtered_df() %>%
        count(study_load, AcademicC) %>%
        rename(cluster = study_load , group = AcademicC, count = n) %>%
        group_by(cluster) %>%
        mutate(
          percent = count/sum(count) *100
        ) %>%
        ungroup()
      
      plot_ly(data_b22, 
              y = ~cluster, 
              x = ~percent,
              orientation = "h",
              color = ~as.factor(group), # creates the 'stacks'
              colors = c(purple, yellow, orange),
              type = 'bar',
              height = 250) %>%
        layout(
          margin = list(t=60),
          barmode = 'stack', 
          title = list(text =  "<b>Study Load' impacts on Academic</b>", font = list(color = blue)),
          legend = list(title = list(text = "<b>Academic</b>")),
          yaxis = list(title = "Study Load (lightest at 0)", dtick =1),
          xaxis = list(
            title = "Percentage",
            ticksuffix = "%",
            range = c(0, 100)
          )
        )
    })
    
    output$b_2_3 <- renderPlotly({
      # Group by two variables (relationship and stress level)
      data_b23 <- filtered_df() %>%
        count(teacher_student_relationship, stress_level) %>%
        rename(cluster =  teacher_student_relationship, group = stress_level, count = n)
      
      plot_ly(data_b23, 
              x = ~cluster, 
              y = ~count,
              color = ~as.factor(group), # creates the 'stacks'
              colors = c(purple, yellow, orange),
              type = 'bar',
              height = 350) %>%
        layout(
          margin = list(t=50),
          barmode = 'stack', 
          title = list(text =  "<b>How relationships with teachers stress students?</b>", font = list(color = blue)),
          legend = list(title = list(text = "<b>Stress_level</b>")),
          xaxis = list(title = "Relationship with teachers (best at 5)", dtick =1),
          yaxis = list(
            title = "Count"
          )
        )
    })
    
    output$box_2_4 <- renderPlotly({
      
      plot_ly(
        data = filtered_df(),
        x = ~factor(study_load),
        y = ~depression,
        type = "box",
        boxpoints = "outliers",
        color = ~factor(study_load),
        colors = c(purple, yellow, orange, blue, green, red, pink),
        height = 350
      ) %>%
        layout(
          title = list(
            text = "<b>Depression Score by Study Load</b>",
            font = list(color = blue)),
          xaxis = list(
            title = "Study Load (0 = lowest, 5 = highest)"),
          yaxis = list(
            title = "Depression Score",
            range = c(0, 30)
          ),
          margin = list(l = 60, r = 20, t = 50)
        )
    })
  # --- Output for tab 3:health ---
    output$pct_ax_less_21 <- renderValueBox({
      valueBox(value = scales::percent(nrow(filtered_df()[filtered_df()$anxiety_level <= 21,])/nrow(filtered_df()), accuracy = 0.1), subtitle = "Students have anxiety level lower than 21 (moderate level)", icon = icon("face-frown"))
    })
    
    output$pct_MHP <- renderValueBox({
      valueBox(value = scales::percent(nrow(filtered_df()[filtered_df()$mental_health_history==1,])/nrow(filtered_df()), accuracy = 0.1), 
               subtitle = "Students have mental health problem history",
               icon = icon("book"))
    })
})