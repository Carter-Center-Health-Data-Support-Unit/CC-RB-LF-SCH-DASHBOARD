rm(list = ls())

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(targets)
library(lubridate)
options(scipen = 999)

# load data
tar_load(RB_pre_post_compiled) ### pre and post admin 2 level data



RB_pre_post_compiled <- RB_pre_post_compiled %>% mutate(
  percentage_treated = round(popn_treated_during_current_month/total_population*100)
)



region_cols <-c("month","year","date", "adm1_name","cumulatative_target","cumulatative_percentage_treated",
                "popn_treated_during_current_month", "utg_treatment_target_for_each_round","percentage_treated",
                "utg_2_treatment_target_for_the_whole_year", "total_popn_projected",
                "popn_treated_round1", "popn_treated_round2", "popn_treated_cumulative_all_rounds",
                "active_villages_for_the_year", "villages_treated_during_current_month",
                "villages_treated_round_1", "villages_treated_round_2", "total_popn_census",
                "total_population", "percent_utg_treated_round_1", "percent_utg_2_treated_all_rounds",
                "percent_popn_treated_round_1", "percent_active_villages_treated_for_round_1",
                "percent_active_villages_treated_for_round_2",
                "percent_popn_treated_round_2")






RB_pre_post_compiled$month <- month(RB_pre_post_compiled$month,label = T)
RB_pre_post_compiled$year <- factor(RB_pre_post_compiled$year)

RB_pre_post_compiled <- RB_pre_post_compiled %>% group_by(year) %>%
  mutate(
    cumulatative_target = cumsum(popn_treated_during_current_month),
    cumulatative_percentage_treated = cumsum(percentage_treated)) %>% ungroup()

region_df <- RB_pre_post_compiled %>% select(region_cols) %>% group_by(date,month,year,adm1_name) %>%
  summarise_all(sum) %>% ungroup()







######### UI ###############

ui <- fluidPage(

  # Styling -----------------------------------------------------------------

  tags$head(
    HTML('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />'), includeCSS("style.css")
  ),
  #

  navbarPage(

    windowTitle = "CC-RB-SCH DASHBOARD",
    HTML('<a style="padding-left:20px;" class = "navbar-brand" href = "https://www.cartercenter.org" target="_blank"><img src = "logo.png" height = "46"></a><span class="navbar-text" style="font-size: 16px; color: #FFFFFF"><strong>ETHIOPIA CC-RB-LF-SCH DASHBOARD</strong></span>'),

    tabPanel("Admin Level info!",


             column(width = 6,
                    br(),

                    h3("Region level Info"),
                    tags$div(pickerInput("select_admin1",
                                         label = "Select Region (Admin 1):",
                                         choices = RB_pre_post_compiled$adm1_name %>% unique() %>% dput(),
                                         selected = (RB_pre_post_compiled$adm1_name %>% unique() %>% dput())[1],
                                         multiple = F,
                                         options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                    ),style="display:inline-block"),
                    hr(),

                    h4("Treatment over time by zone"),
                    plotOutput("graph_monthly_region", height = "300px"),

                    h4("Yearly Cumulatitative by zone"),
                    plotOutput("cumulative_region_year", height = "300px")



             ), ## end column 1

             column(width = 6,
                    br(),
                    h3("Zone level Info"),

                    tags$div(pickerInput("select_admin2",
                                         label = "Select Zone (Admin 2):",
                                         choices = NULL,
                                         selected = NULL,
                                         multiple = F,
                                         options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                    ),style="display:inline-block"),

                    br(),
                    hr(),
                    h4("Treatment over time by zone"),
                    plotOutput("graph_monthly_treated", height = "300px"),


                    hr(),
                    h4("Percentage treated [SOMETHING IS WRONG]"),
                    plotOutput("cum_percentage_treated", height = "300px"),

                    # h4("Treatment over time by round"),
                    # plotOutput("grpah_treatment_by_round", height = "300px"),

                    h4("Cumulative treatment over time(original)"),
                    plotOutput("grpah_treatment_cuma", height = "300px"),

                    h4("Cumulative treatment over time(recalculated)"),
                    plotOutput("grpah_treatment_cuma_cal", height = "300px")



             ) ## end column


    ), ## END TAB 1

    ### start tab 2
    tabPanel("Current Situation Monitoring",
             column(width = 12,
                    br(),
                    h4("Comming soon!")

                    )



    ) ## end tab 2







  )  ########## navarpage


) ## fludpage


################### server ################
server <- function(input, output,session){

  admin1_name <- reactive({input$select_admin1})
  admin_1_level_data_filter <-  reactive({RB_pre_post_compiled %>% dplyr::filter(adm1_name == admin1_name())})


  ####################### available district name in the selected governorate ############

  available_adm2 <- reactive({admin_1_level_data_filter()$adm2_name %>% unique()})

  observe({
    updatePickerInput(session, "select_admin2", choices = available_adm2())
  })



  admin2_name <- reactive({input$select_admin2})
  admin_2_level_data_filter <-  reactive({admin_1_level_data_filter() %>% dplyr::filter(adm2_name == admin2_name())})



  output$graph_monthly_treated<- renderPlot({
    ggplot(data=admin_2_level_data_filter(),
           aes(x=admin_2_level_data_filter()$date,
               y=admin_2_level_data_filter()$popn_treated_during_current_month)) +
      geom_bar(stat="identity")+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Date")+ ylab("Population treated (Monthly)")



  }) ## monthly treated graph



  ### treatement by round graph

  df_line <- reactive({admin_2_level_data_filter() %>% select(date, adm1_name,adm2_name,popn_treated_round1,
                                                              popn_treated_round2) %>%
      pivot_longer(cols = !c("date","adm1_name","adm2_name"),names_to = "variable",values_to = "value")

  })


  output$grpah_treatment_by_round<- renderPlot({ggplot(data=df_line(),
                                                       aes(x=date, y=value, colour=variable)) +
      geom_line()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Date")+ ylab("Population treated by each round")
  })

  #### population treated cumalitative

  output$grpah_treatment_cuma<- renderPlot({
    ggplot(data=admin_2_level_data_filter(),
           aes(x=month, y=popn_treated_cumulative_all_rounds,color = year,group= year)) +
      geom_line()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Month")+ ylab("Population treated (Cumulative[original])")
  })



  ###### cumulatative graph
  output$grpah_treatment_cuma_cal<- renderPlot({
  ggplot(admin_2_level_data_filter(), aes(x=month, y=cumulatative_target,color = year,group= year)) +
    geom_line()+
          theme(panel.background = element_rect(fill = "white",
                                                colour = "black",
                                                size = 0.5, linetype = "solid")) +
          xlab("Month")+ ylab("Population treated (Cumulative[calculated])")
      })



  ###### cumulative graph percentage treated
  output$cum_percentage_treated<- renderPlot({
    ggplot(admin_2_level_data_filter(),
           aes(x=month, y=percentage_treated,color = year,group= year)) +
      geom_line()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Month")+ ylab("% treated")
  })

  ################################################## start::rgion ######################



  graph_data <- reactive({region_df %>% filter(adm1_name == admin1_name())})

  output$graph_monthly_region<- renderPlot({
    ggplot(data=graph_data(),
           aes(x=graph_data()$date,
               y=graph_data()$popn_treated_during_current_month)) +
      geom_bar(stat="identity")+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Date")+ ylab("Population treated (Monthly)")



  }) ## monthly treated graph



  output$cumulative_region_year<- renderPlot({


      ggplot(graph_data(), aes(x=month, y=cumulatative_target,color = year,group= year)) +
        geom_line()+
        theme(panel.background = element_rect(fill = "white",
                                              colour = "black",
                                              size = 0.5, linetype = "solid")) +
        xlab("Month")+ ylab("Population treated (Cumulative[calculated])")


  }) ## monthly treated graph





  ################################################ END::rgion ###############################


} # end server

######## Run the app #########

shinyApp(ui = ui, server = server)

