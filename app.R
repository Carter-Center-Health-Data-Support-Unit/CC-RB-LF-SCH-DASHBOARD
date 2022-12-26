rm(list = ls())

library(shiny)
library(shinyWidgets)
library(tidyverse)




# call target
source("_targets.R")

#make target
tar_make()

# load data
tar_load(RB_pre_post_compiled) ### pre and post admin 2 level data




######### UI ###############

ui <- fluidPage(

    # Styling -----------------------------------------------------------------

    tags$head(
      HTML('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />'), includeCSS("style.css")
    ),
    #

    navbarPage(

      windowTitle = "CC-RB-SCH DASHBOARD",
      HTML('<a style="padding-left:10px;" class = "navbar-brand" href = "https://www.cartercenter.org" target="_blank"><img src = "logo.png" height = "50"></a><span class="navbar-text" style="font-size: 16px; color: #FFFFFF"><strong>ETHIOPIA CC-RB-LF-SCH DASHBOARD</strong></span>'),

      tabPanel("DASHBOARD",

               column(width = 12,
                 br(),
                 h3("aaaa"),

                 tags$div(pickerInput("select_admin2",
                                      label = "Select Admin 2:",
                                      choices = RB_pre_post_compiled$adm2_name %>% unique() %>% dput(),
                                      selected = (RB_pre_post_compiled$adm2_name %>% unique() %>% dput())[1],
                                      multiple = F,
                                      options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                 ),style="display:inline-block"),

                      br(),
                      h3("sdsd"),
                      plotOutput("graph_monthly_treated", height = "300px"),
                      plotOutput("grpah_treatment_by_round", height = "300px")


               ) ## end column


      ) ## END TAB 1

    )  ########## navarpage


  ) ## fludpage


################### server ################
server <- function(input, output,session){

   admin2_name <- reactive({input$select_admin2})

   admin_2_level_data_filter <-  reactive({RB_pre_post_compiled %>% dplyr::filter(adm2_name == admin2_name())})


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
     geom_line()
   })


}

######## Run the app #########

shinyApp(ui = ui, server = server)

