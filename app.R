rm(list = ls())

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(targets)
library(lubridate)
library(DT)
library(sf)
library(htmltools)
library(snakecase)
library(leaflet)
# load internal funcs
invisible(purrr::map(dir(here::here("R/"),full.names = T),~source(.x,verbose = FALSE,echo = F)))

options(scipen = 999)

# load data
tar_load(RB_pre_post_compiled) ### pre and post admin 2 level data

#### fixing Admin 2 boundary name


### read ocha boundary

admin_zero <- st_read("data/shapefile/eth_admbnda_adm0_csa_bofedb_itos_2021.shp") |>
  janitor::clean_names() |>
  select(adm0_en, adm0_pcode) |>
  sanitize_admins()

admin1_boundary<- st_read("data/shapefile/eth_admbnda_adm1_csa_bofedb_2021.shp") |>
  janitor::clean_names() |>
  select(adm0_en, adm0_pcode,adm1_en, adm1_pcode) |>
  sanitize_admins()

admin2_boundary<- st_read("data/shapefile/eth_admbnda_adm2_csa_bofedb_2021.shp") |>
  janitor::clean_names() |>
  select(adm0_en, adm0_pcode,adm1_en, adm1_pcode, adm2_en, adm2_pcode) |>
  sanitize_admins()




#%>% mutate(
  # ADM2_EN = case_when(ADM2_EN == "Itang Special woreda" ~ "Itang Special Woreda",T~ ADM2_EN)
# )

######################################### START::Fix admin 1 Name ############################
# admin2_boundary$ADM1_EN %>% unique()

# RB_pre_post_compiled <- RB_pre_post_compiled %>% mutate(
#   adm1_name = to_title_case(adm1_name)
# ) %>% mutate(
#   adm1_name = case_when(adm1_name == "Snnp" ~ "SNNP",T~adm1_name)
# )

######################################### END::Fix admin 1 Name ############################

######################################### START::Fix admin 2 Name ############################
# RB_pre_post_compiled$adm2_name <- RB_pre_post_compiled$adm2_name %>% snakecase::to_title_case()

# RB_pre_post_compiled <- RB_pre_post_compiled %>% mutate(
#   adm2_name = case_when(adm2_name == "North Shewa Am" ~ "North Shewa",
#                         adm2_name == "North Shoa 2" ~ "North Shewa",
#                         adm2_name == "North Shewa or" ~"North Shewa",
#                         T ~  adm2_name)
# ) %>% mutate(
#   adm2_name = case_when(adm1_name == "Amhara" & adm2_name == "North Shewa" ~ "North Shewa (AM)",
#                         adm1_name == "Oromia" & adm2_name == "North Shewa" ~ "North Shewa (OR)",
#                         T~adm2_name)
# )

######################################### END::Fix admin 2 Name ############################


region_cols <-c("month","year","date", "adm1_name","cumulatative_target",
                "popn_treated_during_current_month", "utg_treatment_target_for_each_round",
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

RB_pre_post_compiled <- RB_pre_post_compiled %>%
  group_by(adm2_name,year) %>%
  mutate(cumulatative_target = cumsum(popn_treated_during_current_month)) %>%
  ungroup()

RB_pre_post_compiled <- RB_pre_post_compiled %>% mutate(
  cum_percentage_treated = round(cumulatative_target/total_population*100),
  cum_percentage_utg = round(cumulatative_target/utg_treatment_target_for_each_round*100)

)

region_df <- RB_pre_post_compiled %>%
  select(all_of(region_cols)) %>%
  group_by(date,month,year,adm1_name) %>%
  summarise(
    across(everything(),~sum(.x,na.rm = T)),
    .groups = "drop"
  )


region_df <- region_df %>%
  group_by(adm1_name,year) %>%
  mutate(cumulatative_target = cumsum(popn_treated_during_current_month)) %>%
  ungroup()

region_df <- region_df %>% mutate(
  cum_percentage_treated = round(cumulatative_target/total_population*100),
  cum_percentage_utg = round(cumulatative_target/utg_treatment_target_for_each_round*100)

)

pop_data <- RB_pre_post_compiled %>% select(adm2_name,adm1_name,year,total_population,
                                            popn_treated_during_current_month,
                                            utg_treatment_target_for_each_round,
                                            utg_2_treatment_target_for_the_whole_year
)

## max_sum pop

pop_data_max_sum <- pop_data %>% group_by(year,adm1_name,adm2_name) %>%
  summarise(
  population_max = max(total_population,na.rm = T),
  population_mean = mean(total_population,na.rm = T),
  population_sum = sum(total_population,na.rm = T),
  population_median = median(total_population,na.rm = T),
  utg_target_max = max(utg_2_treatment_target_for_the_whole_year,na.rm = T),
  utg_target_mean = max(utg_2_treatment_target_for_the_whole_year,na.rm = T),
  utg_target_median = median(utg_2_treatment_target_for_the_whole_year,na.rm = T),
  utg_target_sum = max(utg_2_treatment_target_for_the_whole_year,na.rm = T)

)

# region_df <- region_df %>% mutate(
#   cum_percentage_treated = round(cumulatative_target/total_population*100),
#   cum_percentage_utg = round(cumulatative_target/utg_treatment_target_for_each_round*100)
#
# )



############# START:: CURRENT DATA:: FOR CURRENT SITUATION MONITORING TAB ######
latest_date <- max(RB_pre_post_compiled$date)
current_data <- RB_pre_post_compiled %>%
  filter(date == latest_date) %>%
  mutate(
  treated_vs_target = round(popn_treated_during_current_month/utg_treatment_target_for_each_round*100,2)
)


current_data <- current_data %>% distinct(adm2_name,.keep_all = T) ### to remove

spatial_data <- admin2_boundary %>%
  select(adm2_name= adm2_en)  %>% left_join(current_data)

current_monitoring_title = glue::glue("{lubridate::month(latest_date,label=T, abbr=F)} {lubridate::year(latest_date)} - Current Program Status ")

plot_heatchart <-  heat_map_gg(.dat =RB_pre_post_compiled,date_col = "date" )

## preparing base map

# leaflet_map -------------------------------------------------------------

mypalette <- colorNumeric( palette="viridis", domain=spatial_data$treated_vs_target, na.color="#58585A")


base_map <- leaflet::leaflet() %>%
  leaflet::addProviderTiles(providers$CartoDB.Positron) %>%
  leaflet::addPolygons(data = admin_zero,color = "#EE5859",fillColor = "transparent") %>%
  leaflet::addPolygons(data = admin1_boundary,color = "#58585A",
                       weight = 2,dashArray = "12",fillColor = "transparent") %>%
  leaflet::addPolygons(data = spatial_data,color = "#58585A",
                       popup = case_when( is.na(spatial_data$treated_vs_target) ~ "Non assessed", T~
                                            paste("Region:", spatial_data$adm1_name, "<br>",
                                                  "Zone:", spatial_data$adm2_name, "<br>",
                                                  "Target achived", paste0(spatial_data$treated_vs_target),"%")),


                       label = ~htmlEscape(case_when( is.na(spatial_data$treated_vs_target) ~ "",T~
                                                        paste0(adm2_name,"(",spatial_data$treated_vs_target, "%)"))),
                       labelOptions = labelOptions(noHide = F,
                                                   direction = 'center',
                                                   textOnly = T,
                                                   style = list(
                                                     "font-family" = "serif",
                                                     "font-size" = "15px",
                                                     "font-weight" =  "bold"
                                                   )),

                       weight = 1.5,dashArray = "12",fillColor = ~mypalette(treated_vs_target) ) #%>% setView(zoom = 6)

############# END:: CURRENT DATA:: FOR CURRENT SITUATION MONITORING TAB ######

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
    tabPanel("Overview",
             column(
               width= 12,
               h3("% Ultimate Treatment Goal by Month and Zone"),
               ggiraph::ggiraphOutput("heat_chart",
                                      width = "100%",height="500px")
             )
             )
    ,

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
                    plotOutput("cumulative_region_year", height = "300px"),

                    h4("Percentage treated, out of total population"),
                    plotOutput("cum_percentage_treated_r", height = "300px"),

                    h4("Percentage treated, out of UTG targets"),
                    plotOutput("cum_percentage_treated_utg_r", height = "300px")





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

                    h4("Percentage treated, out of total population"),
                    plotOutput("cum_percentage_treated", height = "300px"),

                    h4("Percentage treated, out of UTG targets"),

                    plotOutput("cum_percentage_treated_utg", height = "300px"),

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
                    h4(current_monitoring_title),
                    hr(),
                    div(class = "outer",
                        tags$style(type = "text/css", ".outer {
                    position: fixed;
                    top: 102px;
                    left: 0;
                    right: 0;
                    bottom: 0;
                    overflow: hidden;
                    padding: 0}"),
                    leafletOutput("map",height = "100%"))#,



                    # h4("Comming soon!")

             )



    ) ,## end tab 2,
    tabPanel("Explore population!",
             h3("coming soon")

             # tags$div(pickerInput("select_admin1_1",
             #                      label = "Select Region (Admin 1):",
             #                      choices = RB_pre_post_compiled$adm1_name %>% unique() %>% dput(),
             #                      selected = (RB_pre_post_compiled$adm1_name %>% unique() %>% dput())[1],
             #                      multiple = F,
             #                      options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
             # ),style="display:inline-block"),
             #
             #
             # tags$div(pickerInput("select_admin2_1",
             #                      label = "Select Zone (Admin 2):",
             #                      choices = NULL,
             #                      selected = NULL,
             #                      multiple = F,
             #                      options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
             # ),style="display:inline-block"),
             # hr(),
             #
             # h4("Population::Box plot"),
             # plotOutput("population_box", height = "300px"),
             #
             # h4("Population (max) vs target"),
             # plotOutput("population_graph", height = "300px"),
             # hr(),
             # h4("Population (mean) vs target"),
             # plotOutput("population_graph_mean", height = "300px"),
             #
             # hr(),
             # h4("Population (median) vs target"),
             # plotOutput("population_graph_median", height = "300px"),
             #
             #
             # hr(),


             # DTOutput("pop_table"), br(),

    ) ## end tab 1








  )  ########## navarpage


) ## fludpage


################### server ################
server <- function(input, output,session){
  output$heat_chart <- ggiraph::renderggiraph({
    ggiraph::girafe(ggobj = plot_heatchart,width_svg = 16, height_svg =5)
  })
  admin1_name <- reactive({input$select_admin1})
  admin1_name_1 <- reactive({input$select_admin1_1})

  admin_1_level_data_filter <-  reactive({RB_pre_post_compiled %>% dplyr::filter(adm1_name == admin1_name())})


  ####################### available district name in the selected governorate ############

  available_adm2 <- reactive({admin_1_level_data_filter()$adm2_name %>% unique()})

  observe({updatePickerInput(session, "select_admin2", choices = available_adm2())})
  observe({updatePickerInput(session, "select_admin2_1", choices = available_adm2())})


  admin2_name <- reactive({input$select_admin2})
  admin2_name_1 <- reactive({input$select_admin2_1})

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
  #
  #   df_line <- reactive({admin_2_level_data_filter() %>% select(date, adm1_name,adm2_name,popn_treated_round1,
  #                                                               popn_treated_round2) %>%
  #       pivot_longer(cols = !c("date","adm1_name","adm2_name"),names_to = "variable",values_to = "value")
  #
  #   })
  #
  #
  #   output$grpah_treatment_by_round<- renderPlot({ggplot(data=df_line(),
  #                                                        aes(x=date, y=value, colour=variable)) +
  #       geom_line()+
  #       theme(panel.background = element_rect(fill = "white",
  #                                             colour = "black",
  #                                             size = 0.5, linetype = "solid")) +
  #       xlab("Date")+ ylab("Population treated by each round")
  #   })

  #### population treated cumalitative

  output$grpah_treatment_cuma<- renderPlot({
    ggplot(data=admin_2_level_data_filter(),
           aes(x=month, y=popn_treated_cumulative_all_rounds,color = year,group= year)) +
      geom_line()+geom_point()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Month")+ ylab("Population treated (Cumulative[original])")
  })



  ###### cumulatative graph
  output$grpah_treatment_cuma_cal<- renderPlot({
    ggplot(admin_2_level_data_filter(), aes(x=month, y=cumulatative_target,color = year,group= year)) +
      geom_line()+geom_point()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Month")+ ylab("Population treated (Cumulative[calculated])")
  })



  ###### cumulative graph percentage treated
  output$cum_percentage_treated<- renderPlot({
    ggplot(admin_2_level_data_filter(),
           aes(x=month, y=cum_percentage_treated,color = year,group= year)) +
      geom_line()+geom_point()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Month")+ ylab("% treated, out of total population")
  })



  output$cum_percentage_treated_utg<- renderPlot({
    ggplot(admin_2_level_data_filter(),
           aes(x=month, y=cum_percentage_utg,color = year,group= year)) +
      geom_line()+geom_point()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Month")+ ylab("% treated, out of UTG Target")
  })



  ######################### start::population graph ##############################

  admin_1_filter_pop <-  reactive({pop_data_max_sum %>% dplyr::filter(adm1_name == admin1_name_1())})
  pop_graph_data <- reactive({admin_1_filter_pop() %>% dplyr::filter(adm2_name == admin2_name_1())})


  box_df1 <-  reactive({RB_pre_post_compiled %>% dplyr::filter(adm1_name == admin1_name_1())})
  box_plot_df <- reactive({box_df1() %>% dplyr::filter(adm2_name == admin2_name_1())})



  output$population_box<- renderPlot({
    ggplot(box_plot_df(), aes(x=year, y=total_population)) +
      geom_boxplot()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid"))
  })


  # pop max
  pop_data_max_sumdf_pi <- reactive({pop_graph_data() %>% select(adm1_name,adm2_name,year,
                                                                 population_max,utg_target_max) %>%
      pivot_longer(cols = c("population_max","utg_target_max"),values_to = "value",
                   names_to = "variable")})



  output$population_graph<- renderPlot({
    ggplot(pop_data_max_sumdf_pi(),
           aes(x=year, y=value, group =variable,color = variable)) +
      geom_line()+
      geom_point()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Year")+ ylab("Population/Targets")
  })

  # pop_mean

  pop_data_max_sumdf_pi_mean <- reactive({pop_graph_data() %>% select(adm1_name,adm2_name,year,
                                                                      population_mean,utg_target_mean) %>%
      pivot_longer(cols = c("population_mean","utg_target_mean"),values_to = "value",
                   names_to = "variable")})



  output$population_graph_mean<- renderPlot({
    ggplot(pop_data_max_sumdf_pi_mean(),
           aes(x=year, y=value, group =variable,color = variable)) +
      geom_line()+
      geom_point()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Year")+ ylab("Population/Targets")
  })



  # pop_mean

  pop_data_max_sumdf_pi_median <- reactive({pop_graph_data() %>% select(adm1_name,adm2_name,year,
                                                                        population_median,utg_target_median) %>%
      pivot_longer(cols = c("population_median","utg_target_median"),values_to = "value",
                   names_to = "variable")})



  output$population_graph_median<- renderPlot({
    ggplot(pop_data_max_sumdf_pi_median(),
           aes(x=year, y=value, group =variable,color = variable)) +
      geom_line()+
      geom_point()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Year")+ ylab("Population/Targets")
  })



  ######################### end::population graph ##############################





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
      geom_line()+geom_point()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Month")+ ylab("Population treated (Cumulative[calculated])")


  }) ## monthly treated graph


  ###### cumulative graph percentage treated
  output$cum_percentage_treated_r<- renderPlot({
    ggplot(graph_data(),
           aes(x=month, y=cum_percentage_treated,color = year,group= year)) +
      geom_line()+geom_point()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Month")+ ylab("% treated, out of total population")
  })



  output$cum_percentage_treated_utg_r<- renderPlot({
    ggplot(graph_data(),
           aes(x=month, y=cum_percentage_utg,color = year,group= year)) +
      geom_line()+geom_point()+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid")) +
      xlab("Month")+ ylab("% treated, out of UTG Target")
  })





  ################################################ END::rgion ###############################



  ####### start::Dt table ########
  output$pop_table <- renderDT({
    datatable(pop_data,
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center;',
                'Table 1: ', htmltools::em('Population by admin 2')

              ))


  })


  ####### END::Dt table ########



  ##### TAB 3::current situation monitoring ####

  output$map <-  renderLeaflet({
    base_map })


} # end server

######## Run the app #########

shinyApp(ui = ui, server = server)

