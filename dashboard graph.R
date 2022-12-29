
admin_2_level_data %>% names()

utg_treatment_target_for_each_round
utg_2_treatment_target_for_the_whole_year

RB_pre_post_compiled$popn_treated_during_current_month


RB_pre_post_compiled$date <- RB_pre_post_compiled$date  %>% as.Date()

df <- RB_pre_post_compiled %>% filter(adm2_name == "awi")



ggplot(data=df, aes(x=date, y=popn_treated_during_current_month)) +
  geom_bar(stat="identity")+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid")) +
  xlab("Date")+ ylab("Population treated (Monthly)")



###### line grphan


df_line <- df %>% select(date, adm1_name,adm2_name,popn_treated_round1,
                         popn_treated_round2) %>%
  pivot_longer(cols = !c("date","adm1_name","adm2_name"),names_to = "variable",values_to = "value")

ggplot(data=df_line,
       aes(x=date, y=value, colour=variable)) +
  geom_line()


#### population treated

df_2 <- df %>% filter(popn_treated_cumulative_all_rounds != 0)

ggplot(data=df_2,
       aes(x=date, y=popn_treated_cumulative_all_rounds)) +
  geom_line()





### Info by region


region_cols <-c("date", "adm1_name",
  "popn_treated_during_current_month", "utg_treatment_target_for_each_round",
  "utg_2_treatment_target_for_the_whole_year", "total_popn_projected",
  "popn_treated_round1", "popn_treated_round2", "popn_treated_cumulative_all_rounds",
  "active_villages_for_the_year", "villages_treated_during_current_month",
  "villages_treated_round_1", "villages_treated_round_2", "total_popn_census",
  "total_population", "percent_utg_treated_round_1", "percent_utg_2_treated_all_rounds",
  "percent_popn_treated_round_1", "percent_active_villages_treated_for_round_1",
  "percent_active_villages_treated_for_round_2",
  "percent_popn_treated_round_2")


region_df <- RB_pre_post_compiled %>% select(region_cols) %>% group_by(date,adm1_name) %>%
  summarise_all(~sum(na.rm = T))



########## cumlutative graph

cum_df <- RB_pre_post_compiled %>% group_by(year) %>%
  mutate(
  cumulatative_target = cumsum(popn_treated_during_current_month)) %>% ungroup()

cum_df_for_graph$month <- month(cum_df_for_graph$month,label = T)
cum_df_for_graph$year <- factor(cum_df_for_graph$year)

ggplot(cum_df_for_graph, aes(x=month, y=cumulatative_target,color = year,group= year)) +
  geom_line()


##### population data tabe

pop_data <- RB_pre_post_compiled %>% select(adm2_name,adm1_name,year,total_population,
                                            popn_treated_during_current_month,
                                            utg_treatment_target_for_each_round,
                                            utg_2_treatment_target_for_the_whole_year
                                            )


ggplot(pop_data, aes(x=year, y=total_population)) +
  geom_boxplot()


box_plot_data <-

## max pop

pop_data_max_sum <- pop_data %>% group_by(year,adm1_name,adm2_name) %>% summarise(
  population_max = max(total_population,na.rm = T),
  population_avg = mean(total_population,na.rm = T),
  population_sum = sum(total_population,na.rm = T),

  utg_target_max = max(utg_2_treatment_target_for_the_whole_year,na.rm = T),
  utg_target_mean = max(utg_2_treatment_target_for_the_whole_year,na.rm = T),
  utg_target_sum = max(utg_2_treatment_target_for_the_whole_year,na.rm = T),

  )
#%>% pivot_wider(id_cols = c(adm1_name,adm2_name),names_from = "year",values_from = "population")

pop_data_max_sumdf <- pop_data_max_sum %>% filter(adm2_name== "awi")

pop_data_max_sumdf_pi <- pop_data_max_sumdf %>% select(adm1_name,adm2_name,year,
                                                       population_max,utg_target_max) %>%
  pivot_longer(cols = c("population_max","utg_target_max"),values_to = "value",
               names_to = "variable")

ggplot(pop_data_max_sumdf_pi,
       aes(x=year, y=value,group =variable,color = variable)) +
  geom_line()+
  geom_point()+
  xlab("Year")+ ylab("Population")


