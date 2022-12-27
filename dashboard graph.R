
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

