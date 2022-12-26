
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
