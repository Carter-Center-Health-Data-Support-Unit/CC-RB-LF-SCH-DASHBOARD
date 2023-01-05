rb_boxplot <- function(.data,
                       x,
                       y,
                       pop,
                       facet=NULL,
                       plot_title ="Distribution of reported values for pop treated in current month by zone",
                       log=F

){
  p1 <- .data |>
    ggplot(aes(x=!!sym(x),
               y=!!sym(y)))+
    geom_boxplot()+
    geom_point(
      aes(x=!!sym(x),
          y=!!sym(pop),
          color="reported total\npopulation"
      ))+
    scale_color_manual(name = "", values = c("reported total\npopulation" = "red")) +
    theme_bw()+
    ggtitle(label = plot_title)+
    theme(
      axis.text.x = element_text(angle = 90),
      # axis.text.y=element_text(angle = 90),
      axis.title = element_blank(),
      # legend.position = c(.95, .95),
      # legend.justification = c("right", "top"),
      # legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    )

  if(isTRUE(log)){
    p2 <- p1+
      scale_y_log10(

        breaks = c(500,
                   50000,
                   100000,
                   200000,
                   400000,
                   800000,
                   1600000,
                   5e6),
        labels = c( "500","50K","100K","200K","400K", "800K","1.6M","5M")
      )
  }
  if(isFALSE(log)){
    p2 <-  p1+
      scale_y_continuous(
        breaks = c(
          10000,
          100000,
          200000,
          400000,
          800000,
          1600000,
          5e6),
        labels = c( "10K","100K","200K","400K", "800K","1.6M","5M")
      )
  }
  if(!is.null(facet)){

    p2 <- p2 +
      facet_wrap(vars(!!sym(facet)),scales="free")
  }

  p3 <- p2 +
    coord_flip()
  return(p3)

}
# dat_latest_year <- RB_pre_post_compiled |>
#   filter(lubridate::year(date)==lubridate::year(latest_date))
#
#
# dat_p_treated_by_utg |>
#   ggplot(aes(x=date, y=percent_treated_utg_total, color=adm2_name,group=adm2_name))+
#   geom_line()+
#   geom_point()+
#   facet_wrap(~adm1_name)+
#   theme(
#     legend.position = "none",
#     panel.background = element_rect(fill = "white",
#                                     colour = "black",
#                                     size = 0.5, linetype = "solid")
#   )

abbreviate_large_numbers <- function(tx) {
  div <- findInterval(as.numeric(gsub("\\,", "", tx)),
                      c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
  paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2),
        c("","K","M","B","T")[div] )}

heat_map_gg <- function(.dat, date_col="date"){

  dat_p <- .dat |>
    arrange(date) |>
    group_by(year,
             adm1_name,
             adm2_name
    ) |>
    summarise(
      treated_cumulative = cumsum(popn_treated_during_current_month),
      utg_total = utg_2_treatment_target_for_the_whole_year,
      date= unique(date),.groups = "keep"
    ) |>
    mutate(
      updated_utg_total = utg_total[which(date==max(date))],
      pct_treated_utg_total = treated_cumulative/updated_utg_total,
      year = lubridate::year(date),
      month= lubridate::month(date)
    ) |>
    filter(!is.na(adm2_name))

    p_base <- dat_p |>
      ggplot2::ggplot()+
      # gghdx::scale_fill_hdx_mint(drop=F)+
      # scale_fill_continuous()+
      gghdx::scale_fill_gradient_hdx_tomato(
        breaks= seq(0,1,by=0.1),
        label=scales::percent)+
    # ggplot2::scale_fill_manual(values = trigger_pal(name= "white_to_rd"),
    #                            drop=F,guide=ggplot2::guide_legend(nrow=1))+

    ggplot2::scale_x_date(
      # breaks = "year",
                          date_breaks = "3 months",
                          # date_major_breaks= "year",
                          date_minor_breaks = "1 month",
                          date_labels = "%b-%y",expand = c(0,0))+
    # ggplot2::scale_y_discrete(
    # expand= c(0,0)
    # )+
    ggplot2::theme_bw()+
    ggplot2::facet_grid(rows = vars(adm1_name),
                        switch = "both",
                        scales = "free",
                        space = "free"
    ) +
    ggplot2::theme(
      legend.position = "none",
      legend.title = element_blank(),
      legend.margin=margin(0,0,0,0),
      legend.key.width = unit(3,"line"),
      legend.key.height = unit(1,"line"),
      legend.text = element_text(size = 12),
      # text= element_text(family = "TT Arial"),
      # text = element_text(size = 10, family = "roboto"),
      # legend.box.margin=margin(-10,-10,-10,-10),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90),
      axis.title.y = element_blank(),
      panel.spacing = unit(0, "lines"),
      strip.background = element_blank(),
      strip.placement = "left"

    )+
    guides(fill = guide_legend(label.position = "bottom",nrow=1))

    pf <- p_base+
      ggiraph::geom_tile_interactive(aes(x= date,
                                         y= adm2_name,
                                         fill=pct_treated_utg_total,
                                         tooltip= glue::glue("Region: {adm1_name}<br>
                                                             Zone: {adm2_name}<br>
                                                             Year: {year}<br>
                                                             Month: {month}<br>
                                                             Treated cumulative ({year}): {abbreviate_large_numbers(treated_cumulative)}<br>
                                                             % goal treated: {round(pct_treated_utg_total*100,1)}<br>",

                                         )),color="grey")+
      geom_vline(xintercept = lubridate::ymd(c(
        "2016-12-31",
        "2017-12-31",
        "2018-12-31",
        "2019-12-31",
        "2020-12-31",
        "2021-12-31"
        )))

  return(pf)
}



# RB_pre_post_compiled |>
#   group_by(adm1_name,adm2_name) |>
#   summarise(
#     date= unique(date),
#     total_treatments = cumsum(popn_treated_during_current_month)
#   ) |>
#   ggplot(
#     aes(x= date, y=total_treatments, color =adm2_name)
#   )+
#   geom_point()+ geom_line()
#
# RB_pre_post_compiled |>
#   group_by(adm1_name,date) |>
#   summarise(
#     total_treatments = sum(popn_treated_during_current_month,na.rm=T)
#   ) |>
#   mutate(
#     cumulative_treatment= cumsum(total_treatments)
#   ) |>
#   ggplot(aes(x= date, y= cumulative_treatment, color = adm1_name))+
#   geom_line()





trigger_pal <- function(name= "white_to_rd"){
  if(name=="white_to_rd"){
    c(
      "#ffffffff", # white - 0
                 "#fffee0", # light yellow -1
                 "#f3e838ff",# med yellow -2
                 "#eb7d24ff", # orange - thresh 1 week
                 "#cd2026ff", # red - thresh 2 week
                 "#5d060cff" # dark red/brown - trigger
    )
  }
}
