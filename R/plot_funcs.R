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
