# ENHANCED AND VISUAL QC
# 6. Precision inconsistencies 
# 
# - count the amount of decimales grouped (.0, .1, .. ., .9) by year
# - years with more than 40% of data in any level is deleted
# - years with more than 35% and 0% of data in at least three levels (anyone) at the same year is deleted
# - years with less than 35% of data is deleted (need to define)
# 
# 
# 7. Reduced variability 
# 
# - by data visualization
# - Eliminación de series con quiebres obvios y periodos con inhomogeneidades evidentes:


get_dec_from_xts <- function(xts_obj)
{
    
    w = abs(xts_obj)
    w = as.integer((round(w, 1) - 
                      as.integer(round(w, 1)))*10)
    
    data.frame(year = format(time(xts_obj), "%Y") %>% as.numeric(),
               dec = factor(w, levels = seq(9, 0, -1))) 
    
  # format(time(xts_obj), "%Y") %>% 
  #   unique() %>% 
  #   lapply(., function(year){
  #     
  #     w = xts_obj[format(time(xts_obj), "%Y") %in% year]
  #     intG <- as.integer((round(w, 1) - as.integer(round(w, 1)))*10)
  #     aggregate(intG, by = list(intG), FUN = length) %>%
  #       setNames(., c("dec", "n")) %>%
  #       transform(perc = 100*n/length(w),
  #                 year = year)
  #     
  #   }) %>%
  #   do.call(rbind, .)
  
}

get_pRcs_temp <- function(xts_obj)
{
  
  tmax = xts_obj$tmax %>% .[!is.na(.)]
  tmin = xts_obj$tmin %>% .[!is.na(.)]
  
  return(list(xts_obj = list(tmax = tmax, tmin = tmin),
              prcs_xts_obj = list(tmin = get_dec_from_xts(xts_obj = tmin),
                                  tmax = get_dec_from_xts(xts_obj = tmax))))
  
}

var_plot <- function(df_data)
  {
  
  if(dim(df_data)[1] != 0){
    
   df_data %>%
      ggplot2::ggplot() + 
      ggplot2::geom_point(ggplot2::aes(x = index, y = value), size = .1) + 
      ggplot2::xlab("") + ggplot2::ylab(paste("value ", "(", unique(df_data$series), ")", sep = "")) + 
      ggplot2::scale_x_date(date_minor_breaks = "1 year") + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5), 
                     axis.title.x = ggplot2::element_blank()) -> pp1
    
  } else {
    df_data %>%
      ggplot2::ggplot() + ggplot2::geom_blank() + 
      ggplot2::xlab("") + ggplot2::ylab("") + 
      ggplot2::scale_x_date(date_minor_breaks = "1 year") + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5), 
                     axis.title.x = ggplot2::element_blank()) -> pp1
    
  }
  
  pp1

}

prcs_plot <- function(df_data){
  
  rhg_cols <- rev(c("black", "yellow", "orange", "red", "darkslateblue", "darkgray", "magenta","blue", "cyan", "darkgreen"))
  
  if(dim(df_data)[1] != 0){
    
    ggplot2::ggplot() + 
      ggplot2::geom_bar(data = df_data, ggplot2::aes(x = year, fill = dec),  width=.8) + 
      ggplot2::scale_fill_manual(values = rhg_cols, drop = F, " ")+ 
      ggplot2::ylab("frequency (days/year)") + ggplot2::xlab(" ") +
      # ggplot2::scale_x_continuous(limits = c(min(df_data$year)-1.5, max(df_data$year))+ .9,
      #                             expand = c(0.01, 0.01)) + 
      ggplot2::scale_y_continuous(expand = c(.015, .015)) + 
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, label.position = "bottom", reverse = TRUE)) +
      ggplot2::theme_bw() + 
      ggplot2::theme(legend.position="bottom",
                     legend.box = "horizontal",
                     legend.spacing.x = ggplot2::unit(-.05, 'cm'),
                     legend.key.size = ggplot2::unit(0.5, "cm"),
                     legend.margin=ggplot2::margin(0,0,0,0),
                     legend.box.margin=ggplot2::margin(-20, 0, 0, 0),
                     axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5)) -> pp2
    
  } else {
    
    ggplot2::ggplot() + 
      ggplot2::geom_blank() +
      ggplot2::scale_fill_manual(values = rhg_cols, drop = F, " ") + 
      ggplot2::ylab("frequency (days/year)") + ggplot2::xlab(" ") +
      # ggplot2::scale_x_continuous(limits = c(min(df_data$year)-1.5, max(df_data$year))+ .9,
      #                             expand = c(0.01, 0.01)) + 
      ggplot2::scale_y_continuous(expand = c(.015, .015)) + 
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, label.position = "bottom", reverse = TRUE)) +
      ggplot2::theme_bw() + 
      ggplot2::theme(legend.position="bottom",
                     legend.box = "horizontal",
                     legend.spacing.x = ggplot2::unit(-.05, 'cm'),
                     legend.key.size = ggplot2::unit(0.5, "cm"),
                     legend.margin=ggplot2::margin(0,0,0,0),
                     legend.box.margin=ggplot2::margin(-20, 0, 0, 0),
                     axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5)) -> pp2
    
  }
  
  pp2
}

enhanced_qc_plot <- function(get_pRcs_temp_output,
                          title_plt = "ID")
  {
  
  tmax_plt_pr <- get_pRcs_temp_output$prcs_xts_obj[["tmax"]] %>% prcs_plot()
  tmin_plt_pr <- get_pRcs_temp_output$prcs_xts_obj[["tmin"]] %>% prcs_plot()
  tmax_plt_vr <- get_pRcs_temp_output$xts_obj[["tmax"]] %>% broom::tidy() %>% var_plot()
  tmin_plt_vr <- get_pRcs_temp_output$xts_obj[["tmin"]] %>% broom::tidy() %>% var_plot()
  
  ggpubr::ggarrange(tmin_plt_vr, tmax_plt_vr, 
                    tmin_plt_pr, tmax_plt_pr, ncol = 2, nrow = 2) %>% # in row order as matrix(1:4, ncol = 2, byrow = T)
    ggpubr::annotate_figure(fig.lab = title_plt,
                            fig.lab.pos = c("bottom")) %>%
    return()
  
  }
