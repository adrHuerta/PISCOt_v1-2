# ENHANCED AND VISUAL QC
# 6. Precision inconsistencies 
# 
# - count the amount of decimales grouped (.0, .1, .. ., .9) by year
# 
# 7. Reduced variability 
# 
# - by data visualization
# - Eliminación de series con quiebres obvios y periodos con inhomogeneidades evidentes:
# ###
# Adrian Huerta (adrhuerta@gmail.com)
# ###

get_dec_from_xts <- function(xts_obj)
{
  
  w = abs(xts_obj)
  w = as.integer((round(w, 1) - 
                    as.integer(round(w, 1)))*10)
  
  data.frame(year = format(time(xts_obj), "%Y") %>% as.numeric(),
             dec = factor(w, levels = seq(9, 0, -1))) 
}

get_pRcs_temp <- function(xts_obj)
{
  
  ts = xts_obj %>% .[!is.na(.)]

  return(list(xts_obj = ts,
              prcs_xts_obj = get_dec_from_xts(xts_obj = ts)))
  
}

var_plot <- function(df_data)
{
  
  df_data %>%
    ggplot2::ggplot() + 
    ggplot2::geom_point(ggplot2::aes(x = index, y = value), size = .1) + 
    ggplot2::xlab("") + ggplot2::ylab(paste("value ", "(", unique(df_data$series), ")", sep = "")) + 
    ggplot2::scale_x_date(date_minor_breaks = "1 year") + 
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5), 
                   axis.title.x = ggplot2::element_blank())
  
}

prcs_plot <- function(df_data)
{
  
  rhg_cols <- rev(c("black", "yellow", "orange", "red", "darkslateblue", "darkgray", "magenta","blue", "cyan", "darkgreen"))
  
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
                   axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))
}

enhanced_qc_plot <- function(get_pRcs_temp_output,
                             title_plt = "ID")
{
  
  ts_plt_pr <- get_pRcs_temp_output$prcs_xts_obj %>% prcs_plot()
  ts_plt_vr <- get_pRcs_temp_output$xts_obj %>% broom::tidy() %>% var_plot()

  ggpubr::ggarrange(ts_plt_vr, ts_plt_pr, nrow = 2) %>% # in row order as matrix(1:4, ncol = 2, byrow = T)
    return()
  
}
