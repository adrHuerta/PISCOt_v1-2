# Enhanced and visual QC
# From https://www.geography.unibe.ch/unibe/portal/fak_naturwis/e_geowiss/c_igeogr/content/e39603/e68757/e84588/e199552/e640019/files640026/flag_description_eng.pdf
# 0) Some atypical values are deleted (automatic QC may no too robust [needed for lower thresholds], but trade-off false positives vs number of missing data)
# 1) Missing temperature intervals are corrected as possible (mis.tem.int), worst case are deleted (such as 3)
# 2) Rounding errors, not evaluated as this need an specific evaluation of the frequencies of decimals (also confirmation by looking at original documents) and can be seen as other problem 
# 3) Asymmetric rounding patterns (asy.rou), all asymmetric rounding patterns are accepted
# 4) Low measurement resolution (mea.pre.inc), worst case are deleted (such as 0)
# 5) Irregularities in the data pattern (oth.qua.pro), worst case are deleted (such as 3)
# 6) Obvious in-homogeneities (gro.inh), worst case are deleted (such as 3)

#----------------------------------------------------------------------
# get_dec_from_xts - distribution calculation of decimal values
# nested function - main routine
# xts_obj : an xts obj single time serie

# Note:
# The approach follows the idea of https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/joc.5037

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


#----------------------------------------------------------------------
get_VarMean_change_point <- function(xts_obj)
  {
  
  if(sum(!is.na(xts_obj)) < (365*5)){
    
    change_var <- NA
    change_mean <- NA
    
  } else {
    
    changepoint::cpt.var(as.numeric(xts_obj),
                         penalty = "MBIC",
                         pen.value = 0.05,
                         method = "AMOC",
                         class = TRUE,
                         minseglen = 365*5) -> change_var
    
    changepoint::cpt.mean(as.numeric(xts_obj),
                          penalty = "MBIC",
                          pen.value = 0.05,
                          method = "AMOC",
                          class = TRUE,
                          minseglen = 365*5) -> change_mean
    
    change_var <- time(xts_obj)[changepoint::cpts(change_var)]
    change_mean <- time(xts_obj)[changepoint::cpts(change_mean)]
    
  }
  
  return(c(change_var = change_var,
           change_mean = change_mean))
  
  }


#----------------------------------------------------------------------
# get_pRcs_temp - distribution calculation of decimal values
# xts_obj : an xts obj time serie (tmax and tmin)

# Note:
# The approach follows the idea of https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/joc.5037

get_pRcs_temp <- function(xts_obj)
{
  
  tmax = xts_obj$tmax %>% .[!is.na(.)]
  tmin = xts_obj$tmin %>% .[!is.na(.)]
  
  return(list(xts_obj = list(tmax = tmax, tmin = tmin),
              prcs_xts_obj = list(tmin = get_dec_from_xts(xts_obj = tmin),
                                  tmax = get_dec_from_xts(xts_obj = tmax)),
              varmean_change = list(tmin = get_VarMean_change_point(xts_obj = tmin["1981/"]),
                                    tmax = get_VarMean_change_point(xts_obj = tmax["1981/"]))))
  
}




#----------------------------------------------------------------------
# var_plot - Time series plot for tmax and tmin
# nested function - main routine
# df_data : a data frame time series (tmax and tmin)

var_plot <- function(df_data,
                     step_changes)
  {
  
  if(dim(df_data)[1] != 0){
    
   df_data %>%
      ggplot2::ggplot() + 
      ggplot2::geom_point(ggplot2::aes(x = index, y = value), size = .1) + 
      ggplot2::xlab("") + ggplot2::ylab(paste("value ", "(", unique(df_data$series), ")", sep = "")) + 
      ggplot2::scale_x_date(date_minor_breaks = "1 year") + 
      ggplot2::theme_bw() +
      ggplot2::geom_vline(ggplot2::aes(xintercept = as.Date(step_changes["change_var"])),
                          colour = "red", alpha = .2, size = 2) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = as.Date(step_changes["change_mean"])),
                          colour = "blue", alpha = .2, size = 2) +
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




#----------------------------------------------------------------------
# prcs_plot - Time series plot of decimal distribution for tmax and tmin 
# nested function - main routine
# df_data : a data frame time series (tmax and tmin)

# Note:
# The approach follows the idea of https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/joc.5037

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





#--------------------------------------------------------------------------------
# enhanced_qc_plot - Plots to be used for visual qc (two plots for tmax and tmin)
# get_pRcs_temp_output : output from get_pRcs_temp()

# Note:
# The approach follows the idea of https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/joc.5037

enhanced_qc_plot <- function(get_pRcs_temp_output,
                             title_plt = "ID")
  {
  
  tmax_plt_pr <- get_pRcs_temp_output$prcs_xts_obj[["tmax"]] %>% prcs_plot()
  tmin_plt_pr <- get_pRcs_temp_output$prcs_xts_obj[["tmin"]] %>% prcs_plot()
  tmax_plt_vr <- get_pRcs_temp_output$xts_obj[["tmax"]] %>% broom::tidy() %>% var_plot(df_data = .,
                                                                                       step_changes = get_pRcs_temp_output$varmean_change$tmax)
  tmin_plt_vr <- get_pRcs_temp_output$xts_obj[["tmin"]] %>% broom::tidy() %>% var_plot(df_data = .,
                                                                                       step_changes = get_pRcs_temp_output$varmean_change$tmin)
  
  ggpubr::ggarrange(tmin_plt_vr, tmax_plt_vr, 
                    tmin_plt_pr, tmax_plt_pr, ncol = 2, nrow = 2) %>% # in row order as matrix(1:4, ncol = 2, byrow = T)
    ggpubr::annotate_figure(fig.lab = title_plt,
                            fig.lab.pos = c("bottom")) %>%
    return()
  
  }
