"%>%" = magrittr::`%>%`

data.frame(Dataset = c("MOD11A2 cite wan2015mod11a2",
                       "GMTED2010 cite danielson2011global",
                       "-",
                       "-",
                       "-"),
           Variables = c("Land surface temperature (LST) day, night", 
                         "Elevation (DEM)",
                         "Longitud (X)",
                         "Latitud (Y)",
                         "Topographic disection index cite holden2011empirical (TDI)"),
           Distribution_resolution = c("Global, 1 km",
                                       "Global, 7.5 arc-seconds",
                                       "Study area, 1 km",
                                       "Study area, 1 km",
                                       "Study area, 1 km"))  %>%
  write.csv("./paper/output/Tab_spatial_predictors_variables",
            row.names = FALSE)
