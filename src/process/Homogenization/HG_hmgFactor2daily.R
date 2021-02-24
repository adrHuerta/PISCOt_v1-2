# Monthly2Daily - monthly to daily homogenization factors
# monthly_ts: Original monthly time serie
# monthly_ts_hmg: Homogenized monthly time serie
# daily_ts: Original daily time serie
# period_ts: Period time to interpolate monthly to daily values

# Note:
# Each monthly factor is assumed to be located at the 15th day of each month
# the rest of values are obtained using 1D linear interpolation
# finally, these values are added to the daily_ts to get a daily homogenized serie
# similar to the approach in https://journals.ametsoc.org/view/journals/clim/15/11/1520-0442_2002_015_1322_hodtoc_2.0.co_2.xml

hmgFactor2daily <- function(monthly_ts,
                            monthly_ts_hmg,
                            daily_ts,
                            period_ts = seq(as.Date("1980-12-15"), as.Date("2020-01-15"), by = "month"))
{
  
  # getting correction factor
  rawfactor <- zoo::coredata(monthly_ts - monthly_ts_hmg)
  
  # adding data to left/right to make interpolation
  left_rawfactor_right <- c(rawfactor[1], rawfactor, rawfactor[length(rawfactor)])
  left_rawfactor_right <- xts::xts(left_rawfactor_right, period_ts)
  
  # NA xts where data is interpolated
  NAs_ts <- seq(as.Date("1980-12-15"), as.Date("2020-01-15"), by = "day")
  rawfactor2int <- xts(rep(NA, length(NAs_ts)), NAs_ts)
  rawfactor2int <- xts::merge.xts(rawfactor2int, left_rawfactor_right, join='left')[, 2]
  
  # Interpolation
  rawfactor2int <- xts::xts(pracma::interp1(1:length(rawfactor2int), as.numeric(rawfactor2int), method = "linear"), NAs_ts)
  
  # Subsetting based on time series (no data from left/right)
  rawfactor2int <- rawfactor2int[time(daily_ts)]
  
  return(round(daily_ts - rawfactor2int, 2))
  #plot(daily_ts - round(daily_ts - rawfactor2int, 2))
  #plot(xts::apply.monthly(daily_ts, mean) - xts::apply.monthly(round(daily_ts - rawfactor2int, 2), mean)) is it similar?
  
}