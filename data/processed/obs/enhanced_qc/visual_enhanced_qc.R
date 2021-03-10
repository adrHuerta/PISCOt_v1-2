# Visual inspection following the ideas of https://www.geography.unibe.ch/unibe/portal/fak_naturwis/e_geowiss/c_igeogr/content/e39603/e68757/e84588/e199552/e640019/files640026/flag_description_eng.pdf

# Note: 
# 0) Some atypical values are deleted (automatic QC may no too robust [needed for lower thresholds], but trade-off false positives vs number of missing data)
# 1) Missing temperature intervals are corrected as possible (mis.tem.int), worst case are deleted (such as 3)
# 2) Rounding errors, not evaluated as this need an specific evaluation of the frequencies of decimals (also confirmation by looking at original documents) and can be seen as other problem 
# 3) Asymmetric rounding patterns (asy.rou), all asymmetric rounding patterns are accepted
# 4) Low measurement resolution (mea.pre.inc), worst case are deleted (such as 0)
# 5) Irregularities in the data pattern (oth.qua.pro), worst case are deleted (such as 3)
# 6) Obvious in-homogeneities (gro.inh), worst case are deleted (such as 3)
# ADD: all data deleted
# NDD: new data deleted, new/recent data (2019-) deleted
# NMH: not mean homogenization is enough 

list(
  # Best stations
  BO001006 = c("tmin['/1981'] <- NA"),
  CH000083 = c("tmin[tmin > 26.5] <- NA",
               "tmax['/1959'] <- NA"),
  EC000003 = c("tmin[tmin < -4] <- NA"),
  EC000005 = c("tmax[tmax > 37] <- NA"),
  EC000033 = c("tmax[tmax > 31] <- NA"),
  EC000025 = c("tmin['2007/2009'] <- NA"),
  PE100020 = c("tmin[tmin > 19.5] <- NA"),
  PE105076 = c("tmin[tmin < 7.5] <- NA"),
  PE108027 = c("tmin[tmin > 26.5] <- NA",
               "tmax[tmax > 40] <- NA"),
  PE111022 = c("tmin['2008/2010'] <- NA"),
  PE112037 = c("tmax[tmax > 28.5] <- NA"),
  PE113030 = c("tmin[tmin > 22.5] <- NA",
               "tmax['/1969'] <- NA"),
  PE113042 = c("tmin['1963/1964'] <- NA",
               "tmax['1963/1964'] <- NA"),
  PE114038 = c("tmin['1964'] <- NA",
               "tmax['1976/1978'] <- NA"),
  PE111018 = c("tmin['1980/1984'] <- NA"),
  PE113025 = c("tmin['1993/1994'] <- NA"),
  PE113100 = c("tmax[tmax > 27.5] <- NA"),
  PE116014 = c("tmin['1988/1991'] <- NA"),
  PE117006 = c("tmin['1987/1992'] <- NA"),
  # Less-Great stations
  BO001012 = c("tmin['/1981'] <- NA"),
  BO004074 = c("tmin['/1996'] <- NA", # NEW CHANGE tmin['/1981'] <- NA
               "tmax['1981'] <- NA"), # NMH
  BR082704 = c("tmin['/1974'] <- NA",
               "tmin['1982/1988'] <- NA",
               "tmin['2017/2018'] <- NA"),
  EC000031 = c("tmax[tmax > 25] <- NA"),
  PE100015 = c("tmin['1994/1997'] <- NA",
               "tmin['2016/2018'] <- NA"),
  PE100019 = c("tmin['2010/2014'] <- NA"),
  PE100034 = c("tmin[tmin > 30] <- NA",
               "tmin['/1998'] <- NA",
               "tmax['/1980'] <- NA"),
  PE100059 = c("tmin['/2003-06'] <- NA",
               "tmax['/2003-06'] <- NA"),
  PE100092 = c("tmin['1998/2005-02'] <- NA", ###
               "tmin['2009-11/2010-01'] <- NA"),
  PE103043 = c("tmin[tmin < 13] <- NA"),
  PE104058 = c("tmin['1963'] <- NA",
               "tmax['1963'] <- NA"),
  PE105001 = c("tmin['1952'] <- NA",
               "tmax['1952'] <- NA"),
  PE105055 = c("tmin['/1965'] <- NA",
               "tmax['/1965'] <- NA"),
  PE105065 = c("tmin['2000/2002'] <- NA",
               "tmin['1969/1971'] <- NA",
               "tmin['1982/1983'] <- NA",
               "tmax['1969/1971'] <- NA"),
  PE105068 = c("tmin['1991/1994'] <- NA",
               "tmin['1999/2001'] <- NA",
               "tmin['2006/2009'] <- NA"),
  PE105106 = c("tmin['1969/1970'] <- NA",
               "tmax['/1970'] <- NA"),
  PE106019 = c("tmin['/1989'] <- NA",
               "tmax['/1998'] <- NA"),
  PE106042 = c("tmin['1953/1960'] <- NA"),
  PE106045 = c("tmin['1981/1982'] <- NA",
               "tmin['1999'] <- NA",
               "tmax['1981/1982'] <- NA"),
  PE106054 = c("tmin['1995'] <- NA"),
  PE107013 = c("tmin['1972/1973'] <- NA", ### NDD
               "tmax['1972/1973'] <- NA",
               "tmin['2017/'] <- tmin['2017/'] - 2"),
  PE107017 = c("tmin['1999/2003'] <- NA",
               "tmin['/1995'] <- NA",
               "tmax['/1995'] <- NA"), ###
  PE107018 = c("tmin['1993/1997'] <- NA", ###
               "tmin['2011/2014'] <- NA"),
  PE107028 = c("tmin['1946/1961'] <- NA",
               "tmax['1946/1961'] <- NA"),
  PE107037 = c("tmin['1989/1991'] <- NA",
               "tmax['1989/1991'] <- NA"),
  PE108045 = c("tmin['2004'] <- NA",
               "tmax['2004'] <- NA"),
  PE109017 = c("tmin[tmin > 13] <- NA"),
  PE109032 = c("tmin['/1999'] <- NA"),
  PE110017 = c("tmin['2000/'] <- NA",
               "tmax['2000/'] <- NA"),
  PE110027 = c("tmin['2001/2006'] <- NA",
               "tmax['2001/2006'] <- NA"),
  PE111020 = c("tmin['2000/2001'] <- NA",
               "tmax['2000/2001'] <- NA"),
  PE111046 = c("tmin['/1997'] <- NA",
               "tmax['/1998'] <- NA"),
  PE112012 = c("tmin['/1992'] <- NA", ###
               "tmax['/1992'] <- NA"),
  PE112055 = c("tmin['/1994'] <- NA",
               "tmin['2001/2005'] <- NA",
               "tmax['/1994'] <- NA",
               "tmax[tmax > 25] <- NA"),
  PE112059 = c("tmin['2006/2010'] <- NA",
               "tmax['1977/1999'] <- NA"),
  PE113019 = c("tmin[tmin > 20] <- NA",
               "tmin['1968/1969'] <- NA"),
  PE113022 = c("tmin['/1993'] <- NA",
               "tmax['/1993'] <- NA"),
  PE113034 = c("tmin['1980/1985'] <- NA", # NEW CHANGE tmin['1980/1983'] <- NA
               "tmax['1980/1984'] <- NA"), # NEW CHANGE tmax['1980/1983'] <- NA
  PE113037 = c("tmin['/1970'] <- NA", "tmin['1976/1982'] <- NA", # NEW CHANGE
               "tmax['/1970'] <- NA", "tmax['1979/1982'] <- NA"), # NEW CHANGE
  PE113044 = c("tmin['1970/1992'] <- NA",
               "tmax['1970/1997'] <- NA"),
  PE114030 = c("tmin['/1998'] <- NA",
               "tmax['/1998'] <- NA"),
  PE114032 = c("tmin['/1991'] <- NA",
               "tmax['/1991'] <- NA"),
  PE114033 = c("tmin['/1962'] <- NA",
               "tmax['/1962'] <- NA"),
  PE114035 = c("tmax['1950/1964'] <- NA"),
  PE114040 = c("tmin['2003-07/2014-10'][tmin['2003-07/2014-10'] < 0] <- tmin['2003-07/2014-10'][tmin['2003-07/2014-10'] < 0] + 4.2"), # as Hunziker et al. 2017
  PE114042 = c("tmin['1968/1970'] <- NA",
               "tmax['1973/1976'] <- NA"),
  PE114043 = c("tmin['2009/2014'] <- NA",
               "tmax['2009/2014'] <- NA"),
  PE114046 = c("tmax[tmax > 27.5] <- NA"),
  PE115015 = c("tmin['/1965'] <- NA"),
  PE115018 = c("tmin['1965/1968'] <- NA"),
  PE115019 = c("tmin['/1968'] <- NA",
               "tmin['1983/1986'] <- NA",
               "tmin['1998/1999'] <- NA", "tmax['1989/1992'] <- NA"), # NEW CHANGE
  PE115024 = c("tmin['/1968'] <- NA"),
  PE115033 = c("tmax['1977/1985'] <- NA"),
  PE115037 = c("tmin['1963'] <- NA",
               "tmin['1977/1978'] <- NA",
               "tmax['/1965'] <- NA",
               "tmax['1975/1976'] <- NA"),
  PE115047 = c("tmin['1998-05/2011-09'][tmin['1998-05/2011-09'] < 0] <- tmin['1998-05/2011-09'][tmin['1998-05/2011-09'] < 0] + 4.2"),
  PE115101 = c("tmin['/2005'] <- NA",
               "tmin[tmin > 2.5] <- NA"),
  PE116003 = c("tmin['/1994'] <- NA",
               "tmax['/1994'] <- NA"),
  PE116009 = c("tmin['/2004'] <- NA",
               "tmax['/2004'] <- NA"),
  PE116017 = c("tmin['/1977'] <- NA",
               "tmax['/1959'] <- NA"),
  PE116021 = c("tmin['/1996'] <- NA",
               "tmax['/1996'] <- NA"),
  PE116023 = c("tmin['1981/1986'] <- NA",
               "tmin['1996/2001'] <- NA"),
  PE116029 = c("tmin['2005/2010'] <- NA",
               "tmax['2005/2010'] <- NA"),
  PE116033 = c("tmin['/2002'] <- NA"),
  PE117020 = c("tmin['/1996'] <- NA",
               "tmax[tmax > 30] <- NA"),
  PE117037 = c("tmin['2016/'] <- NA",
               "tmax['2016/'] <- NA"),
  PE114104 = c("tmin['1964'] <- NA",
               "tmin['1998/2000'] <- NA"),
  PE114023 = c("tmin['1986'] <- NA",
               "tmin['1991/1992'] <- NA"),
  PE115098 = c("tmin['2011/2013'] <- NA"),
  PE115129 = c("tmin['2007'] <- NA"),
  PE116030 = c("tmin['2009/2011'] <- NA"),
  # Less-Less stations
  PE107009 = c("tmin['1980/1989'] <- NA"),
  CO44015060 = c("tmin['1998/2001'] <- NA",
                 "tmin['2011/2012'] <- NA",
                 "tmax['2011/2012'] <- NA"),
  EC000102 = c("tmin['/1969'] <- NA",
               "tmin['1972'] <- NA",
               "tmin['1975'] <- NA",
               "tmin['1980/1988'] <- NA",
               "tmin['1991'] <- NA",
               "tmin['1997/1998'] <- NA",
               "tmin['2010/2011'] <- NA",
               "tmax['/1970'] <- NA",
               "tmax['1981/1984'] <- NA"
  ),
  EC000162 = c("tmin['/1971'] <- NA",
               "tmin['2000/2001'] <- NA",
               "tmax['/1971'] <- NA"),
  EC000180 = c("tmin['/1974'] <- NA",
               "tmax['/1974'] <- NA",
               "tmax['1981/1983'] <- NA"),
  PE100132 = c("tmin['/2017'] <- NA"),
  PE103031 = c("tmin['/1965'] <- NA", ###
               "tmin['1994/1996'] <- NA",
               "tmax['/1965'] <- NA",
               "tmax['2005/2007'] <- NA"),
  PE103040 = c("tmin['/1967'] <- NA", "tmin['1996'] <- NA", # NEW CHANGE
               "tmax['/1967'] <- NA", 
               "tmax['1978/1988'] <- NA"), # NEW CHANGE tmax['1986/1988'] <- NA
  #            "tmax['2015/2016'] <- NA")
  PE103046 = c("tmin['/1993'] <- NA",
               "tmax['/1993'] <- NA",
               "tmin['1999/2001'] <- NA"),
  PE105016 = c("tmin['2011/2016'] <- NA"),
  PE105042 = c("tmin['/2007'] <- NA",
               "tmax['/2007'] <- NA"),
  PE105054 = c("tmin['1969/1971'] <- NA",
               "tmin['2005'] <- NA",
               "tmax['1998/2001'] <- NA"),
  PE105056 = c("tmin['/1977'] <- NA", "tmin[tmin > 19] <- NA"), # NEW CHANGE
  PE105075 = c("tmin['/2004'] <- NA",
               "tmin['2017/'] <- NA",
               "tmin[tmin < 12.5] <- NA",
               "tmax['/2004'] <- NA"),
  PE106014 = c("tmin['/1991'] <- NA",
               "tmax['/1962'] <- NA",
               "tmax['1980/1995'] <- NA",
               "tmax[tmax > 37.5] <- NA"),
  PE106016 = c("tmin['/1985'] <- NA",
               "tmin['2008/2015'] <- NA",
               "tmax['/1985'] <- NA",
               "tmax['1990/1991'] <- NA"),
  PE106018 = c("tmin[tmin > 27.5] <- NA",
               "tmin['/1965'] <- NA",
               "tmax['/1967'] <- NA"),
  PE106034 = c("tmin['/2005'] <- NA",
               "tmax['/2003'] <- NA"),
  PE106037 = c("tmin['/2010'] <- NA",
               "tmax['/2010'] <- NA"),
  PE106056 = c("tmin['/1985'] <- NA",
               "tmax['/1985'] <- NA"),
  PE106058 = c("tmin['2008/2017'] <- NA"),
  PE106088 = c("tmin['2017/2019'] <- NA",
               "tmax['2017/2019'] <- NA"),
  PE106091 = c("tmin['2015/'] <- NA",
               "tmax['2015/'] <- NA"),
  PE108003 = c("tmin['1967/1968'] <- NA",
               "tmax['1967/1968'] <- NA"),
  PE108025 = c("tmin['/1999'] <- NA",
               "tmin['2003'] <- NA",
               "tmax['/1999'] <- NA"),
  PE108028 = c("tmin['/2001'] <- NA",
               "tmax['/1996'] <- NA"),
  PE108034 = c("tmin['1994/2003'] <- NA",
               "tmax['1994/2003'] <- NA"),
  PE109020 = c("tmax['/2002'] <- NA"),
  PE109022 = c("tmin['/2001'] <- NA",
               "tmax['/2001'] <- NA"),
  PE110025 = c("tmin[tmin > 18] <- NA", ###
               "tmin['/1976'] <- NA", "tmin['1995/1996'] <- NA", # NEW CHANGE
               "tmin['2001/2003'] <- NA",
               "tmin[tmin < -5] <- NA",
               "tmax['/1976'] <- NA", "tmax['1986/1996'] <- NA", # NEW CHANGE
               "tmax['2001/2003'] <- NA"),
  PE111028 = c("tmin['1964'] <- NA",
               "tmin['1981/1986'] <- NA",
               "tmax['1982/1983'] <- NA"),
  PE114028 = c("tmin['2013/'] <- NA",
               "tmax['2013/'] <- NA"),
  PE115044 = c("tmin['/1997'] <- NA",
               "tmax['2005/2012'] <- NA"),
  PE116043 = c("tmin['/2007'] <- NA",
               "tmin['2010/2014'] <- NA",
               "tmax['/2007'] <- NA",
               "tmax['2010/2014'] <- NA"),
  PE117002 = c("tmin['/1960'] <- NA",
               "tmax['/1960'] <- NA"),
  PE116060 = c("tmin['/2000'] <- NA", # NMH # NEW CHANGE
               "tmax['1976/1980'] <- NA",   # NEW CHANGE
               "tmax['1983'] <- NA",        # NEW CHANGE
               "tmax['1987'] <- NA"),       # NEW CHANGE
  PE115092 = c("tmin['/2008'] <- NA", # NHM
               "tmax['2011/2013'] <- NA"),
  PE113016 = c("tmin['/1991'] <- NA",
               "tmax['/1960'] <- NA"),
  PE112065 = c("tmin['/1990'] <- NA",
               "tmin['1993/1995'] <- NA",
               "tmin['1998/2000'] <- NA",
               "tmax['/1998'] <- NA",
               "tmax['1995/1996'] <- NA"),
  # Worst stations
  BR082410 = c("tmin['/2000'] <- NA", ###
               "tmin['2016/2017'] <- NA",
               "tmax['2016/2017'] <- NA",
               "tmax['/2000'] <- NA"),
  EC000105 = c("tmin['/1984'] <- NA",
               "tmin['1996/1997'] <- NA",
               "tmax['/1972'] <- NA"),
  EC000138 = c("tmin['/1976'] <- NA",
               "tmax['/1970'] <- NA"),
  EC000139 = c("tmin['1991/1993'] <- NA"),
  EC000142 = c("tmin['/1994'] <- NA",
               "tmax['/1993'] <- NA"),
  EC000148 = c("tmin['/1970'] <- NA",
               "tmin['1986/1987'] <- NA",
               "tmin['1994/2006'] <- NA",
               "tmin[tmin > 18] <- NA",
               "tmax['/1970'] <- NA",
               "tmax['1980'] <- NA",
               "tmax['1994/2002'] <- NA"),
  PE100021 = c("tmin['/1990'] <- NA",
               "tmax['/1989'] <- NA"),
  PE100109 = c("tmin['/2009'] <- NA",
               "tmax['/2009'] <- NA"),
  PE100119 = c("tmin[tmin < -4.9] <- NA",
               "tmax['/2003'] <- NA"),
  PE102009 = c("tmin['2003/2005'] <- NA",
               "tmin['2007/2008'] <- NA"),
  # PE103042 = c("tmin['2007/2009'] <- NA", # NEW CHANGE
  #              "tmax['2007/2009'] <- NA"), # NEW CHANGE
  PE103049 = c("tmin['1994/1995'] <- NA", ###
               "tmin['2005/2008'] <- NA",
               "tmax['1976/1985'] <- NA"), # 1976/1995
  PE104056 = c("tmin['/1996'] <- NA",
               "tmax['/1996'] <- NA",
               "tmax['2003'] <- NA"),
  PE105057 = c("tmin['1971/1978'] <- NA",
               "tmax['1969'] <- NA",
               "tmax['1984/1986'] <- NA",
               "tmax['2010/2011'] <- NA"),
  PE105095 = c("tmin['/1965'] <- NA",
               "tmin['1979/2009'] <- NA",
               "tmax['/1966'] <- NA",
               "tmax['1979/2009'] <- NA",
               "tmax['2011-12/2012-03'] <- NA"),
  PE106013 = c("tmin['/1961'] <- NA",
               "tmin['1965'] <- NA",
               "tmin['1973'] <- NA",
               "tmin['1974'][tmin['1974'] > 23] <- NA",
               "tmax['/1961'] <- NA",
               "tmax['1980/1984'] <- NA",
               "tmax['2004/2010'] <- NA"),
  PE106053 = c("tmin['/1970'] <- NA",
               "tmin['1976/1983'] <- NA",
               "tmax['/1970'] <- NA",
               "tmax['1976/1983'] <- NA",
               "tmax['2005/2010'][tmax['2005/2010'] > 27.5] <- NA"),
  PE106071 = c("tmin['/2003'] <- NA"),
  PE107002 = c("tmin['/1996'] <- NA",
               "tmax['/1998'] <- NA",
               "tmax['2010'] <- NA",
               "tmax['2015/2016'] <- NA"),
  PE108017 = c("tmin['/2000'] <- NA",
               "tmin['2018/2019'] <- NA",
               "tmax['/2000'] <- NA"),
  PE108033 = c("tmin['/1980'] <- NA",
               "tmax['/1980'] <- NA"),
  PE109013 = c("tmin['/1955'] <- NA",
               "tmax['/1955'] <- NA"),
  PE111026 = c("tmin['/1999'] <- NA", ###
               "tmax['/1994'] <- NA"),
  PE111027 = c("tmin['1991/1993'] <- NA",
               "tmin['1998/2000'] <- NA",
               "tmax['1976/1983'] <- NA",
               "tmax['1990/1994'] <- NA"),
  PE112061 = c("tmin['/1961'] <- NA", ###
               "tmin['1980/2010'] <- NA",
               "tmin['2011/'][tmin['2011/'] < 0] <- tmin['2011/'][tmin['2011/'] < 0] + 0.5", ###
               "tmax['/1961'] <- NA",
               "tmax['1980/2010'] <- NA"),
  PE113035 = c("tmin['1984/2002'] <- NA",
               "tmax['1984/2010'] <- NA"),
  PE113119 = c("tmin['/2003'] <- NA",
               "tmin['2011/2014'] <- NA",
               "tmin['2018'] <- NA"),
  PE115049 = c("tmax['1999/2001'] <- NA"),
  PE116020 = c("tmin['/1999'] <- NA",
               "tmin['2004/2009'] <- NA"),
  PE116052 = c("tmin['/2013'] <- NA",
               "tmax['/2013'] <- NA"),
  PE116061 = c("tmin['/1963'] <- NA",
               "tmin['1978'] <- NA",
               "tmin['1981'] <- NA",
               "tmax['/1963'] <- NA",
               "tmax['1973/1980'] <- NA"),
  PE117019 = c("tmin['1969/1996'] <- NA",
               "tmin['2014/2015'] <- NA", "tmin['2010/2013'] <- NA", # NEW CHANGE
               "tmax['2008/2016'] <- NA"),
  PE100070 = c("tmin['2010/2014'] <- NA",
               "tmin['1982/1983'] <- NA",
               "tmax['1982/1983'] <- NA",
               "tmax['1988/1990'] <- NA",
               "tmax['2010/2014'] <- NA"),
  PE105014 = c("tmin['/2010'] <- NA",
               "tmax['/2010'] <- NA"),
  PE109019 = c("tmin['/2009'] <- NA",
               "tmin['2011/2012'] <- NA"),
  PE109040 = c("tmin['/2015'] <- NA",
               "tmax['/2015'] <- NA"),
  PE115088 = c("tmin['/2008'] <- NA",
               "tmax['/2013'] <- NA"),
  PE115078 = c("tmin['2009/2016'] <- NA"), # NMH
  PE112135 = c("tmin['/2010'] <- NA",
               "tmax['/2011-06'] <- NA"),
  PE113021 = c("tmin['/1969'] <- NA",
               "tmin['1977/1997'] <- NA",
               "tmin['2006/2013'] <- NA",
               "tmin['2016/2017'] <- NA",
               "tmax['/1980'] <- NA"),
  PE112142 = c("tmin['/2004'] <- NA",
               "tmax['/2004'] <- NA"),
  PE111095 = c("tmin['/2009'] <- NA",
               "tmax['/2008'] <- NA"),
  PE109027 = c("tmin['/1999'] <- NA", # NMH
               "tmax['/1970'] <- NA"),
  # very bad time series
  EC000146 = c("tmin['/1972'] <- NA", ### # to delete
               "tmin['1982/1992'] <- NA",
               "tmin['1995/1997'] <- NA",
               "tmin['2008/2012'] <- NA",
               "tmax['1976/1984'] <- NA"),
  PE100065 = c("tmin['1990/2020'] <- NA", # ADD
               "tmax['1990/2020'] <- NA"),
  PE100150 = c("tmin['/1967'] <- NA", # NDD ###
               "tmin['1990/1997'] <- NA",
               "tmin['2003/2008'] <- NA",
               "tmin['2012/2020'] <- NA",
               "tmax['/1967'] <- NA",
               "tmax['2013/2020'] <- NA"),
  PE103036 = c("tmin['1967/1971'] <- NA", # NDD
               "tmin['2016/2020'] <- NA",
               "tmax['2014/2020'] <- NA"),
  PE103038 = c("tmin['1982/1987'] <- NA", # NDD
               "tmin['2014/2020'] <- NA",
               "tmax['1982/1987'] <- NA",
               "tmax['2014/2020'] <- NA",
               "tmax[tmax > 39] <- NA"), ###
  PE103044 = c("tmin['1992/1993'] <- NA", # NDD
               "tmin['2007/2009'] <- NA",
               "tmin['2019/2020'] <- NA",
               "tmax['/1999'] <- NA",
               "tmax['2005'] <- NA"),
  PE104023 = c("tmin['2000/2016'] <- NA", # ADD
               "tmax['2000/2016'] <- NA"),
  PE105067 = c("tmin['/1972'] <- NA", # NDD ###
               "tmin['1988/2001'] <- NA",
               "tmin['2016/2020'] <- NA",
               "tmax['/1966'] <- NA",
               "tmax['1983/1998'] <- NA"),
  PE106017 = c("tmax['1960/2020'] <- NA"), # ADD
  PE106022 = c("tmin['/2016'] <- NA", # NMH
               "tmax['/1995'] <- NA"),
  PE106040 = c("tmin['1975/1977'] <- NA", # NDD
               "tmin['1969/1970'] <- NA",
               "tmax['2006/2020'] <- NA"),
  PE106057 = c("tmin['/1963'] <- NA", # NDD
               "tmin['1977/1985'] <- NA",
               "tmin['2004/2020'] <- NA",
               "tmax['/1963'] <- NA",
               "tmax['1977/1985'] <- NA",
               "tmax['2004/2015'] <- NA"),
  PE107005 = c("tmax['2008/'][tmax['2008/'] > 27] <- NA"), # NDD
  PE107006 = c("tmin['/1985'] <- NA", # NMH
               "tmin[tmin > 18] <- NA"),
  # "tmax['1986/1998'] <- NA"), # NEW CHANGE
  PE107010 = c("tmin['1974/1983'] <- tmin['1974/1983'] + 2.5", # NDD
               "tmin['2017/2019'] <- NA",
               "tmax['2017/2020'] <- NA"),
  PE107052 = c("tmin['/1999'] <- NA", # NMH
               "tmax['/1999'] <- NA"),
  PE108001 = c("tmin['1987/2020'] <- NA", # ADD
               "tmax['1987/2020'] <- NA"),
  PE108047 = c("tmin['2003/2020'] <- NA", # ADD NDD
               "tmax['2003/2020'] <- NA"),
  PE108048 = c("tmin['2015/2020'] <- NA", # ADD NDD
               "tmax['2015/2020'] <- NA"),
  PE109046 = c("tmin['2003/2020'] <- NA", # ADD NDD
               "tmax['2003/2020'] <- NA"),
  PE110007 = c("tmin['1993/2020'] <- NA", # ADD NDD
               "tmax['1993/2020'] <- NA"),
  PE110018 = c("tmin['1963/2020'] <- NA", # ADD NDD
               "tmax['1963/2020'] <- NA"),
  PE110019 = c("tmin['/1992'] <- NA", # NDD
               "tmin['2012/2020'] <- NA",
               "tmax['/1992'] <- NA"),
  PE110039 = c("tmin['2000/2020'] <- NA", # ADD NDD
               "tmax['2000/2020'] <- NA"),
  PE110053 = c("tmin['2004/2020'] <- NA", # ADD NDD
               "tmax['2004/2020'] <- NA"),
  PE111044 = c("tmin['1948/1996'] <- NA", # ADD
               "tmax['1948/1996'] <- NA"),
  PE111076 = c("tmin['2014/2020'] <- NA", # ADD NDD
               "tmax['2014/2020'] <- NA"),
  PE112133 = c("tmin['2003/2020'] <- NA", # ADD NDD
               "tmax['2003/2020'] <- NA"),
  PE112134 = c("tmin['2003/2020'] <- NA", # ADD NDD
               "tmax['2003/2020'] <- NA"),
  PE113038 = c("tmax['2014/2020'] <- NA"), # ADD NDD
  PE113067 = c("tmin['2001/2020'] <- NA", # ADD NDD
               "tmax['2001/2020'] <- NA"),
  PE113086 = c("tmin['2002/2020'] <- NA", # ADD NDD
               "tmax['2002/2020'] <- NA"),
  PE113087 = c("tmin['2002/2020'] <- NA", # ADD NDD
               "tmax['2002/2020'] <- NA"),
  PE114049 = c("tmin['2000/2020'] <- NA", # ADD NDD
               "tmax['2000/2020'] <- NA"),
  PE114055 = c("tmin['2003/2020'] <- NA", # ADD NDD
               "tmax['2003/2020'] <- NA"),
  PE114096 = c("tmin['/2004'] <- NA",
               "tmax['2003/2020'] <- NA"), # ADD
  PE116051 = c("tmin['2015/2020'] <- NA",
               "tmax['2015/2020'] <- NA"), # NDD ADD
  PE117010 = c("tmin['2006/2020'] <- NA",
               "tmax['2009/2020'] <- NA"), # NDD ADD
  PE117030 = c("tmin['2000/2020'] <- NA",
               "tmax['2000/2020'] <- NA"), # NDD ADD
  PE116025 = c("tmin['/2001'] <- NA", # NDD
               "tmin['2018/2020'] <- NA",
               "tmax['/2001'] <- NA"),
  PE115086 = c("tmin['/2003'] <- NA", # NDD
               "tmin['2019/2020'] <- NA",
               "tmax['/2004'] <- NA",
               "tmax['2018/2020'] <- NA"),
  PE115043 = c("tmin['1997/2020'] <- NA",
               "tmax['1997/2020'] <- NA"), # NDD ADD
  PE115021 = c("tmin['/2002'] <- NA", # NDD NMH
               "tmin['2017/2020'] <- NA",
               "tmax['1984/2002'] <- NA"),
  PE115020 = c("tmin['/1966'] <- NA", # NDD
               "tmin['1986/2020'] <- NA",
               "tmax['/1985'] <- NA"),
  PE113082 = c("tmin['/2014'] <- NA", # NMH
               "tmax['2006/2007'] <- NA"),
  PE109038 = c("tmin['2004/2020'] <- NA"), #NDD ADD
  PE112067 = c("tmin['/1997'] <- NA", # NDD
               "tmin['2015/2020'] <- NA",
               "tmax['/1995'] <- NA"),
  PE112060 = c("tmax['1998/2020'] <- NA"), # NDD NMH
  PE111029 = c("tmin['/1994'] <- NA", # NDD NMH
               "tmin['2016/2020'] <- NA",
               "tmax['/1994'] <- NA",
               "tmax['2016/2020'] <- NA"),
  PE111023 = c("tmin['1973'] <- NA", # NDD NMH
               "tmin['1983/1986'] <- NA",
               "tmax['1994/2020'] <- NA"),
  BR082610 = c("tmin['/1996'] <- NA", # NDD
               "tmin['2016/2018'] <- NA",
               "tmax['/1997'] <- NA",
               "tmax['2017/2020'] <- NA"),
  PE105062 = c("tmin['/1999'] <- NA", # NDD NMH
               "tmin['2007/2010'] <- NA",
               "tmin['2018/2020'] <- NA",
               "tmax['/1999'] <- NA"),
  PE110021 = c("tmax['1990/1993'] <- NA", # NDD
               "tmax['2012/2020'] <- NA"),
  PE117007 = c("tmin['2012/2020'] <- NA"), # NDD
  BO004074 = c("tmin['/1995'] <- NA"), # NMH
  PE113081 = c("tmax['2001'] <- NA"),
  PE116049 = c("tmin['2002'] <- NA"),
  PE117041 = c("tmin['2001'] <- NA"),
  PE115035 = c("tmin['/1995'] <- NA"), # NMH NEW CHANGE
  PE117043 = c("tmin['/2011-11-01'] <- tmin['/2011-11-01'] + 6"),  # NEW CHANGE
  PE107008 = c("tmin['1980/1981'] <- NA",  # NEW CHANGE
               "tmax['1980/1981'] <- NA"),  # NEW CHANGE
  PE107036 = c("tmin['/2007-11-04'] <- tmin['/2007-11-04'] + 4",  # NEW CHANGE
               "tmin[tmin < 6] <- NA"),  # NEW CHANGE
  PE100046 = c("tmax['/1994'] <- NA"),  # NEW CHANGE
  PE100139 = c("tmax['1989/1992'] <- NA"),  # NEW CHANGE
  PE115025 = c("tmin['/1997'] <- NA"),  # NEW CHANGE
  PE115128 = c("tmin[tmin < 2.5] <- NA")  # NEW CHANGE
)
