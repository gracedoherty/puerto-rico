
# ---------- WORLDPOP ------------

#0 prep
getwd()
setwd("/Users/grace/OneDrive/Documents/R")
library(sf)
memory.limit()



#1.1 Each facility, baseline scenario, driving time and multimodal
D_mm_base <- read.csv('D_mm.csv')
P_mm_base <- read.csv('P_mm.csv')
H_mm_base <- read.csv('H_mm.csv')

P_mm_base <- P_mm_base[ , c('wid', 'X1P', 'mm1P')]
mm_base <- merge(x = D_mm_base, y = P_mm_base, by = "wid", all.x = TRUE) 

H_mm_base <- H_mm_base[ , c('wid', 'X1H', 'mm1H')]
mm_base <- merge(x = mm_base, y = H_mm_base, by = "wid", all.x = TRUE) 

mm_base <- mm_base[ , -c(2,6,7)] # Drop X, NN, walk_time
write.csv(mm_base, file = "mm_base.csv")



#1.2 Each facility, flood scenario, driving time and multimodal
D_mm_flood <- read.csv('D_mm_flood.csv')
P_mm_flood <- read.csv('P_mm_flood.csv')
H_mm_flood <- read.csv('H_mm_flood.csv')

P_mm_flood <- P_mm_flood[ , c('wid', 'X1P', 'mm1P')]
mm_flood <- merge(x = D_mm_flood, y = P_mm_flood, by = "wid", all.x = TRUE) 

H_mm_flood <- H_mm_flood[ , c('wid', 'X1H', 'mm1H')]
mm_flood <- merge(x = mm_flood, y = H_mm_flood, by = "wid", all.x = TRUE) 

mm_flood <- mm_flood[ , -c(2,6,7)] # Drop X, NN, walk_time
write.csv(mm_flood, file = "mm_flood.csv")



#1.3 Each facility, landslide scenario, driving time and multimodal
D_mm_landslide <- read.csv('D_mm_landslide.csv')
P_mm_landslide <- read.csv('P_mm_landslide.csv')
H_mm_landslide <- read.csv('H_mm_landslide.csv')

P_mm_landslide <- P_mm_landslide[ , c('wid', 'X1P', 'mm1P')]
mm_landslide <- merge(x = D_mm_landslide, y = P_mm_landslide, by = "wid", all.x = TRUE) 

H_mm_landslide <- H_mm_landslide[ , c('wid', 'X1H', 'mm1H')]
mm_landslide <- merge(x = mm_landslide, y = H_mm_landslide, by = "wid", all.x = TRUE) 

mm_landslide <- mm_landslide[ , -c(2,6,7)] # Drop X, NN, walk_time
write.csv(mm_landslide, file = "mm_slide.csv")



#2.1 Join baseline data to georeferenced file
wpop <- st_read('wpop3wgs84.shp')

base_wp <- merge(x = wpop, y = mm_base, by = "wid", all.x = TRUE) 
sum(is.na(base_wp$X1D)) # Test for each travel time column to see if there was any join issue. 3893 NA values.
st_write(base_wp, "base_wp", driver="ESRI Shapefile")



#2.2 Join flooding data to georeferenced file
flood_wp <- merge(x = wpop, y = mm_flood, by = "wid", all.x = TRUE) 
sum(is.na(flood_wp$X1D)) # Test for each travel time column to see if there was any join issue.
st_write(flood_wp, "flood_wp", driver="ESRI Shapefile")



#2.3 Join landslide data to georeferenced file
mm_landslide <- read.csv('mm_slide.csv')
landslide_wp <- merge(x = wpop, y = mm_landslide, by = "wid", all.x = TRUE) 
sum(is.na(landslide_wp$mm1P)) # Test for each travel time column to see if there was any join issue. #3562 NA values.
landslide_wp <- landslide_wp[ , c('wid', 'wpop.x', 'xmid.x', 'municipio.x', 'X1D', 'mm1D', 'X1P', 'mm1P', 'X1H', 'mm1H', 'geometry')]
landslide_wp <- rename(landslide_wp, c("xmid.x"="mid"))
st_write(landslide_wp, "slide_wp", driver="ESRI Shapefile")



# ---------- MUNICIPIO ------------

# 0 prep
library(sf)
library(plyr)



#1.1 Load baseline values assigned to WorldPop points.
base_wp <- st_read('base_wp.shp')
base_wp <- rename(base_wp, c("wpop.x"="wpop"))



#1.2 Load flooding values assigned to WorldPop points.
flood_wp <- st_read('flood_wp.shp')
flood_wp <- rename(flood_wp, c("wpop.x"="wpop"))



#1.3 Load landslide values assigned to WorldPop points.
landslide_wp <- st_read('slide_wp.shp')
landslide_wp <- rename(landslide_wp, c("wpop.x"="wpop"))



#2.1 Baseline access as function of proportion of municipal population at each origin
# Dialysis
base_wp$D_mm_wp <- NA
base_wp$D_mm_wp <- with(base_wp, mm1D * wpop) # multimodal
base_wp$D_d_wp <- NA
base_wp$D_d_wp <- with(base_wp, X1D * wpop) # drive time only

# Pharmacy
base_wp$P_mm_wp <- NA
base_wp$P_mm_wp <- with(base_wp, mm1P * wpop) # multimodal
base_wp$P_d_wp <- NA
base_wp$P_d_wp <- with(base_wp, X1P * wpop) # drive time only

# Hospital
base_wp$H_mm_wp <- NA
base_wp$H_mm_wp <- with(base_wp, mm1H * wpop) # multimodal
base_wp$H_d_wp <- NA
base_wp$H_d_wp <- with(base_wp, X1H * wpop) # drive time only



#2.2 Flooding scenario access as function of proportion of municipal population at each origin
# Dialysis
flood_wp$D_mm_wp <- NA
flood_wp$D_mm_wp <- with(flood_wp, mm1D * wpop) # multimodal
flood_wp$D_d_wp <- NA
flood_wp$D_d_wp <- with(flood_wp, X1D * wpop) # drive time only

# Pharmacy
flood_wp$P_mm_wp <- NA
flood_wp$P_mm_wp <- with(flood_wp, mm1P * wpop) # multimodal
flood_wp$P_d_wp <- NA
flood_wp$P_d_wp <- with(flood_wp, X1P * wpop) # drive time only

# Hospital
flood_wp$H_mm_wp <- NA
flood_wp$H_mm_wp <- with(flood_wp, mm1H * wpop) # multimodal
flood_wp$H_d_wp <- NA
flood_wp$H_d_wp <- with(flood_wp, X1H * wpop) # drive time only



#2.2 Landslide scenario access as function of proportion of municipal population at each origin
# Dialysis
landslide_wp$D_mm_wp <- NA
landslide_wp$D_mm_wp <- with(landslide_wp, mm1D * wpop) # multimodal
landslide_wp$D_d_wp <- NA
landslide_wp$D_d_wp <- with(landslide_wp, X1D * wpop) # drive time only

# Pharmacy
landslide_wp$P_mm_wp <- NA
landslide_wp$P_mm_wp <- with(landslide_wp, mm1P * wpop) # multimodal
landslide_wp$P_d_wp <- NA
landslide_wp$P_d_wp <- with(landslide_wp, X1P * wpop) # drive time only

# Hospital
landslide_wp$H_mm_wp <- NA
landslide_wp$H_mm_wp <- with(landslide_wp, mm1H * wpop) # multimodal
landslide_wp$H_d_wp <- NA
landslide_wp$H_d_wp <- with(landslide_wp, X1H * wpop) # drive time only



# 3.1 Baseline municipal pop-weighted access rating. mun rating = (sum(accessvariable*wpop))/wpopm
wpop_m <- as.data.frame(xtabs(wpop ~ mid, base_wp))
wpop_m <- rename(wpop_m, c("Freq"="wpop_m"))

# Dialysis
D_mm_m <- as.data.frame(xtabs(D_mm_wp ~ mid, base_wp))
D_mm_m <- rename(D_mm_m, c("Freq"="D_mm_sum"))
D_mm_m <- merge(x=D_mm_m, y=wpop_m, by="mid")
D_mm_m$D_mm_m <- NA
D_mm_m$D_mm_m <- with(D_mm_m, D_mm_sum / wpop_m)
write.csv(D_mm_m, file = "D_mm_m.csv")

D_d_m <- as.data.frame(xtabs(D_d_wp ~ mid, base_wp))
D_d_m <- rename(D_d_m, c("Freq"="D_d_sum"))
D_d_m <- merge(x=D_d_m, y=wpop_m, by="mid")
D_d_m$D_d_m <- NA
D_d_m$D_d_m <- with(D_d_m, D_d_sum / wpop_m)
write.csv(D_d_m, file = "D_d_m.csv")

# Pharmacy
P_mm_m <- as.data.frame(xtabs(P_mm_wp ~ mid, base_wp))
P_mm_m <- rename(P_mm_m, c("Freq"="P_mm_sum"))
P_mm_m <- merge(x=P_mm_m, y=wpop_m, by="mid")
P_mm_m$P_mm_m <- NA
P_mm_m$P_mm_m <- with(P_mm_m, P_mm_sum / wpop_m)
write.csv(P_mm_m, file = "P_mm_m.csv")

P_d_m <- as.data.frame(xtabs(P_d_wp ~ mid, base_wp))
P_d_m <- rename(P_d_m, c("Freq"="P_d_sum"))
P_d_m <- merge(x=P_d_m, y=wpop_m, by="mid")
P_d_m$P_d_m <- NA
P_d_m$P_d_m <- with(P_d_m, P_d_sum / wpop_m)
write.csv(P_d_m, file = "P_d_m.csv")

# Hospital
H_mm_m <- as.data.frame(xtabs(H_mm_wp ~ mid, base_wp))
H_mm_m <- rename(H_mm_m, c("Freq"="H_mm_sum"))
H_mm_m <- merge(x=H_mm_m, y=wpop_m, by="mid")
H_mm_m$H_mm_m <- NA
H_mm_m$H_mm_m <- with(H_mm_m, H_mm_sum / wpop_m)
write.csv(H_mm_m, file = "H_mm_m.csv")

H_d_m <- as.data.frame(xtabs(H_d_wp ~ mid, base_wp))
H_d_m <- rename(H_d_m, c("Freq"="H_d_sum"))
H_d_m <- merge(x=H_d_m, y=wpop_m, by="mid")
H_d_m$H_d_m <- NA
H_d_m$H_d_m <- with(H_d_m, H_d_sum / wpop_m)
write.csv(H_d_m, file = "H_d_m.csv")



# 3.2 Flooding municipal pop-weighted access rating. mun rating = (sum(accessvariable*wpop))/wpopm
# Dialysis
D_mm_m <- as.data.frame(xtabs(D_mm_wp ~ mid, flood_wp))
D_mm_m <- rename(D_mm_m, c("Freq"="D_mm_sum"))
D_mm_m <- merge(x=D_mm_m, y=wpop_m, by="mid")
D_mm_m$D_mm_m <- NA
D_mm_m$D_mm_m <- with(D_mm_m, D_mm_sum / wpop_m)
write.csv(D_mm_m, file = "D_fl_mm_m.csv")

D_d_m <- as.data.frame(xtabs(D_d_wp ~ mid, flood_wp))
D_d_m <- rename(D_d_m, c("Freq"="D_d_sum"))
D_d_m <- merge(x=D_d_m, y=wpop_m, by="mid")
D_d_m$D_d_m <- NA
D_d_m$D_d_m <- with(D_d_m, D_d_sum / wpop_m)
write.csv(D_d_m, file = "D_fl_d_m.csv")

# Pharmacy
P_mm_m <- as.data.frame(xtabs(P_mm_wp ~ mid, flood_wp))
P_mm_m <- rename(P_mm_m, c("Freq"="P_mm_sum"))
P_mm_m <- merge(x=P_mm_m, y=wpop_m, by="mid")
P_mm_m$P_mm_m <- NA
P_mm_m$P_mm_m <- with(P_mm_m, P_mm_sum / wpop_m)
write.csv(P_mm_m, file = "P_fl_mm_m.csv")

P_d_m <- as.data.frame(xtabs(P_d_wp ~ mid, flood_wp))
P_d_m <- rename(P_d_m, c("Freq"="P_d_sum"))
P_d_m <- merge(x=P_d_m, y=wpop_m, by="mid")
P_d_m$P_d_m <- NA
P_d_m$P_d_m <- with(P_d_m, P_d_sum / wpop_m)
write.csv(P_d_m, file = "P_fl_d_m.csv")

# Hospital
H_mm_m <- as.data.frame(xtabs(H_mm_wp ~ mid, flood_wp))
H_mm_m <- rename(H_mm_m, c("Freq"="H_mm_sum"))
H_mm_m <- merge(x=H_mm_m, y=wpop_m, by="mid")
H_mm_m$H_mm_m <- NA
H_mm_m$H_mm_m <- with(H_mm_m, H_mm_sum / wpop_m)
write.csv(H_mm_m, file = "H_fl_mm_m.csv")

H_d_m <- as.data.frame(xtabs(H_d_wp ~ mid, flood_wp))
H_d_m <- rename(H_d_m, c("Freq"="H_d_sum"))
H_d_m <- merge(x=H_d_m, y=wpop_m, by="mid")
H_d_m$H_d_m <- NA
H_d_m$H_d_m <- with(H_d_m, H_d_sum / wpop_m)
write.csv(H_d_m, file = "H_fl_d_m.csv")



# 3.3 Landslide municipal pop-weighted access rating. mun rating = (sum(accessvariable*wpop))/wpopm
# Dialysis
D_mm_m <- as.data.frame(xtabs(D_mm_wp ~ mid, landslide_wp))
D_mm_m <- rename(D_mm_m, c("Freq"="D_mm_sum"))
D_mm_m <- merge(x=D_mm_m, y=wpop_m, by="mid")
D_mm_m$D_mm_m <- NA
D_mm_m$D_mm_m <- with(D_mm_m, D_mm_sum / wpop_m)
write.csv(D_mm_m, file = "D_sl_mm_m.csv")

D_d_m <- as.data.frame(xtabs(D_d_wp ~ mid, landslide_wp))
D_d_m <- rename(D_d_m, c("Freq"="D_d_sum"))
D_d_m <- merge(x=D_d_m, y=wpop_m, by="mid")
D_d_m$D_d_m <- NA
D_d_m$D_d_m <- with(D_d_m, D_d_sum / wpop_m)
write.csv(D_d_m, file = "D_sl_d_m.csv")

# Pharmacy
P_mm_m <- as.data.frame(xtabs(P_mm_wp ~ mid, landslide_wp))
P_mm_m <- rename(P_mm_m, c("Freq"="P_mm_sum"))
P_mm_m <- merge(x=P_mm_m, y=wpop_m, by="mid")
P_mm_m$P_mm_m <- NA
P_mm_m$P_mm_m <- with(P_mm_m, P_mm_sum / wpop_m)
write.csv(P_mm_m, file = "P_sl_mm_m.csv")

P_d_m <- as.data.frame(xtabs(P_d_wp ~ mid, landslide_wp))
P_d_m <- rename(P_d_m, c("Freq"="P_d_sum"))
P_d_m <- merge(x=P_d_m, y=wpop_m, by="mid")
P_d_m$P_d_m <- NA
P_d_m$P_d_m <- with(P_d_m, P_d_sum / wpop_m)
write.csv(P_d_m, file = "P_sl_d_m.csv")

# Hospital
H_mm_m <- as.data.frame(xtabs(H_mm_wp ~ mid, landslide_wp))
H_mm_m <- rename(H_mm_m, c("Freq"="H_mm_sum"))
H_mm_m <- merge(x=H_mm_m, y=wpop_m, by="mid")
H_mm_m$H_mm_m <- NA
H_mm_m$H_mm_m <- with(H_mm_m, H_mm_sum / wpop_m)
write.csv(H_mm_m, file = "H_sl_mm_m.csv")

H_d_m <- as.data.frame(xtabs(H_d_wp ~ mid, landslide_wp))
H_d_m <- rename(H_d_m, c("Freq"="H_d_sum"))
H_d_m <- merge(x=H_d_m, y=wpop_m, by="mid")
H_d_m$H_d_m <- NA
H_d_m$H_d_m <- with(H_d_m, H_d_sum / wpop_m)
write.csv(H_d_m, file = "H_sl_d_m.csv")



# 4.1 Baseline combine to one file.

base_mm_m = Reduce(function(x, y) merge(x, y, by="mid", all=TRUE), list(D_mm_m, H_mm_m, P_mm_m))
base_mm_m <- base_mm_m[ , c('mid', 'D_mm_m', 'H_mm_m', 'P_mm_m', 'wpop_m')]
write.csv(base_mm_m, file = "base_mm_m.csv")

base_d_m = Reduce(function(x, y) merge(x, y, by="mid", all=TRUE), list(D_d_m, H_d_m, P_d_m))
base_d_m <- base_d_m[ , c('mid', 'D_d_m', 'H_d_m', 'P_d_m', 'wpop_m')]
write.csv(base_d_m, file = "base_d_m.csv")


# 4.2 Flooding combine to one file.

flood_mm_m = Reduce(function(x, y) merge(x, y, by="mid", all=TRUE), list(D_mm_m, H_mm_m, P_mm_m))
flood_mm_m <- flood_mm_m[ , c('mid', 'D_mm_m', 'H_mm_m', 'P_mm_m', 'wpop_m')]
write.csv(flood_mm_m, file = "flood_mm_m.csv")

flood_d_m = Reduce(function(x, y) merge(x, y, by="mid", all=TRUE), list(D_d_m, H_d_m, P_d_m))
flood_d_m <- flood_d_m[ , c('mid', 'D_d_m', 'H_d_m', 'P_d_m', 'wpop_m')]
write.csv(flood_d_m, file = "flood_d_m.csv")



# 4.3 Landslide combine to one file.

landslide_mm_m = Reduce(function(x, y) merge(x, y, by="mid", all=TRUE), list(D_mm_m, H_mm_m, P_mm_m))
landslide_mm_m <- landslide_mm_m[ , c('mid', 'D_mm_m', 'H_mm_m', 'P_mm_m', 'wpop_m')]
write.csv(landslide_mm_m, file = "slide_mm_m.csv")

landslide_d_m = Reduce(function(x, y) merge(x, y, by="mid", all=TRUE), list(D_d_m, H_d_m, P_d_m))
landslide_d_m <- landslide_d_m[ , c('mid', 'D_d_m', 'H_d_m', 'P_d_m', 'wpop_m')]
write.csv(landslide_d_m, file = "slide_d_m.csv")



