#code to reproduce Supplementary Table 2
#"Effect of COVID-19 response policies on walking behavior in US cities"
#author: Esteban Moro
library(data.table)
library(ggplot2)
library(patchwork)
library(scales)
library(stringr)
library(stargazer)

#visualization settings
source("./visualization_settings.R")

#### Load the data

# Load the census data
# Data was downloaded from ACS 2018 release. See get_census_data.R for the exact description of the tables and variables
census.df <- fread("./data/census_tracts_ACS_2018.csv.gz")
census.df$GEOID <- as.character(census.df$GEOID)
census.df$GEOID <- str_pad(census.df$GEOID,side="left",width=11,pad = "0")
census.df$income <- log(census.df$income)


# Load the CDC data
# Census Tract-level Data (GIS Friendly Format), 2019 release
# Downloaded from https://chronicdata.cdc.gov/500-Cities-Places/500-Cities-Census-Tract-level-Data-GIS-Friendly-Fo/k86t-wghb
# Accessed 03-03-2021
cdc <- fread("./data/500_Cities__Census_Tract-level_Data__GIS_Friendly_Format___2019_release.csv.gz")
cdc$TractFIPS <- str_pad(cdc$TractFIPS,side="left",width=11,pad = "0")
cdc <- cdc[TractFIPS %in% as.character(census.df$GEOID)]
cdc$GEOID <- cdc$TractFIPS
#some tracts are not unique and we need to put them together, average weighting
cdc <- cdc[,.(OBESITY_CrudePrev = sum(OBESITY_CrudePrev*Population2010)/sum(Population2010),
              DIABETES_CrudePrev= sum(DIABETES_CrudePrev*Population2010)/sum(Population2010)),
           .(GEOID)]


# Load the City Health Dashboard data
# Downloaded from https://www.cityhealthdashboard.com
# Accessed 01-09-2020
chdb <- fread("./data/CHDB_data_tract_all v9_0.csv.gz")
chdb$stcotr_fips <- str_pad(chdb$stcotr_fips,side="left",width=11,pad = "0")
chdb$GEOID <- chdb$stcotr_fips
#get only the Park access variable
chdb <- chdb[metric_name=="Park access"]
chdb$Park_access <- chdb$est

# Put all the tract data together
tracts.df <- merge(census.df,cdc[,c("GEOID","OBESITY_CrudePrev","DIABETES_CrudePrev")],all.x = T)
tracts.df <- merge(tracts.df,chdb[,c("GEOID","Park_access")],all.x=T)
tracts.df <- unique(tracts.df)

# Finally load the walk data
files <- Sys.glob("./data/walks_day_MSA_*.csv.gz")
walks.day.tract <- rbindlist(lapply(files,fread))
walks.day.tract$GEOID <- str_pad(walks.day.tract$GEOID,side="left",width=11,pad = "0")



# Calculate the averages before and after
ndays_before <- length(unique(walks.day.tract$day[walks.day.tract$day < "2020-03-06"]))
wwt_before <- walks.day.tract[day < "2020-03-06",
                              .(td=sum(d_walks*population)/mean(population)),
                              .(GEOID,MSA)]
wwt_before[,td:=td/ndays_before]
wwt_before <- merge(wwt_before,tracts.df,by="GEOID")

ndays_after <- length(unique(walks.day.tract$day[walks.day.tract$day > "2020-06-10"]))
wwt_after<- walks.day.tract[day > "2020-06-10",
                            .(td=sum(d_walks*population)/mean(population)),
                            .(GEOID,MSA)]
wwt_after[,td:=td/ndays_after]
wwt_after <- merge(wwt_after,tracts.df,by="GEOID")


#Get the shapefiles for the tracts. Shapefiles were downloaded from the TIGER service at the Census and put together https://www2.census.gov/geo/tiger/TIGER2020/TRACT/
require(sf)
map_sf <- read_sf("./data/shp/tracts.shp")


#### Model before
map_before <- merge(map_sf,na.omit(wwt_before),by="GEOID")
#calculate the neighbors of each tract
map_before_sp <- as(map_before,"Spatial")
require(spdep)
#remove the polygons with no neighbors
w <- poly2nb(map_before_sp, row.names=map_before_sp$GEOID)
nn <- unlist(lapply(w,sum))
map_before <- map_before[which(nn!=0),]
#recalculate without polygons with no neighbors
map_before_sp <- as(map_before,"Spatial")
w <- poly2nb(map_before_sp, row.names=map_before_sp$GEOID)
wm <- nb2mat(w, style='B',zero.policy = T)
rwm <- mat2listw(wm, style='W')

#OLS fit before
fit_before <- lm(td~scale(income) + scale(fblack) + scale(fpublic) + scale(fage64) + scale(Park_access) + scale(OBESITY_CrudePrev) + factor(MSA),data=map_before)
map_before$res_fit1 <- residuals(fit_before)
summary(fit_before)

#Spatially lagged fit before
require(spatialreg)
fit_before_lag <- lagsarlm(td~scale(income) + scale(fblack) + scale(fpublic) + 
                             scale(fage64) + scale(Park_access) + 
                             scale(OBESITY_CrudePrev) + 
                             factor(MSA),data=map_before,rwm,zero.policy = TRUE,quiet = F)

#### Model after
map_after<- merge(map_sf,na.omit(wwt_after),by="GEOID")
#calculate the neighbors of each tract
map_after_sp <- as(map_after,"Spatial")
#remove the polygons with no neighbors
w <- poly2nb(map_after_sp, row.names=map_after_sp$GEOID)
nn <- unlist(lapply(w,sum))
map_after <- map_after[which(nn!=0),]
#recalculate without polygons with no neighbors
map_after_sp <- as(map_after,"Spatial")
w <- poly2nb(map_after_sp, row.names=map_after_sp$GEOID)
wm <- nb2mat(w, style='B',zero.policy = T)
rwm <- mat2listw(wm, style='W')

#OLS fit after
fit_after <- lm(td~scale(income) + scale(fblack) + scale(fpublic) + scale(fage64) + scale(Park_access) + scale(OBESITY_CrudePrev) + factor(MSA),data=map_after)
map_after$res_fit1 <- residuals(fit_after)
summary(fit_after)

#Spatially lagged fit after
require(spatialreg)
fit_after_lag <- lagsarlm(td~scale(income) + scale(fblack) + scale(fpublic) + 
                            scale(fage64) + scale(Park_access) + 
                            scale(OBESITY_CrudePrev) + 
                            factor(MSA),data=map_after,rwm,zero.policy = TRUE,quiet = F)


stargazer(fit_before,fit_before_lag,fit_after,fit_after_lag,omit="factor",type = "text")

