#code to reproduce Table 1 of
#"Effect of COVID-19 response policies on walking behavior in US cities"
#author: Esteban Moro
library(data.table)
library(ggplot2)
library(patchwork)
library(scales)
library(stringr)


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
fit_before <- lm(td ~ scale(income) + scale(fblack) + scale(fpublic) + scale(fage64) + scale(Park_access) + scale(OBESITY_CrudePrev)+factor(MSA),data=wwt_before)
summary(fit_before)

ndays_after <- length(unique(walks.day.tract$day[walks.day.tract$day > "2020-06-10"]))
wwt_after<- walks.day.tract[day > "2020-06-10",
                              .(td=sum(d_walks*population)/mean(population)),
                              .(GEOID,MSA)]
wwt_after[,td:=td/ndays_after]
wwt_after <- merge(wwt_after,tracts.df,by="GEOID")
fit_after <- lm(td ~ scale(income) + scale(fblack) + scale(fpublic) + scale(fage64) + scale(Park_access) + scale(OBESITY_CrudePrev)+factor(MSA),data=wwt_after)
summary(fit_after)


library(stargazer)
stargazer(fit_before,fit_after,omit="factor",single.row = T)
