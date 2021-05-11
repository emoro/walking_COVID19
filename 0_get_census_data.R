library(data.table)
library(totalcensus)
library(tidycensus)
library(tidyverse)
library(tigris)

#Get first the tables for each census tract in the US
tabla_census <- data.table()
states_available <- unique(fips_codes$state)
#remove American Samoa,Guam, and Northern Marianas, Palau
states_available <- setdiff(states_available,c("AS","GU","MP","PW","UM","VI"))
for(k in 1:length(states_available)){
  cat(k," >> ",states_available[k],"\n")
  census_data <- read_acs5year(2018, states = states_available[k],
                               table_contents = c(
                                 "income = B19013_001",
                                 "no_insurance = B27010_066", 
                                 "married = B11001_003",
                                 "white = B02001_002",
                                 "black = B02001_003",
                                 'asian = B02001_005', 
                                 'ameindian = B02001_004',
                                 'hispanic = B03002_012',
                                 "education_1 = B15002_014",
                                 "education_2 = B15002_015",
                                 "education_3 = B15002_016",
                                 "education_4 = B15002_017",
                                 "education_5 = B15002_018",
                                 "education_6 = B15002_031",
                                 "education_7 = B15002_032",
                                 "education_8 = B15002_033",
                                 "education_9 = B15002_034",
                                 "education_10 = B15002_035",
                                 "units_1 = B11011_004",
                                 "units_2 = B11011_009",
                                 "units_3 = B11011_013",
                                 "units_4 = B11011_017",
                                 "age64_1 = B01001_020",
                                 "age64_2 = B01001_021",
                                 "age64_3 = B01001_022",
                                 "age64_4 = B01001_023",
                                 "age64_5 = B01001_024",
                                 "age64_6 = B01001_025",
                                 "age64_7 = B01001_044",
                                 "age64_8 = B01001_045",
                                 "age64_9 = B01001_046",
                                 "age64_10 = B01001_047",
                                 "age64_11 = B01001_048",
                                 "age64_12 = B01001_049",
                                 "car = B08301_002",
                                 "work_home = B08301_021", 
                                 "total_workers = B08301_001", 
                                 'public_transportation = B08301_010', 
                                 "HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE = B11016_001", 
                                 "HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE2 = B11016_003", 
                                 "HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE3 = B11016_004", 
                                 "HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE4 = B11016_005", 
                                 "HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE5 = B11016_006", 
                                 "HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE6 = B11016_007", 
                                 "HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE7 = B11016_008", 
                                 "HOUSEHOLD_BY_PRESENCE_UNDER_18_WITH = B11005_002",
                                 "units_total = B25002_001",
                                 "units_vacant = B25002_003",
                                 "units_rented = B25003_003",
                                 "units_owner = B25003_002",
                                 "year_built_total = B25034_001",
                                 "year_built_2014_or_later = B25034_002",
                                 "year_built_2010_2013 = B25034_003",
                                 "year_built_2000_2009 = B25034_004",
                                 "year_built_1990_1999 = B25034_005",
                                 "year_built_1980_1989 = B25034_006",
                                 "year_built_1970_1979 = B25034_007",
                                 "year_built_1960_1969 = B25034_008",
                                 "year_built_1950_1959 = B25034_009",
                                 "year_built_1940_1949 = B25034_010",
                                 "year_built_1939_or_earlier = B25034_011",
                                 'Healthcare_M = C24020_016', 'Healthcare_F = C24020_052', 
                                 'Service_occupations_M = C24020_019', 
                                 'Service_occupations_F = C24020_055', 
                                 'Natural_resource_constr_maintenance_M = C24020_030', 
                                 'Natural_resource_constr_maintenance_F = C24020_066', 
                                 'Productin_Trans_Moving_M = C24020_034', 
                                 'Productin_Trans_Moving_F = C24020_070'), 
                               summary_level="tract",
                               show_progress = TRUE)
  census_data <- as.data.table(census_data)
  census_data[,GEOID:=substr(GEOID,8,25)]
  tabla_census <- rbind(tabla_census,census_data)
}
tabla_census[,age64:=age64_1+age64_2+age64_3+age64_4+age64_5+age64_6+
               age64_7+age64_8+age64_9+age64_10+age64_11+age64_12]

tabla_census[,education:=education_1+education_2+education_3+education_4+
               education_5+education_6+education_7+education_8+education_9+education_10]
tabla_census[,one_unit:=units_1+units_2+units_3+units_4]

#add the Core Base Statistical area for each CBS
counties_cbsa <- fread("/data/safegraph/map_counties_cbsa.txt",colClasses = c(rep("character",4)))
tabla_census$cbsa <- counties_cbsa$cbsa[match(substr(tabla_census$GEOID,1,5),counties_cbsa$GEOID)]
tabla_census <- unique(tabla_census)


#add the density
### compute the density 
density_df <- data.frame()
for (state in states_available){
  area_tract_2018 <- tracts(year = 2018, state = state, 
                            cb = TRUE, class = "sf", progress_bar = FALSE) %>%
    mutate(area = ALAND / 2589988) %>%
    dplyr::select(GEOID, area)
  density_df_tmp = data.frame(GEOID = area_tract_2018$GEOID, area = area_tract_2018$area)
  density_df = rbind(density_df, density_df_tmp)
}
tabla_census <- merge(tabla_census,density_df,by="GEOID")

#add the 2016 voting results by county
#from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
voting <- fread("/data/safegraph/countypres_2000-2016.csv")
voting <- voting[year=="2016" & !is.na(party)]
voting.rep <- voting[,.(trump_vote=candidatevotes[party=="republican"]/mean(totalvotes)),.(FIPS)]
voting.rep$FIPS <- str_pad(voting.rep$FIPS,5,side="left",pad="0")
tabla_census$trump_vote <- voting.rep$trump_vote[match(substr(tabla_census$GEOID,1,5),voting.rep$FIPS)]


independent.vars <- tabla_census %>% transmute(
  GEOID = GEOID,cbsa=cbsa,
  fblack = black/population,fwhite=white/population,fasian=asian/population,
  fhispanic = hispanic/population,
  fameindian = ameindian/population,
  fedu = education/population,fcar=car/population,fpublic=public_transportation/population,
  fmarried = married/HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE,fworkhome=work_home/population,fnoinsurance=no_insurance/population,
  fhousehold2 = HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE2/HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE,
  fhousehold3 = HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE3/HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE,
  fhousehold4 = HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE4/HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE,
  fhousehold5 = HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE5/HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE,
  fhousehold6 = HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE6/HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE,
  fhousehold7 = HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE7/HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE,
  fwithkids = HOUSEHOLD_BY_PRESENCE_UNDER_18_WITH/HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE,
  fnecessary_workers = (Healthcare_M+Healthcare_F+Service_occupations_M+
                          Service_occupations_F+
                          Natural_resource_constr_maintenance_M+
                          Natural_resource_constr_maintenance_F)/total_workers,
  population = population,
  income = log(income),
  fage64 = age64/population,
  density = log(population/area),
  density_h=log(HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE/area),
  f_units_2000_or_later = (year_built_2000_2009+year_built_2010_2013+year_built_2014_or_later)/year_built_total,
  f_units_1939_or_earlier = year_built_1939_or_earlier/year_built_total,
  f_units_occupied = 1-units_vacant/units_total,
  f_units_rented = units_rented/(units_total-units_vacant),
  f_one_unit = one_unit/HOUSEHOLD_TYPE_BY_HOUSEHOLD_SIZE,
  lat=lat,
  trump_vote=trump_vote
)

fwrite(independent.vars,file="./data/census_tracts.csv")


