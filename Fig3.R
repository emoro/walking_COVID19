#code to reproduce Figure 3 of 
#"Effect of COVID-19 response policies on walking behavior in US cities"
#author: Esteban Moro
library(data.table)
library(ggplot2)
library(patchwork)
library(scales)
library(stringr)
library(tidycensus)


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


# let's build the walk activity time series for each quantile in each variable
find_quants <- function(x,n){
  qq <- quantile(x,probs = seq(0,1,length=n+1),na.rm=T)
  classes <- paste0(names(qq)[-length(qq)]," - ",names(qq)[-1])
  classes[as.numeric(cut(x,breaks = unique(qq),include.lowest = T))]
}

# income
tracts.df[,qq:=find_quants(income,5)]
wplot <- merge(walks.day.tract,tracts.df[,c("GEOID","qq")],by="GEOID")
income.df <- wplot[day > "2020-02-16",
                   .(md = sum(d_walks*population)/sum(population),
                     mdu= sum(d_walks_utilitarian*population)/sum(population),
                     mdl= sum(d_walks_leisure*population)/sum(population)),
                   .(qq,day)][!is.na(qq)]
income.df$var <- "Median Income"


# higher education
tracts.df[,qq:=find_quants(fbachelor,5)]
wplot <- merge(walks.day.tract,tracts.df[,c("GEOID","qq")],by="GEOID")
education.df <- wplot[day > "2020-02-16",
                   .(md = sum(d_walks*population)/sum(population),
                     mdu= sum(d_walks_utilitarian*population)/sum(population),
                     mdl= sum(d_walks_leisure*population)/sum(population)),
                   .(qq,day)][!is.na(qq)]
education.df$var <- "% Bachelor degree"

#Public Transportation
tracts.df[,qq:=find_quants(fpublic,5)]
wplot <- merge(walks.day.tract,tracts.df[,c("GEOID","qq")],by="GEOID")
ptrans.df <- wplot[day > "2020-02-16",
                      .(md = sum(d_walks*population)/sum(population),
                        mdu= sum(d_walks_utilitarian*population)/sum(population),
                        mdl= sum(d_walks_leisure*population)/sum(population)),
                      .(qq,day)][!is.na(qq)]
ptrans.df$var <- "Use of Public Transportation"
#The first two quintiles are equal, so we rename the last quintile for the visualization
ptrans.df$qq[ptrans.df$qq=="60% - 80%"] <- "80% - 100%"

#Necessary workers
tracts.df[,qq:=find_quants(fnecessary_workers,5)]
wplot <- merge(walks.day.tract,tracts.df[,c("GEOID","qq")],by="GEOID")
nworkers.df <- wplot[day > "2020-02-16",
                   .(md = sum(d_walks*population)/sum(population),
                     mdu= sum(d_walks_utilitarian*population)/sum(population),
                     mdl= sum(d_walks_leisure*population)/sum(population)),
                   .(qq,day)][!is.na(qq)]
nworkers.df$var <- "Fraction of necessary workers"

#Park Access
tracts.df[,qq:=find_quants(Park_access,5)]
wplot <- merge(walks.day.tract,tracts.df[,c("GEOID","qq")],by="GEOID")
paccess.df <- wplot[day > "2020-02-16",
                   .(md = sum(d_walks*population)/sum(population),
                     mdu= sum(d_walks_utilitarian*population)/sum(population),
                     mdl= sum(d_walks_leisure*population)/sum(population)),
                   .(qq,day)][!is.na(qq)]
paccess.df$var <- "Park Access"
#The last two quartiles are equal, so we merge them
paccess.df$qq[paccess.df$qq=="60% - 80%"] <- "80% - 100%"

#Obesity
tracts.df[,qq:=find_quants(OBESITY_CrudePrev,5)]
wplot <- merge(walks.day.tract,tracts.df[,c("GEOID","qq")],by="GEOID")
obesity.df <- wplot[day > "2020-02-16",
                    .(md = sum(d_walks*population)/sum(population),
                      mdu= sum(d_walks_utilitarian*population)/sum(population),
                      mdl= sum(d_walks_leisure*population)/sum(population)),
                    .(qq,day)][!is.na(qq)]
obesity.df$var <- "Obesity Prevalence"

#### Plots
#merge all the data to plot
total.df <- rbind(income.df,education.df,ptrans.df,nworkers.df,paccess.df,obesity.df)


#### Fig 3 Panel A
span_loess <- 0.1
fig3a <- ggplot() + 
  geom_smooth(data= total.df[var=="Median Income"] %>% dplyr::filter(qq %in% c("0% - 20%","80% - 100%")), aes(x=as.Date(day),y=md,col=factor(qq),group=qq),span=span_loess,se=F,lwd=0.6*1.2) + 
  geom_smooth(data= total.df[var=="Median Income"] %>% dplyr::filter(!(qq %in% c("0% - 20%","80% - 100%"))), aes(x=as.Date(day),y=md,col=factor(qq),group=qq),span=span_loess,se=F,lwd=0.2*1.2) + 
  theme_paper + facet_grid(~var)+
  theme(legend.key = element_rect(fill = NA),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height =  unit(0.2, "cm")
  ) + scale_color_viridis(discrete=T,option="D",direction=-1,begin=0,end=0.96,alpha=1) + 
  guides(color = guide_legend(override.aes = list(size=2)))+
  labs(color = "Quintile:") + xlab("Date")+ylab("Daily distance walked")+
  geom_vline(xintercept=as.Date("2020-03-13"),col="red",linetype="dashed") +
  scale_y_continuous(limits=c(75,375)) + theme(legend.position="none")
fig3a

#### Fig 3 Panel B
# Let's compute the change of utilitarian and leisure walked distance by quantile of income
before <- income.df[day < "2020-03-06",.(mdu=mean(mdu),mdl=mean(mdl)),.(qq)]
before$type="before"
after <- income.df[day > "2020-06-10",.(mdu=mean(mdu),mdl=mean(mdl)),.(qq)]
after$type="after"
total <- rbind(after,before) 
total.qq <- total[,.(Leisure = (mdl[type=="after"]-mdl[type=="before"])/mdl[type=="before"],
                     Utilitarian = (mdu[type=="after"]-mdu[type=="before"])/mdu[type=="before"]),
                  .(qq)]
#and the total change
total.after <- walks.day.tract[day > "2020-06-10",
                               .(mdu=sum(d_walks_utilitarian*population)/sum(population),
                                 mdl=sum(d_walks_leisure*population)/sum(population))]
total.after$type <- "after"
total.before <- walks.day.tract[day < "2020-03-06",
                                .(mdu=sum(d_walks_utilitarian*population)/sum(population),
                                  mdl=sum(d_walks_leisure*population)/sum(population))]
total.before$type <- "before"
tt <- rbind(total.after,total.before)
tt.df <- tt[,.(Leisure = (mdl[type=="after"]-mdl[type=="before"])/mdl[type=="before"],
               Utilitarian = (mdu[type=="after"]-mdu[type=="before"])/mdu[type=="before"])]
tt.df$qq <- "Total"

#put them together and visualize
final.df <- rbind(tt.df,total.qq)
mm <- melt(final.df,id.vars = "qq")
fig3b <- ggplot(mm,aes(x=value,y=qq,fill=variable)) + 
  geom_bar(stat="identity",position="dodge",width=0.5) + 
  theme_paper + xlab("Change") + 
  geom_vline(xintercept = 0,linetype="dashed")+ 
  scale_fill_manual(name="",values=c(col_leisure,col_utilitarian)) + 
  scale_x_continuous(labels = scales::percent) + 
  xlab("Change in distance") + ylab("Income Quintile") + theme(legend.position="none")
fig3b

#### Fig 4 Panel C
tt <- total.df[var %in% c("Use of Public Transportation","Park Access","Obesity Prevalence")]
fig3c <- ggplot(tt) + 
  geom_smooth(data= tt %>% dplyr::filter(qq %in% c("0% - 20%","80% - 100%")), aes(x=as.Date(day),y=md,col=factor(qq),group=qq),span=span_loess,se=F,lwd=0.6*1.2) + 
  geom_smooth(data= tt %>% dplyr::filter(!(qq %in% c("0% - 20%","80% - 100%"))), aes(x=as.Date(day),y=md,col=factor(qq),group=qq),span=span_loess,se=F,lwd=0.2*1.2) + 
  facet_wrap(~var) + theme_paper +
  theme(legend.key = element_rect(fill = NA),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height =  unit(0.2, "cm")
  ) + scale_color_viridis(discrete=T,option="D",direction=-1,begin=0,end=0.96,alpha=1) + 
  guides(color = guide_legend(override.aes = list(size=2)))+
  labs(color = "Quintile:") + xlab("Date")+ylab("Daily distance walked")+
  geom_vline(xintercept=as.Date("2020-03-13"),col="red",linetype="dashed") 
fig3c
