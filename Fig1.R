#code to reproduce Figure 1 of 
#"Effect of COVID-19 response policies on walking behavior in US cities"
#author: Esteban Moro
library(data.table)
library(ggplot2)
library(patchwork)
library(scales)

#visualization settings
source("./visualization_settings.R")


#read the walks by day, tract and area
files <- Sys.glob("./data/walks_day_MSA_*.csv.gz")
walks.day.tract <- rbindlist(lapply(files,fread))

#aggregate by area
walks.day.area <- walks.day.tract[,.(n_walks=sum(n_walks*population)/sum(population),
                                     d_walks=sum(d_walks*population)/sum(population),
                                     n_walks_leisure=sum(n_walks_leisure*population)/sum(population),
                                     n_walks_utilitarian=sum(n_walks_utilitarian*population)/sum(population),
                                     d_walks_leisure=sum(d_walks_leisure*population)/sum(population),
                                     d_walks_utilitarian=sum(d_walks_utilitarian*population)/sum(population),
                                     population=sum(population)),.(day,MSA)]
walks.day.area[,name:=names[match(MSA,areas)]]
#aggregate all
walks.day.total <- walks.day.tract[,.(n_walks=sum(n_walks*population)/sum(population),
                                      d_walks=sum(d_walks*population)/sum(population),
                                      n_walks_leisure=sum(n_walks_leisure*population)/sum(population),
                                      n_walks_utilitarian=sum(n_walks_utilitarian*population)/sum(population),
                                      d_walks_leisure=sum(d_walks_leisure*population)/sum(population),
                                      d_walks_utilitarian=sum(d_walks_utilitarian*population)/sum(population),
                                      population=sum(population)),.(day)]
walks.day.total$MSA <- "00000"
walks.day.total$name <- "Total"

walks_fig1 <- rbind(walks.day.area,walks.day.total)


#Figure 1, Panel B
cols <- c("Total"="#000000","Leisure"=col_leisure,"Utilitarian"=col_utilitarian)
span_loess <- 0.12
#select the dates to visualize
date_min <- "2020-02-18"
date_max <- "2020-06-25"
dataset <- walks_fig1[day > date_min & day <= date_max]
fig_total_nwalks <- ggplot() + 
  geom_smooth(data=dataset[name=="Total"], aes(x=as.Date(day),y=n_walks,col="Total"),span=span_loess,se=F,lwd=0.6*1.2) + 
  geom_smooth(data=dataset[name=="Total"], aes(x=as.Date(day),y=n_walks_utilitarian,col="Utilitarian"),span=span_loess,se=F,lwd=0.5*1.2)+
  geom_smooth(data=dataset[name=="Total"], aes(x=as.Date(day),y=n_walks_leisure,col="Leisure"),span=span_loess,se=F,lwd=0.5*1.2)+
  theme_paper +
  theme(legend.key = element_rect(fill = NA),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height =  unit(0.2, "cm")
  ) + 
  xlab("Date")+ylab("Average number of bouts")+
  scale_y_continuous() + 
  geom_vline(xintercept=as.Date("2020-03-13"),col="red",linetype="dashed") +
  scale_colour_manual(name=" ",values=cols,breaks=c("Leisure","Utilitarian","Total")) + 
  #  geom_text(aes(x=as.Date("2020-03-13"),y=0,label="National emergency"))
  theme(legend.position = c(0.8,0.9)) + guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.text=element_text(size=14))

#Figure 1, Panel C
fig_total_distance <- ggplot() + 
  geom_smooth(data=dataset[name=="Total"], aes(x=as.Date(day),y=d_walks),span=span_loess,se=F,lwd=0.6*1.2,col="black") + 
  geom_smooth(data=dataset[name=="Total"], aes(x=as.Date(day),y=d_walks_utilitarian),span=span_loess,se=F,lwd=0.5*1.2,col=col_utilitarian)+
  geom_smooth(data=dataset[name=="Total"], aes(x=as.Date(day),y=d_walks_leisure),span=span_loess,se=F,lwd=0.5*1.2,col=col_leisure)+
  theme_paper +
  theme(legend.key = element_rect(fill = NA),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height =  unit(0.2, "cm")
  ) + scale_color_tableau() + xlab("Date")+ylab("Average distance (meters)")+ 
  geom_vline(xintercept=as.Date("2020-03-13"),col="red",linetype="dashed")+
  scale_y_continuous()

#Figure 1, Panel D
t1 <- "2020-03-03"
t2 <- "2020-06-01"
dataset <- walks_fig1[day > "2020-02-16" & day < "2020-07-01"]
ndays_before <- length(unique(dataset$day[dataset$day < t1]))
ndays_after <- length(unique(dataset$day[dataset$day > t2]))
percentage_change <- dataset[,.(change_distance = (sum(d_walks[day > t2])/ndays_after-sum(d_walks[day < t1])/ndays_before)/(sum(d_walks[day < t1])/ndays_before)),
                             .(name,MSA)]
fig_change <- ggplot(percentage_change[name!="Total"],aes(x=reorder(name,change_distance),y=change_distance)) + 
  geom_bar(stat="identity",col="darkred",fill="darkred",alpha=0.2,width = 0.75) + 
  scale_fill_tableau("Tableau 20") + coord_flip() + 
  xlab("")+ylab("Change in distance") + theme_paper + 
  scale_y_continuous(labels = scales::percent,breaks=c(-0.4,-0.2,0)) + 
  geom_hline(yintercept=percentage_change$change_distance[percentage_change$name=="Total"],linetype="dashed")

fig_total_nwalks + fig_total_distance + fig_change
