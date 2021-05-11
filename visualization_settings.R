#visualization settings for the Figures in 
#"Effect of COVID-19 response policies on walking behavior in US cities"
#author: Esteban Moro
library(ggthemes)
library(viridis)


#colors
col_utilitarian <- rgb(242,142,42,maxColorValue = 256)
col_leisure <- rgb(72,117,165,maxColorValue = 256)

#names of the MSA areas
areas <- c("14460","16980","19100","31080","33100","35620","37980","41860","42660","47900")
names <- c("Boston","Chicago","Dallas","Los Angeles","Miami","New York","Philadelphia","San Francisco","Seattle","Washington DC")

## ggplot theme
font_selected <- "Helvetica"
theme_paper <- theme_hc() + theme(
  text=element_text(family=font_selected)
)