install.packages("foreign")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plyr")
install.packages("maps")
install.packages("ggmap")

library(foreign)
library(dplyr)
library(ggplot2)
library(plyr)
library(maps)
library(ggmap)

setwd("C:\\Users\\Zachary\\Desktop\\ADS Project")

ACS_pus_3 = subset(ACS_pus_2, ACS_pus_2$POWSP <= 56 & ACS_pus_2$POWSP != 2 & ACS_pus_2$POWSP != 15)

State_ACS_name = read.csv("ACS_Name.csv",header=TRUE)
ACS_pus_4 = merge(ACS_pus_3, State_ACS_name, by="POWSP")

ACS_pus_5 = read.csv("ACS_pus_new_collapse.csv", header=T)
ACS_pus_6 = merge(ACS_pus_5, State_ACS_name, by="POWSP")
all_states = map_data("state")

states_plot =  ggplot() + geom_polygon(data=all_states,
                                       aes(x=long,y=lat,group=group),
                                       colour="white",fill="grey10")

states_plot = states_plot + geom_pointrange(data=ACS_pus_6,aes(x=long,y=lat,size=marriage_prop_percent,color=marriage_prop_percent),
                                            ymin=5,ymax=25)


states_plot = states_plot + scale_size(name="Percentage Married") 

#tates_plot = states_plot + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
#+ scale_size_area(max_size=12)


#states_plot = states_plot + geom_text(data=ACS_pus_6, hjust=0.5,vjust=-0.5,
#                                    aes(x=long,y=lat,label=" "), color="gold2",size=4)
