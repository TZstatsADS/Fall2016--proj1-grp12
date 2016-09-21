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

cen_data = read.dta("ACS Pop_Total12.dta")
dim(cen_data)

#subsetting data 
cen_data_1 = select(cen_data,c(SERIALNO,SSMC,HINCP,FINCP,HHL,MULTG,
                               NOC,NPF,NR,HUGCL,WGTP))
table(cen_data_1$SSMC)
cen_data_1$SSMC[cen_data_1$SSMC %in% "2"] = "1"
table(cen_data_1$SSMC)

med_inc_SSMC = median(cen_data$HINCP,by=cendata$SSMC)

####################
###visuzaliation#############

ggplot(cen_data, aes(x=SSMC)) + geom_bar() 

###############################################
######### personal data #####################3
ACS_pus_2 = read.csv("ACS_pus_2.csv")
dim(ACS_pus_2)

ACS_pus_2$MAR_New[ACS_pus_2$MAR == 1] = 1
ACS_pus_2$MAR_New[ACS_pus_2$MAR != 1] = 0

table(ACS_pus_2$RAC1P)

Loe_Lev = c(0,500,800,1005,1300,1600,2000,2100,2200,2600,3000,3600,3700,
            4000,4200,4300,4700,5000,6005,6200,6800,7000,7700,9000,9800,9900)
loe_lev_name = c("MGR","BUS", "FIN", "CMM", "ENG", "SCI", "CMS", "LGL", "EDU", 
                 "ENT", "MED","HLS","PRT","EAT","CLN","PRS","SAL","OFF","FFF",
                 "CON","EXT","RPR","PRD","TRN","MIL","UNEMPLOYED ")
for (i in 1:26){
  ACS_pus_2$OCCP_New[ACS_pus_2$OCCP >= Loe_Lev[i] ] = i
  ACS_pus_2$OCCUPENCY[ACS_pus_2$OCCP >= Loe_Lev[i]] = loe_lev_name[i]
}

ACS_pus_2$OCCP_New = as.factor(ACS_pus_2$OCCP_New)
ACS_pus_2$WKW = as.factor(ACS_pus_2$WKW)
ACS_pus_2$SCHL = as.factor(ACS_pus_2$SCHL)
ACS_pus_2$RAC1P = as.factor(ACS_pus_2$RAC1P)

#ACS_pus_2$Count = vector(length = 1638593)
Marriage.Status = as.factor(ACS_pus_2$MAR_New)

ggplot(ACS_pus_2, aes(OCCUPENCY), main = "Plot of Marriage VS Occupation", xlab = "OCCUPENCY", ylab = "Count of Marriage Status") + 
  geom_bar(aes(fill = Marriage.Status),position = "dodge")

###########################################
########## visuzliation ############

df.new = ddply(ACS_pus_2,.(Marriage.Status),summarise,
               prop=prop.table(table(RAC1P)),
               RAC1P=names(table(RAC1P)))


ggplot(ACS_pus_2, aes(Marriage.Status,..count..), main = "Plot of Marriage VS Occupation", xlab = "OCCUPENCY", ylab = "Count of Marriage Status") + 
  geom_bar(aes(,fill = RAC1P), position = "dodge")


##########################################
######### MAP CREATION ##################
setwd("C:\\Users\\Zachary\\Desktop\\ADS Project")
ACS_pus_2 = read.csv("ACS_pus_2.csv")
#loading US map
all_states = map_data("state")
#plotting ggplot


head(all_states)
head(ACS_pus_2)
head(ACS_pus_2$POWSP)

ACS_pus_3 = subset(ACS_pus_2, ACS_pus_2$POWSP <= 56 & ACS_pus_2$POWSP != 2 & ACS_pus_2$POWSP != 15)

#reading lat and long data excel into R
State_ACS_name = read.csv("ACS_Name.csv",header=TRUE)
ACS_pus_4 = merge(ACS_pus_3, State_ACS_name, by="POWSP")

write.dta(ACS_pus_4, "ACS_pus_4.dta") 

##################################################################################################
################redoing
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




