library(ggplot2)
library(boot)
############################# Modeling ########################
ACS_pusa<-read.csv(file.choose(),header=T)
ACS_pusb<-read.csv(file.choose(),header=T)

names(ACS_pusa)
head(ACS_pusa)
dim(ACS_pusa)
names(ACS_pusb)
head(ACS_pusb)
dim(ACS_pusb)

ACS_pus <- rbind(ACS_pusa, ACS_pusb)
NACS_pus <- ACS_pus[order(ACS_pus$SERIALNO),] 

ACS_pus_1 = NACS_pus[ ,c("SERIALNO","PWGTP","MAR","MARHD","MARHD","MARHM","MARHT","MARHW","MARHYP","OCCP"
                         ,"WKW","POWSP","PINCP","SCHL","FOD1P","AGEP","RAC1P","RAC2P","POBP",
                         "FMARP","FMARHDP","FMARHMP","FMARHTP","FMARHWP","FMARHYP","FESRP",
                         "FOCCP","FWKWP","FPOWSP","FPINCP","FSCHLP","FAGEP","FPOBP")]
dim(ACS_pus_1)
ACS_pus_2 <- subset(ACS_pus_1, AGEP >= 21 & AGEP <= 30 & MAR != 2)
dim(ACS_pus_2)
ACS_pus_2[1:50,]


ACS_pus_2$MAR_New[ACS_pus_2$MAR == 1] = 1
ACS_pus_2$MAR_New[ACS_pus_2$MAR != 1] = 0
write.csv(ACS_pus_2,file = "D:/ACS_pus_2.csv")
ACS_pus_2 <- read.csv(file = "D:/ACS_pus_2.csv",header=T)

## convert into category Variables, classify age 
Loe_Lev = c(0,500,800,1005,1300,1600,2000,2100,2200,2600,3000,3600,3700,
            4000,4200,4300,4700,5000,6005,6200,6800,7000,7700,9000,9800,9900)
loe_lev_name = c("MGR","BUS", "FIN", "CMM", "ENG", "SCI", "CMS", "LGL", "EDU", 
                 "ENT", "MED","HLS","PRT","EAT","CLN","PRS","SAL","OFF","FFF",
                 "CON","EXT","RPR","PRD","TRN","MIL","UNEMP")
for (i in 1:26){
  ACS_pus_2$OCCP_New[ACS_pus_2$OCCP >= Loe_Lev[i] ] = i
  ACS_pus_2$Occupation[ACS_pus_2$OCCP >= Loe_Lev[i]] = loe_lev_name[i]
}
## include NA 
# ACS_pus_2$OCCP_New[is.na(ACS_pus_2$OCCP)] = 0
# ACS_pus_2$OCCUPENCY[is.na(ACS_pus_2$OCCP)] = "N/A"
# ACS_pus_2$OCCP[1:1000]
# ACS_pus_2$OCCP_New[1:1000]
# ACS_pus_2$OCCUPENCY[1:1000]
summary(ACS_pus_2$OCCP)
summary(ACS_pus_2$Occupation)


## classify education 
ACS_pus_2$SCHL = as.numeric(ACS_pus_2$SCHL)
ACS_pus_2$SCHL_code[ACS_pus_2$SCHL < 17 ] = "1.Less than K12"
ACS_pus_2$SCHL_code[ACS_pus_2$SCHL > 16 & ACS_pus_2$SCHL <21] = "2.K12"
ACS_pus_2$SCHL_code[ACS_pus_2$SCHL == 21] = "3.Bachelor's"
ACS_pus_2$SCHL_code[ACS_pus_2$SCHL == 22 | ACS_pus_2$SCHL == 23] = "4.Master's"
ACS_pus_2$SCHL_code[ACS_pus_2$SCHL == 24] = "5.Doctor's"
summary(ACS_pus_2$SCHL_code)
ACS_pus_2$SCHL_code[1:500]

## Code other independent variables from number to words
# WKW
ACS_pus_2$WKW[1:500]
WKW_name = c("50-52 Weeks","48-49 Weeks", "40-47 Weeks", "27-39 Weeks", "14-26 Weeks", "13 and less")
for (i in 1:6){
  ACS_pus_2$Length.of.Work[ACS_pus_2$WKW == i] = WKW_name[i]
}
ACS_pus_2$Length.of.Work[1:500]

# RAC1P
ACS_pus_2$RAC1P[1:500]
race_name = c("White","African", "American Indian", "Alaska Native","not specified","Asian", 
              "Pacific", "Some Other Race","Two or More Races ")
for (i in 1:9){
  ACS_pus_2$Race[ACS_pus_2$RAC1P == i] = race_name[i]
}
ACS_pus_2$Race[1:500]      

########## Historgram for independent variables############ 
# variables: Occupation + Length.of.Work + Personal Income + Education + Age + Race 
ggplot(ACS_pus_2, aes(Occupation, ..count..,weight=Weight)) + 
  geom_bar(aes(fill = ..count..))+
  xlab("Occupation")+
  ylab("Percentage")
# Length of work
ggplot(ACS_pus_2, aes(Length.of.Work, ..count..,weight=Weight)) + 
  geom_bar(aes(fill = ..count..))+
  xlab("Length of Work")+
  ylab("Percentage")
# Education
ggplot(ACS_pus_2, aes(SCHL_code, ..count..,weight=Weight)) + 
  geom_bar(aes(fill = ..count..))+
  xlab("Education")+
  ylab("Percentage")
# Age
ggplot(ACS_pus_2, aes(AGEP, ..count..,weight=Weight)) + 
  geom_bar(aes(fill = ..count..))+
  xlab("Age")+
  ylab("Percentage")
#Race
ggplot(ACS_pus_2, aes(Race, ..count..,weight=Weight)) + 
  geom_bar(aes(fill = ..count..))+
  xlab("Race")+
  ylab("Percentage")

# Personal Income
ggplot((ACS_pus_2), aes(PINCP, ..count..,weight=Weight))+
  geom_histogram(aes(fill = ..count..))+
  scale_x_sqrt() +
  xlab("Persobal Income")+
  ylab("Density")



################# Visualization ################
Marriage.Status = as.factor(ACS_pus_2$MAR_New)
ggplot(ACS_pus_2, aes(Occupation,fill = Marriage.Status,weight=Weight)) + 
  geom_bar(position = "fill") +
  xlab("Occupation") +
  ylab("Percentage of Marriage Status") + 
  scale_fill_manual("Marriage Status",values = alpha( c("firebrick", "dodgerblue4"), 1))  

ggplot(ACS_pus_2, aes(Length.of.Work,fill = Marriage.Status,weight=Weight)) + 
  geom_bar(position = "fill")+
  xlab("Length of Work") +
  ylab("Percentage of Marriage Status") + 
  scale_fill_manual("Marriage Status",values = alpha( c("firebrick", "dodgerblue4"), 1))  

ggplot(ACS_pus_2, aes(SCHL_code,fill = Marriage.Status,weight=Weight)) + 
  geom_bar(position = "fill")+
  xlab("Education") +
  ylab("Percentage of Marriage Status") + 
  scale_fill_manual("Marriage Status",values = alpha( c("firebrick", "dodgerblue4"), 1))  

ggplot(ACS_pus_2, aes(Race,fill = Marriage.Status,weight=Weight)) + 
  geom_bar(position = "fill")+
  xlab("Race") +
  ylab("Percentage of Marriage Status") + 
  scale_fill_manual("Marriage Status",values = alpha( c("firebrick", "dodgerblue4"), 1))  

ggplot(ACS_pus_2, aes(AGEP,fill = Marriage.Status)) + 
  geom_bar(position = "fill")+
  xlab("Age") +
  ylab("Percentage of Marriage Status") + 
  scale_fill_manual("Marriage Status",values = alpha( c("firebrick", "dodgerblue4"), 1))  

ggplot(ACS_pus_2, aes(PINCP,fill = Marriage.Status,weight=Weight)) + 
  geom_histogram(position = "fill",binwidth = 130000)+
  xlab("Personal Income") +
  ylab("Percentage of Marriage Status") + 
  scale_fill_manual("Marriage Status",values = alpha( c("firebrick", "dodgerblue4"), 1))  

ggplot(ACS_pus_2, aes(Race,fill = Marriage.Status,weight=Weight)) + 
  geom_bar(position = "fill")+
  xlab("Race") +
  ylab("Percentage of Marriage Status") + 
  scale_fill_manual("Marriage Status",values = alpha( c("firebrick", "dodgerblue4"), 1))  +
  # third dimension eg:education:
  facet_grid ( . ~ SCHL_code ) +
  # rotate x axis, eg:angle = 45 or 90 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(ACS_pus_2, aes(AGEP,fill = Marriage.Status,weight=Weight)) + 
  geom_bar(position = "fill")+
  xlab("Race") +
  ylab("Percentage of Marriage Status") + 
  scale_fill_manual("Marriage Status",values = alpha( c("firebrick", "dodgerblue4"), 1))  +
  # third dimension eg:education:
  facet_grid ( . ~ SCHL_code ) +
  # rotate x axis, eg:angle = 45 or 90 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############## logistic regression############### 
ACS_pus_2$Weight = ACS_pus_2$PWGTP/sum(ACS_pus_2$PWGTP)
class_model = glm(MAR_New ~ Occupation+Length.of.Work+PINCP+SCHL_code+AGEP+Race, data = na.omit(ACS_pus_2),
                  family = "binomial",weights = Weight)
head(fitted.values(class_model))
summary(class_model)
# training error
error = abs(round(fitted.values(class_model))-na.omit(ACS_pus_2)$MAR_New)
sum(error)      

## CV Logistic Regression 
glmcv = cv.glm(na.omit(ACS_pus_2), class_model, K=5)
glmcv$delta

########logistic regression without occupation#############
class_model2 = glm(MAR_New ~ Length.of.Work+PINCP+SCHL_code+AGEP+Race, data = na.omit(ACS_pus_2),
                   family = "binomial",weights = Weight)
head(fitted.values(class_model2))
summary(class_model2)
# training error
error2 = abs(round(fitted.values(class_model2))-na.omit(ACS_pus_2)$MAR_New)
sum(error2)




