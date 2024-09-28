## Replace localPath with the folder where you have saved the data
setwd("localPath")

rm(list=ls())

data<-read.csv("Mitigation with gravity vars amended FULL Feb 14 2023.csv")


data$NDC15[data$NDC15%in%1 & !data$Year%in%2015]<-0
data$NDC15[data$NDC16%in%1 & !data$Year%in%2016]<-0
data$NDC15[data$NDC17%in%1 & !data$Year%in%2017]<-0
data$NDC15[data$NDC18%in%1 & !data$Year%in%2018]<-0

## In data the amount of Adapt and Mitigation are defined as NA
## after constructing the dataset, change the adaptation and mitigation to zeros for NA cells

data[is.na(data[,5]),5]<-0
data[is.na(data[,6]),6]<-0
data[is.na(data[,7]),7]<-0
data[is.na(data[,8]),8]<-0

## remove deals less than 100 USD
## 35 obs.for adaptation and 29 for mitigation

data[data[,5]>0&data[,5]<0.1,5]<-0
data[data[,7]>0&data[,7]<0.1,7]<-0

## assign NA International Agreement as zero
data$InvestAgree[is.na(data$InvestAgree)]<-0

regData1<-data[,c(1:4,5,7,9:18,21:40)]

regData1$ProviderGDPCur<-log(regData1$ProviderGDPCur)
regData1$RecipientGDPCur<-log(regData1$RecipientGDPCur)
regData1$ProviderPop<-regData1$ProviderPop/10^9
regData1$RecipientPop<-regData1$RecipientPop/10^9

regData1$ProviderfisB<-regData1$ProviderfisB/100
regData1$RecipientfisB<-regData1$RecipientfisB/100

regData1$Providerdebt<-regData1$Providerdebt/100
regData1$Recipientdebt<-regData1$Recipientdebt/100

regData1$distw<-log(regData1$distw)

## change the NA for MDB to zero
regData1$MDBDummy[is.na(regData1$MDBDummy)]<-0

regDataMiti<-regData1[complete.cases(regData1),]

names(regDataMiti)[5]<-"AdaptAmount"
names(regDataMiti)[6]<-"MitiAmount"

## remove the zero provider countries
regDataMiti$ProviderISO<-as.character(regDataMiti$ProviderISO)


library(fastDummies)

regDataMitiD<-dummy_cols(regDataMiti, select_columns = c("Sector","RecipientISO","ProviderISO","Year"), remove_first_dummy = FALSE)

## column 42 to 110 are recipient
## 69 countries 
names(regDataMitiD)[44:111]


reg1<-regDataMitiD[,c(5:149)]

reg1$MitiAmount[reg1$MitiAmount!=0]<-log(reg1$MitiAmount[reg1$MitiAmount!=0]*1000)
reg1$AdaptAmount[reg1$AdaptAmount!=0]<-log(reg1$AdaptAmount[reg1$AdaptAmount!=0]*1000)

reg1$MDBAdapt[reg1$MDBAdapt!=0]<-log(reg1$MDBAdapt[reg1$MDBAdapt!=0]*1000)
reg1$MDBMiti[reg1$MDBMiti!=0]<-log(reg1$MDBMiti[reg1$MDBMiti!=0]*1000)
## choose the benchmark sectors mannually
## drop these variables in the regression

reg1$NDCGHG<-0
reg1$Sector_Others<-0
reg1$Year_2016<-0
reg1$ProviderISO_USA<-0
## assign zero to countries with observations <0.005
reg1$RecipientISO_ARM<-0
reg1$IncomeGroup<-as.character(reg1$IncomeGroup)
reg1$IncomeGroup[reg1$IncomeGroup=="UMICs"]<-"BaseUM"

names(reg1)[33:39]<-c("Water","Transport","Energy","Agri","EnvProtect", "MultiSec","Others")


library(mhurdle)
library(texreg)

## hurdle 1

reg1$WRI<-reg1$RecipientWRIExpo*reg1$RecipientWRIVul
reg1$ProviderGDPtot<-log(exp(reg1$ProviderGDPCur)*reg1$ProviderPop)
reg1$RecipientGDPtot<-log(exp(reg1$RecipientGDPCur)*reg1$RecipientPop)

names(reg1)

Stn <- mhurdle(MitiAmount ~ WRI+CPIAPublicAdm+CPIAbudget+
                 NDCActOnly+NDCnonGHG+NDC15+NDC16+NDC17+NDC18+
                 distw+colony+comlang+comrelig+wto+MDBDummy+EIA+InvestAgree+
                 ProviderGDPtot+RecipientGDPtot+ProviderPop+RecipientPop+
                 ProviderISO_ARE+ProviderISO_AUS+ProviderISO_AUT+ProviderISO_BEL+ProviderISO_CAN+
                 ProviderISO_CHE+ProviderISO_CZE+ProviderISO_DEU+ProviderISO_DNK+ProviderISO_ESP+
                 ProviderISO_FIN+ProviderISO_FRA+ProviderISO_GBR+ProviderISO_GRC+ProviderISO_IRL+
                 ProviderISO_ISL+ProviderISO_ITA+ProviderISO_JPN+ProviderISO_KOR+
                 ProviderISO_LUX+ProviderISO_NLD+ProviderISO_NOR+ProviderISO_NZL+
                 ProviderISO_POL+ProviderISO_PRT+ProviderISO_SVN+ProviderISO_SWE
               | 
                 WRI+CPIAPublicAdm+CPIAbudget+
                 distw+colony+comlang+comrelig+wto+MDBDummy+EIA+InvestAgree+
                 ProviderGDPtot+RecipientGDPtot+ProviderPop+RecipientPop+
                 ProviderfisB+RecipientfisB+Providerdebt+Recipientdebt+
                 RecipientISO_AFG+RecipientISO_AGO+RecipientISO_BDI+RecipientISO_BEN+
                 RecipientISO_BFA+RecipientISO_BGD+RecipientISO_BIH+RecipientISO_BTN+RecipientISO_CAF+
                 RecipientISO_CIV+RecipientISO_CMR+
                 #RecipientISO_COG+
                 RecipientISO_COM+RecipientISO_CPV+
                 RecipientISO_DJI+RecipientISO_ERI+RecipientISO_ETH+RecipientISO_GEO+RecipientISO_GHA+
                 RecipientISO_GIN+RecipientISO_GMB+RecipientISO_GNB+RecipientISO_GRD+RecipientISO_GUY+
                 RecipientISO_HND+RecipientISO_HTI+RecipientISO_IND+RecipientISO_KEN+RecipientISO_KGZ+
                 RecipientISO_KHM+RecipientISO_KIR+RecipientISO_LAO+RecipientISO_LBR+RecipientISO_LKA+
                 RecipientISO_LSO+RecipientISO_MDA+RecipientISO_MDG+RecipientISO_MLI+RecipientISO_MMR+
                 RecipientISO_MNG+RecipientISO_MOZ+RecipientISO_MRT+RecipientISO_MWI+RecipientISO_NER+
                 RecipientISO_NGA+RecipientISO_NIC+RecipientISO_NPL+RecipientISO_PAK+RecipientISO_PNG+
                 RecipientISO_RWA+RecipientISO_SDN+RecipientISO_SEN+RecipientISO_SLB+RecipientISO_SLE+
                 RecipientISO_STP+RecipientISO_TCD+RecipientISO_TGO+RecipientISO_TJK+RecipientISO_TON+
                 RecipientISO_TZA+RecipientISO_UGA+RecipientISO_UZB+RecipientISO_VNM+RecipientISO_VUT+
                 RecipientISO_WSM+RecipientISO_YEM+RecipientISO_ZMB+RecipientISO_ZWE+
                 ProviderISO_ARE+ProviderISO_AUS+ProviderISO_AUT+ProviderISO_BEL+ProviderISO_CAN+
                 ProviderISO_CHE+ProviderISO_CZE+ProviderISO_DEU+ProviderISO_DNK+ProviderISO_ESP+
                 ProviderISO_FIN+ProviderISO_FRA+ProviderISO_GBR+ProviderISO_GRC+ProviderISO_IRL+
                 ProviderISO_ISL+ProviderISO_ITA+ProviderISO_JPN+ProviderISO_KOR+ProviderISO_LTU+
                 ProviderISO_LUX+ProviderISO_LVA+ProviderISO_NLD+ProviderISO_NOR+ProviderISO_NZL+
                 ProviderISO_POL+ProviderISO_PRT+ProviderISO_SVN+ProviderISO_SWE+
                 Year_2011+Year_2012+Year_2013+Year_2014+Year_2015+Year_2017+Year_2018+
                 Water+Transport+Agri+EnvProtect+MultiSec+Energy,
               reg1,
               dist = "n", h2 = TRUE, corr = FALSE, method = "Bhhh", print.level = 0,finalHessian = TRUE)
summary(Stn)




Slnd <- update(Stn, corr = TRUE)
coef(summary(Slnd), "corr")


result1<-texreg(list(Stn, Slnd),
                custom.model.names = c("log-normal", "Correlated log-normal"),
                caption = "Estimation of double hurdle selection models",
                label = "tab:sep", pos = "ht", digits =3)
result1

write.table(result1, "Baseline Result for  Mitigation 103950 obs")
