er <- read.table("employregion.txt", header=TRUE)
table(er$Region)

er.manova1<-manova(cbind(SER,FIN,SPS,TC) ~ factor(Region), data=er)
summary(er.manova1) #default test is Pillai's

summary(er.manova1, test="Wilks")

summary(er.manova1, test="Roy")

summary(er.manova1, test="Hotelling-Lawley")


selected_data <- er[, c("Region", "SER", "FIN", "SPS", "TC")]
Y<-cbind(selected_data$SER, selected_data$FIN,selected_data$SPS, selected_data$TC) 
library(DescTools)
(HotellingsT2Test(Y ~ as.factor(Region), data=selected_data,subset=as.factor(Region)%in% c("EU", "EFTA")))
(HotellingsT2Test(Y ~ as.factor(Region), data=selected_data,subset=as.factor(Region)%in% c("EU", "Eastern")))
(HotellingsT2Test(Y ~ as.factor(Region), data=selected_data,subset=as.factor(Region)%in% c("EU", "Other")))
(HotellingsT2Test(Y ~ as.factor(Region), data=selected_data,subset=as.factor(Region)%in% c("EFTA", "Eastern")))
(HotellingsT2Test(Y ~ as.factor(Region), data=selected_data,subset=as.factor(Region)%in% c("EFTA", "Other")))
(HotellingsT2Test(Y ~ as.factor(Region), data=selected_data,subset=as.factor(Region)%in% c("Eastern", "Other")))