er <- read.table("employregion.txt", header=TRUE)
selected_data <- er[, c("Region", "SER", "FIN", "SPS", "TC")]

Y<-cbind(selected_data$SER, selected_data$FIN,selected_data$SPS, selected_data$TC)
(cory<-round(cor(Y), digits=3))

library(DescTools)
(HotellingsT2Test(Y ~ as.factor(Region), data=selected_data,subset=as.factor(Region)%in% c("EFTA", "Other")))
