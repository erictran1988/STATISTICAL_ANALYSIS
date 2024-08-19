er <- read.table("employregion.txt", header=TRUE)
employmentregion <- er[, 2:9]
str(employmentregion)

#univariate Shapiro-Wilks tests
library(MVN)
result1<-mvn(employmentregion, mvnTest = "mardia", univariateTest="SW")
names(result1)
result1$univariateNormality

#MVN
employmentregion1<-employmentregion[,5:6]##SER and FIN
par(mfrow = c(1,2))
persp1<-mvn(employmentregion1, mvnTest="mardia", desc=FALSE,
            univariateTest="SW", multivariatePlot="persp")
contour1<-mvn(employmentregion1, mvnTest="mardia", desc=FALSE,
              univariateTest="SW", multivariatePlot="contour")

mardia<-mvn(employmentregion, mvnTest="mardia", univariateTest="SW",
            desc=FALSE)
mardia$multivariateNormality

HZ<-mvn(employmentregion, mvnTest="hz", univariateTest="SW",
        desc=FALSE)
HZ$multivariateNormality

royston<-mvn(employmentregion, mvnTest="royston", univariateTest="SW",
             desc=FALSE)
royston$multivariateNormality
QQplot<-mvn(employmentregion, multivariatePlot="qq")

uvn_variables <- er[, 6:9]
str(uvn_variables)
mardia2<-mvn(uvn_variables, mvnTest="mardia", univariateTest="SW",
            desc=FALSE)
mardia2$multivariateNormality
QQplot<-mvn(uvn_variables, multivariatePlot="qq")