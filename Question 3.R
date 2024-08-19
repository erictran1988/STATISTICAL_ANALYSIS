er <- read.table("employregion.txt", header=TRUE)
data <- er[, c("SER", "FIN", "SPS", "TC")]
str(data)
head(data)
(cor<-round(cor(data), digits =3))

(er.prcomp <- prcomp(er[,6:9], center=TRUE, scale=TRUE ))
(eigen<-(er.prcomp$sdev)^2)

pervar<-((er.prcomp$sdev^2/sum(er.prcomp$sdev^2))*100)
(cumsum<-cumsum(pervar)) #use 2 components

screeplot(er.prcomp, type="lines")
library(psych)
RNGkind(sample.kind = "Rounding") 
set.seed(245)
pa<-fa.parallel(er[,6:9], fm="ml", fa="pc", n.iter=5000)

pa.out<-pa$values
quants <- c(0.95)
pa$pc.values

(pa_95quant<-apply( pa.out[,5:8], 2 , quantile , probs = quants ))

(sumeigen<-sum(eigen))
