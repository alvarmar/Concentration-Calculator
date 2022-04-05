##Script for calculating concentrations from peak areas 
library(dplyr)

##Step 1: Set working directory

##Step 2: Import a csv datafile. Specify the name of the files. Note there is two files, one containing standard curve peak areas and other containing samples peak areas.

stddata <- read.csv("standardpeakareas_220125.csv", header=TRUE)
sampledata <- read.csv("speextractionpeakareas_220125.csv", header=TRUE)

##Step 3: Create linear regression model for each meta and predict concentrations in ptt


metals<- colnames(stddata)[4:10]
results  <- lapply(metals, function(x) {
fit <- lm(substitute(concentration ~ i, list(i = as.name(x))), data = stddata)
#print(summary(fit))
print(predict(fit, newdata= sampledata))
})
predicted <- data.frame(results)

##Step 4: Renaming columns names and  converting to uM
names(predicted) <- c('pptFe','pptCo','pptNi','pptCu','pptZn','pptCd','pptPb')
sampledatanew <- cbind(sampledata,predicted)
sampledatappt<-sampledatanew %>% select(-starts_with('pa'))
uMconcentrations <- sampledatappt[, c("date", "column","sample")]
uMFe <- sampledatappt$pptFe*1.79E-8
uMCo <- sampledatappt$pptCo*1.70E-8
uMNi <- sampledatappt$pptNi*1.70E-8
uMCu <- sampledatappt$pptCu*1.57E-8
uMZn <- sampledatappt$pptZn*1.53E-8
uMCd <- sampledatappt$pptCd*8.90E-9
uMPb <- sampledatappt$pptPb*4.83-9
uMconcentrations <- cbind(uMconcentrations,uMFe, uMCo, uMNi, uMCu, uMZn, uMCd, uMPb)

##Step 5: Safe data as csv
write.csv(x=uMconcentrations, file="ConcentrationsuM.csv")





##------------------------------------------------------------------------------
##These lines can create linear regression model and predictions for each columns individually 
##modelFe <- lm(formula = concentration ~ pa_56Fe, data = stddata)
##modelCo <- lm(formula = concentration ~ pa_59Co, data = stddata)
##modelNi <- lm(formula = concentration ~ pa_60Ni, data = stddata)
##modelCu <- lm(formula = concentration ~ pa_63Cu, data = stddata)
##modelZn <- lm(formula = concentration ~ pa_66Zn, data = stddata)
##modelCd <- lm(formula = concentration ~ pa_111Cd, data = stddata)
##modelPb <- lm(formula = concentration ~ pa_208Pb, data = stddata)

## Make predictions using linear regression model
#pptFe<- predict(modelFe, newdata = sampledata['pa_56Fe'])
#pptCo <- predict(modelCo, newdata = sampledata['pa_59Co'])
#pptNi <- predict(modelNi, newdata = sampledata['pa_60Ni'])
#pptCu <- predict(modelCu, newdata = sampledata['pa_63Cu'])
#pptZn <- predict(modelZn, newdata = sampledata['pa_66Zn'])
#pptCd <- predict(modelCd, newdata = sampledata['pa_111Cd'])
#pptPb <- predict(modelPb, newdata = sampledata['pa_208Pb'])

