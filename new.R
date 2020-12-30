dat <- read.csv(file = 'chest.atact.csv', header = TRUE)

#####description  of  data

Variable          Description                     Codes / Values
id                Identification Code             1 - 500
age               Age at Hospital Admission       Years
gender            Gender                          0 = Male, 1 = Female
hr                Initial Heart Rate              Beats per minute
sysbp             Initial Systolic Blood          mmHg
Pressure
diasbp            Initial Diastolic Blood         mmHg
Pressure
bmi               Body Mass Index                 kg/m^2
cvd               History of Cardiovascular       0 = No, 1 = Yes
Disease
afb               Atrial Fibrillation             0 = No, 1 = Yes
sho               Cardiogenic Shock               0 = No, 1 = Yes
chf               Congestive Heart                0 = No, 1 = Yes
Complications
av3               Complete Heart Block            0 = No, 1 = Yes
miord             MI Order                        0 = First, 1 = Recurrent
mitype            MI Type                         0 = non Q-wave, 1 = Q-wave
year              Cohort Year                     1 = 1997, 2 = 1999, 3 = 2001
admitdate         Hospital Admission Date         mm/dd/yyyy
disdate           Hospital Discharge Date         mm/dd/yyyy
fdate             Date of last Follow Up          mm/dd/yyyy
los               Length of Hospital Stay         Days between Hospital
Discharge and Hospital
Admission
dstat             Discharge Status from           0 = Alive, 1 = Dead
Hospital
lenfol            Total Length of Follow-up       Days between Date of Last
Follow-up and Hospital
Admission Date
fstat             Vital Status at Last            0 = Alive 1 = Dead
Follow-up

#Remove unessary variables.#

which( colnames(dat)=="year" )
df <- dat[,-15:-18] 
which( colnames(df)=="lenfol" )
df <-df[,-17:-18]

# missinng value treatment#

sum(is.na(df))

##no missing value###

## scaleing  the attributes##

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dfNorm <- as.data.frame(lapply(df, normalize))

## outlier treatment##multivariable approach##

mod <- lm( dstat~ ., data=dfNorm)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")

##plot cook's distance##

abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
View(influential)
car::outlierTest(mod)

##No Studentized residuals with Bonferonni p < 0.05 so the outliers do not effect the model as such

### inpportaant  pradictors ###

mcor<-round(cor(dfNorm[,-1]),2)
mcor
upper<-mcor
upper[upper.tri(mcor)]<-""

upper<-as.data.frame(upper)
upper

##inportant  variables from the matrix is#


age               Age at Hospital Admission       Years
           
sysbp             Initial Systolic Blood          mmHg


sho               Cardiogenic Shock               0 = No, 1 = Yes
chf               Congestive Heart                0 = No, 1 = Yes

dstat             Discharge Status from           0 = Alive, 1 = Dead


####test train split####

install.packages("caTools")
library(caTools)

set.seed(0)
split = sample.split(dfNorm,SplitRatio = 0.5)
training_set = subset(dfNorm,split == TRUE)
test_set = subset(dfNorm, split == FALSE)

####model  fit#####

#the model is biaanary logistics rregression#


logit_2 <- glm(dstat~age+sysbp+sho,gaussian(link = "identity"),data = training_set)
summary(logit_2)



 ## fit the model training data to measre bais

train_a = predict(logit_2,training_set)
head(train_a)


##ROC###  

library(pROC)
roc.info<-roc(training_set$dstat,train_a, plot=TRUE)

roc.info <- roc(training_set$dstat,train_a, legacy.axes=TRUE)
str(roc.info)
roc.df <- data.frame(
  tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.info$thresholds)
head(roc.df)
tail(roc.df)
###AOC##
roc(training_set$dstat,train_a, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)



## fit the model test data to measre varience

test_a = predict(logit_2,test_set)
t1<- as.data.frame(test_a)


##ROC###

library(pROC)
roc(test_set$dstat,test_a, plot=TRUE)
 
roc.info1 <- roc(test_set$dstat,test_a, legacy.axes=TRUE)
str(roc.info)
roc.df1 <- data.frame(
  tpp=roc.info1$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info1$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.info1$thresholds)

##AOC##

roc(test_set$dstat,test_a, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

## bais and varience is low and they are  more or  less equal
##so the  model is good .


















