---
title: "ECLS-K-2011"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Here we are setting the WD to google drive.  See earlier versions for getting the original data source.  Need to get all data for every varibable over time.  If a variable is missing for a time point, then create the next variable that should be there and just make it filled with NAs.  Then transform into long format.  Just copy and paste everything three times (get rid of fall, because you do not know which school they are in until the spring), but make sure that you creating NA's for variables that do not exist.  

X1TCHAPP = teacher report approaches to learning
X1TCHCON = teacher report self control
X1TCHPER = teacher report interperonsal
X1TCHEXT = X1 TEACHER REPORT EXTERN PROB BEHAVIORS
X1TCHINT = X1 TEACHER REPORT INTERN PROB BEHAVIORS



```{r}
#setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data")
#data = read.csv("ELCS-K-2011.csv", header = TRUE)

# Now collecting variables for all teacher self report
data1 = cbind(id = 1:length(data1$X1TCHAPP), X1TCHAPP = data$X1TCHAPP, X2TCHAPP = data$X2TCHAPP, X3TCHAPP = data$X3TCHAPP, X4TCHAPP = data$X4TCHAPP, X1TCHCON = data$X1TCHCON, X2TCHCON = data$X2TCHCON, X3TCHCON = data$X3TCHCON, X4TCHCON = data$X4TCHCON,X1TCHPER = data$X1TCHPER, X2TCHPER = data$X2TCHPER, X3TCHPER = data$X3TCHPER, X4TCHPER = data$X4TCHPER, X1TCHEXT = data$X1TCHEXT, X2TCHEXT = data$X2TCHEXT, X3TCHEXT = data$X3TCHEXT, X4TCHEXT = data$X4TCHEXT, X1TCHINT = data$X1TCHINT, X2TCHINT = data$X2TCHINT, X3TCHINT = data$X3TCHINT, X4TCHINT = data$X4TCHINT,X1RTHET = data$X1RTHET, X2RTHET = data$X2RTHET, X3RTHET = data$X3RTHET, X4RTHET = data$X4RTHET, X1MTHET = data$X1MTHET, X2MTHET = data$X2MTHET, X3MTHET = data$X3MTHET, X4MTHET = data$X4MTHET, X1BMI = data$X1BMI, X2BMI = data$X2BMI, X3BMI = data$X3BMI, X4BMI = data$X4BMI, X1PUBPRI = data$X1PUBPRI, X2PUBPRI = data$X2PUBPRI, X3PUBPRI = data$X3PUBPRI, X4PUBPRI = data$X4PUBPRI)
head(data1)
# Change the -9 to NAs
data1 = apply(data1, 2, function(x){ifelse(x == -9, NA, x)})
data1 = as.data.frame(data1)
head(data1)
dim(data1)
```
Change X1PUBPRI to everything 1 = 0 all else 0, making the treatment group nonpublic schools.  Then get rid of the old public and private, because you only want the new values that only have 1's and 0's 
```{r}
XPUBPRI = cbind(X1PUBPRI = data$X1PUBPRI, X2PUBPRI = data$X2PUBPRI, X3PUBPRI = data$X3PUBPRI, X4PUBPRI = data$X4PUBPRI)

XPUBPRI = as.data.frame(apply(XPUBPRI, 2, function(x){ifelse(x == 1, 0, 1)}))
head(XPUBPRI)
data1 = data1[-c(34:38)]
data1 = cbind(data1, XPUBPRI); head(data1)
```
Here we will use Amelia.  Need to set m as five for five imputed data sets.  Then we place each of the variables into their appropriate categories.

```{r}
library(Amelia)
library(mitools)
library(survey)
m = 5
a.out = amelia(x = data1, m=m, noms = "X1PUBPRI")
# Now we can creat seperate data set and then analyze them seperately and combine them later with the mi.meld function in Ameila
#summary(a.out)
#compare.density(a.out, var = "X1TCHCON", main = "Observed and Imputed values of Self Control")
#disperse(a.out, dims = 1, m = 5)
write.amelia(obj = a.out, file.stem = "ECLSK")
```
Read all five data sets back in first.  

Name them the same as below.    

Grab the firsts of every variable.  These five will be used for matching the variables.

Now you need to merge the data based upon id
```{r}
setwd("~/Google Drive/PARCS/Projects/PropScore/Data")
ECLSK1  = read.csv("ECLSK1.csv", header = TRUE)
ECLSK1 = na.omit(ECLSK1)
ECLSK1 = as.data.frame(ECLSK1)

ECLSK2  = read.csv("ECLSK2.csv", header = TRUE)
ECLSK2 = na.omit(ECLSK2)
ECLSK2 = as.data.frame(ECLSK2)

ECLSK3  = read.csv("ECLSK3.csv", header = TRUE)
ECLSK3 = na.omit(ECLSK3)
ECLSK3 = as.data.frame(ECLSK3)

ECLSK4  = read.csv("ECLSK4.csv", header = TRUE)
ECLSK4 = na.omit(ECLSK4)
ECLSK4 = as.data.frame(ECLSK4)

ECLSK5  = read.csv("ECLSK5.csv", header = TRUE)
ECLSK5 = na.omit(ECLSK5)
ECLSK5 = as.data.frame(ECLSK5)



# Now grab only the first from everydata set.  Adding a one to each of them to represent that only have the first variables.
ECLSK11 = cbind(id = ECLSK1$id, X1TCHAPP= ECLSK1$X1TCHAPP, X1TCHCON =ECLSK1$X1TCHCON, X1TCHPER=ECLSK1$X1TCHPER, X1TCHEXT=ECLSK1$X1TCHEXT, X1TCHINT=ECLSK1$X1TCHINT, X1RTHET=ECLSK1$X1RTHET, X1MTHET=ECLSK1$X1MTHET, X1BMI=ECLSK1$X1BMI, X1PUBPRI=ECLSK1$X1PUBPRI); head(ECLSK11)

ECLSK21 = cbind(id = ECLSK2$id, X1TCHAPP=ECLSK2$X1TCHAPP,X1TCHCON=ECLSK2$X1TCHCON, X1TCHPER=ECLSK2$X1TCHPER, X1TCHEXT=ECLSK2$X1TCHEXT, X1TCHINT=ECLSK2$X1TCHINT, X1RTHET=ECLSK2$X1RTHET, X1MTHET=ECLSK2$X1MTHET, X1BMI=ECLSK2$X1BMI, X1PUBPRI=ECLSK2$X1PUBPRI); head(ECLSK21)

ECLSK31 = cbind(id = ECLSK3$id, X1TCHAPP=ECLSK3$X1TCHAPP,X1TCHCON=ECLSK3$X1TCHCON, X1TCHPER=ECLSK3$X1TCHPER, X1TCHEXT=ECLSK3$X1TCHEXT, X1TCHINT=ECLSK3$X1TCHINT, X1RTHET=ECLSK3$X1RTHET, X1MTHET=ECLSK3$X1MTHET, X1BMI=ECLSK3$X1BMI, X1PUBPRI=ECLSK3$X1PUBPRI); head(ECLSK31)

ECLSK41 = cbind(id = ECLSK4$id, X1TCHAPP=ECLSK4$X1TCHAPP,X1TCHCON=ECLSK4$X1TCHCON, X1TCHPER=ECLSK4$X1TCHPER, X1TCHEXT=ECLSK4$X1TCHEXT, X1TCHINT=ECLSK4$X1TCHINT, X1RTHET=ECLSK4$X1RTHET,X1MTHET= ECLSK4$X1MTHET, X1BMI=ECLSK4$X1BMI, X1PUBPRI=ECLSK4$X1PUBPRI); head(ECLSK41)

ECLSK51 = cbind(id = ECLSK5$id, X1TCHAPP=ECLSK5$X1TCHAPP,X1TCHCON=ECLSK5$X1TCHCON, X1TCHPER=ECLSK5$X1TCHPER, X1TCHEXT=ECLSK5$X1TCHEXT, X1TCHINT=ECLSK5$X1TCHINT, X1RTHET=ECLSK5$X1RTHET, X1MTHET=ECLSK5$X1MTHET, X1BMI=ECLSK5$X1BMI, X1PUBPRI=ECLSK5$X1PUBPRI); head(ECLSK51)
```
Now get rid of first variables in each data set.  Then I will later merge the matched values from the full longitduinal data with the data set that is matched on the beginning values.
```{r}
ECLSK1$X1TCHAPP = ECLSK1$X1TCHCON = ECLSK1$X1TCHPER = ECLSK1$X1TCHEXT= ECLSK1$X1TCHINT= ECLSK1$X1RTHET= ECLSK1$X1MTHET= ECLSK1$X1BMI= ECLSK1$X1PUBPRI = NULL; head(ECLSK1)
          
ECLSK2$X1TCHAPP = ECLSK2$X1TCHCON = ECLSK2$X1TCHPER = ECLSK2$X1TCHEXT= ECLSK2$X1TCHINT= ECLSK2$X1RTHET= ECLSK2$X1MTHET= ECLSK2$X1BMI= ECLSK2$X1PUBPRI = NULL; head(ECLSK2)

ECLSK3$X1TCHAPP = ECLSK3$X1TCHCON = ECLSK3$X1TCHPER = ECLSK3$X1TCHEXT= ECLSK3$X1TCHINT= ECLSK3$X1RTHET= ECLSK3$X1MTHET= ECLSK3$X1BMI= ECLSK3$X1PUBPRI = NULL; head(ECLSK3)

ECLSK4$X1TCHAPP = ECLSK4$X1TCHCON = ECLSK4$X1TCHPER = ECLSK4$X1TCHEXT= ECLSK4$X1TCHINT= ECLSK4$X1RTHET= ECLSK4$X1MTHET= ECLSK4$X1BMI= ECLSK4$X1PUBPRI = NULL; head(ECLSK4)

ECLSK5$X1TCHAPP = ECLSK5$X1TCHCON = ECLSK5$X1TCHPER = ECLSK5$X1TCHEXT= ECLSK5$X1TCHINT= ECLSK5$X1RTHET= ECLSK5$X1MTHET= ECLSK5$X1BMI= ECLSK5$X1PUBPRI = NULL; head(ECLSK5)
```
Now we are analyzing one data set, using matchIT and seeing if we can get a regular regression and then a multilevel model with time.  We matched everyone.
```{r}
library(MatchIt)
ECLSK11 = as.data.frame(ECLSK11)
m.out1 = matchit(X1PUBPRI ~ X1TCHAPP + X1TCHCON + X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET + X1BMI, data = ECLSK11, method = "nearest", ratio = 1)

plot(m.out1, type = "jitter")
plot(m.out1, type = "hist")

# Now getting descriptives
m.data1 <- match.data(m.out1)
m.dataMeans1 = apply(m.data1, 2, mean)
m.dataSD1 = apply(m.data1,2, sd)

#Now getting data for other analyses.  Need the weights to figure out what is included and then treat specifcies which in are the treatment (i.e. nonpublic schools)
m.out1Data = as.data.frame(cbind(m.out1$X, weights = m.out1$weights, X1PUBPRI = m.out1$treat))
m.out1Data = m.out1Data[which(m.out1Data$weights == 1),]
head(m.out1Data)
write.csv(m.out1Data, "m.out1Data.csv")
m.out1Data = read.csv("m.out1Data.csv", header = TRUE)
head(m.out1Data)
colnames(m.out1Data)[1] = c("id")
```

Now we get the estimates for the second data set
```{r}
ECLSK21 = as.data.frame(ECLSK21)

m.out2 = matchit(X1PUBPRI ~ X1TCHAPP + X1TCHCON + X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET + X1BMI, data = ECLSK21, method = "nearest", ratio = 1)

# Now getting descriptives
m.data2 <- match.data(m.out2)
m.dataMeans2 = apply(m.data2, 2, mean)
m.dataSD2 = apply(m.data2,2, sd)

m.out2Data = as.data.frame(cbind(m.out2$X, weights = m.out2$weights, X1PUBPRI = m.out2$treat))
m.out2Data = m.out2Data[which(m.out2Data$weights == 1),]
head(m.out2Data)

write.csv(m.out2Data, "m.out2Data.csv")
m.out2Data = read.csv("m.out2Data.csv", header = TRUE)
head(m.out2Data)
colnames(m.out2Data)[1] = c("id")
```
Now get the estimates for the third data set
```{r}

ECLSK31 = as.data.frame(ECLSK31)

m.out3 = matchit(X1PUBPRI ~ X1TCHAPP + X1TCHCON + X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET + X1BMI, data = ECLSK31, method = "nearest", ratio = 1)

# Now getting descriptives
m.data3 <- match.data(m.out3)
m.dataMeans3 = apply(m.data3, 2, mean)
m.dataSD3 = apply(m.data3,2, sd)

m.out3Data = as.data.frame(cbind(m.out3$X, weights = m.out3$weights, X1PUBPRI = m.out3$treat))
m.out3Data = m.out3Data[which(m.out3Data$weights == 1),]
head(m.out3Data)

write.csv(m.out3Data, "m.out3Data.csv")
m.out3Data = read.csv("m.out3Data.csv", header = TRUE)
head(m.out3Data)
colnames(m.out3Data)[1] = c("id")

```
Now get the estimates for the fourth data set
```{r}
ECLSK41 = as.data.frame(ECLSK41)

m.out4 = matchit(X1PUBPRI ~ X1TCHAPP + X1TCHCON + X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET + X1BMI, data = ECLSK41, method = "nearest", ratio = 1)

# Now getting descriptives
m.data4 <- match.data(m.out4)
m.dataMeans4 = apply(m.data4, 2, mean)
m.dataSD4 = apply(m.data4,2, sd)

m.out4Data = as.data.frame(cbind(m.out4$X, weights = m.out4$weights, X1PUBPRI = m.out4$treat))
m.out4Data = m.out4Data[which(m.out4Data$weights == 1),]
head(m.out4Data)

write.csv(m.out4Data, "m.out4Data.csv")
m.out4Data = read.csv("m.out4Data.csv", header = TRUE)
head(m.out4Data)
colnames(m.out4Data)[1] = c("id")
```
Now we get the fifth data set estimates
```{r}
ECLSK51 = as.data.frame(ECLSK51)

m.out5 = matchit(X1PUBPRI ~ X1TCHAPP + X1TCHCON + X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET + X1BMI, data = ECLSK51, method = "nearest", ratio = 1)

# Now getting descriptives
m.data5 <- match.data(m.out5)
m.dataMeans5 = apply(m.data5, 2, mean)
m.dataSD5 = apply(m.data5,2, sd)

m.out5Data = as.data.frame(cbind(m.out5$X, weights = m.out5$weights, X1PUBPRI = m.out5$treat))
m.out5Data = m.out5Data[which(m.out5Data$weights == 1),]
head(m.out5Data)

write.csv(m.out5Data, "m.out5Data.csv")
m.out5Data = read.csv("m.out5Data.csv", header = TRUE)
head(m.out5Data)
colnames(m.out5Data)[1] = c("id")
```
Now we need to merge the 11's with the 1's by only those with the same ID? But I did not include an ID variable.  This merges the original data set minus the first values, and takes the matched data from the propscore matching and finds those people's later in times values from the long data set.

Change nonpublic to X1PUBPRI and grab the rest of those variables
```{r}
ECLSK1 = merge(ECLSK1, m.out1Data, by = c("id"))
ECLSK1$id = ECLSK1$X = NULL
ECLSK2 = merge(ECLSK2, m.out2Data, by = c("id"))
ECLSK3 = merge(ECLSK3, m.out3Data, by = c("id"))
ECLSK4 = merge(ECLSK4, m.out4Data, by = c("id"))
ECLSK5 = merge(ECLSK5, m.out5Data, by = c("id"))
```

Now make into long form for all five versions  
```{r}

ECLSK1Test= reshape(ECLSK1, varying = list(c("X1TCHAPP", "X2TCHAPP", "X3TCHAPP", "X4TCHAPP"), c("X1TCHCON", "X2TCHCON", "X3TCHCON", "X4TCHCON"), c("X1TCHPER", "X2TCHPER", "X3TCHPER", "X4TCHPER"), c("X1TCHEXT", "X2TCHEXT", "X3TCHEXT", "X4TCHEXT"), c("X1TCHINT", "X2TCHINT", "X3TCHINT", "X4TCHINT"), c("X1RTHET", "X2RTHET", "X3RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X3MTHET", "X4RTHET"), c("X1BMI", "X2BMI", "X3BMI", "X4BMI"), c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI","X4PUBPRI")), times = c(1,2,3,4), direction = "long")

ECLSK1$
data1 = as.data.frame(data1)
dim(data1)
head(data1)
```
Now we need to do the multilevel analysis (All get means across time for variables).
```{r}

```

Now we need to combine the results for the descriptive statistics
```{r}
library(Amelia)
allMeans = t(as.matrix(cbind(m.dataMeans1, m.dataMeans2, m.dataMeans3, m.dataMeans4, m.dataMeans5)))

allSDs = t(as.matrix(cbind(m.dataSD1, m.dataSD2, m.dataSD3, m.dataSD4, m.dataSD5)))


allMeansSDsCom = mi.meld(q = allMeans, se = allSDs)

```

Now we are getting the combined estimates for the SC variable
```{r}
# Here we need to rerrange the variables, because it wasn't working.  Here we are getting the

SCCof1 = as.data.frame(SCCof1)
names(SCCof1) = c("ParEst")

SCCof2 = as.data.frame(SCCof2)
names(SCCof2) = c("ParEst")

SCCof3 = as.data.frame(SCCof3)
names(SCCof3) = c("ParEst")

SCCof3 = as.data.frame(SCCof3)
names(SCCof3) = c("ParEst")

SCCof4 = as.data.frame(SCCof4)
names(SCCof4) = c("ParEst")

SCCof5 = as.data.frame(SCCof5)
names(SCCof5) = c("ParEst")

allParsSC = t(as.matrix(cbind(SCCof1, SCCof2, SCCof3, SCCof4, SCCof5)))

SCSes1 = as.data.frame(SCSes1)
names(SCSes1) = c("SE")

SCSes2 = as.data.frame(SCSes2)
names(SCSes2) = c("SE")

SCSes2 = as.data.frame(SCSes2)
names(SCSes2) = c("SE")

SCSes3 = as.data.frame(SCSes3)
names(SCSes3) = c("SE")

SCSes4 = as.data.frame(SCSes4)
names(SCSes4) = c("SE")

SCSes5 = as.data.frame(SCSes5)
names(SCSes5) = c("SE")

allSEsSC = t(as.matrix(cbind(SCSes1, SCSes2, SCSes3, SCSes4, SCSes5)))

allParsSesSCCom = mi.meld(q = allParsSC, se = allSEsSC)
allParsPaperSC = t(as.data.frame(allParsSesSCCom$q.mi))

allSesPaperSC = t(as.data.frame(allParsSesSCCom$se.mi))

allParSesPaperSC = cbind(allParsPaperSC, allSesPaperSC)
write.csv(allParSesPaperSC, "allParSesPaperSC.csv")
```

