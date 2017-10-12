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
data1 = cbind(X1TCHAPP = data$X1TCHAPP, X2TCHAPP = data$X2TCHAPP, X3TCHAPP = data$X3TCHAPP, X4TCHAPP = data$X4TCHAPP, X1TCHCON = data$X1TCHCON, X2TCHCON = data$X2TCHCON, X3TCHCON = data$X3TCHCON, X4TCHCON = data$X4TCHCON,X1TCHPER = data$X1TCHPER, X2TCHPER = data$X2TCHPER, X3TCHPER = data$X3TCHPER, X4TCHPER = data$X4TCHPER, X1TCHEXT = data$X1TCHEXT, X2TCHEXT = data$X2TCHEXT, X3TCHEXT = data$X3TCHEXT, X4TCHEXT = data$X4TCHEXT, X1TCHINT = data$X1TCHINT, X2TCHINT = data$X2TCHINT, X3TCHINT = data$X3TCHINT, X4TCHINT = data$X4TCHINT,X1RTHET = data$X1RTHET, X2RTHET = data$X2RTHET, X3RTHET = data$X3RTHET, X4RTHET = data$X4RTHET, X1MTHET = data$X1MTHET, X2MTHET = data$X2MTHET, X3MTHET = data$X3MTHET, X4MTHET = data$X4MTHET, X1BMI = data$X1BMI, X2BMI = data$X2BMI, X3BMI = data$X3BMI, X4BMI = data$X4BMI, X1PUBPRI = data$X1PUBPRI, X2PUBPRI = data$X2PUBPRI, X3PUBPRI = data$X3PUBPRI, X4PUBPRI = data$X4PUBPRI)
head(data1)
# Change the -9 to NAs
data1 = apply(data1, 2, function(x){ifelse(x == -9, NA, x)})
data1 = as.data.frame(data1)
head(data1)
dim(data1)
```
Change X1PUBPRI to everything 1 = 1 and everything zero this will get you 1 is public and all nonpublic is 0. 
```{r}
XPUBPRI = cbind(X1PUBPRI = data$X1PUBPRI, X2PUBPRI = data$X2PUBPRI, X3PUBPRI = data$X3PUBPRI, X4PUBPRI = data$X4PUBPRI)

XPUBPRI = apply(XPUBPRI, 2, function(x){ifelse(x == 1, 1, 0)})
data1 = data1[-c(34:37)]
head(data1)
data1 = cbind(data1, XPUBPRI)

```

Now make into long form.  
```{r}
data1 = reshape(data1, varying = list(c("X1TCHAPP", "X2TCHAPP", "X3TCHAPP", "X4TCHAPP"), c("X1TCHCON", "X2TCHCON", "X3TCHCON", "X4TCHCON"), c("X1TCHPER", "X2TCHPER", "X3TCHPER", "X4TCHPER"), c("X1TCHEXT", "X2TCHEXT", "X3TCHEXT", "X4TCHEXT"), c("X1TCHINT", "X2TCHINT", "X3TCHINT", "X4TCHINT"), c("X1RTHET", "X2RTHET", "X3RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X3MTHET", "X4RTHET"),c("X1MTHET", "X2MTHET", "X3MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X3BMI", "X4BMI"), c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI","X4PUBPRI")), times = c(1,2,3,4), direction = "long")
data1 = as.data.frame(data1)
dim(data1)
head(data1)
```
Here we will use Amelia.  Need to set m as five for five imputed data sets.  Then we place each of the variables into their appropriate categories.

```{r}
library(Amelia)
library(mitools)
library(survey)
m = 5
a.out = amelia(x = data1, m=m, logistic = c("X1PUBPRI"))
# Now we can creat seperate data set and then analyze them seperately and combine them later with the mi.meld function in Ameila
summary(a.out)
compare.density(a.out, var = "X1TCHCON", main = "Observed and Imputed values of Self Control")
disperse(a.out, dims = 1, m = 5)
write.amelia(obj = a.out, file.stem = "ECLSK")
head(data1)
```
Now we are analyzing one data set, using matchIT and seeing if we can get a regular regression and then a multilevel model with time.  We matched everyone.

Here are the estimates for the first model.  Need to grab the parameter estimates and sd's 
```{r}
library(MatchIt)
setwd("~/Google Drive/PARCS/Projects/PropScore/Data")
ECLSK1  = read.csv("ECLSK1.csv", header = TRUE)
ECLSK1 = na.omit(ECLSK1)
ECLSK1 = as.data.frame(ECLSK1)


# Need to change all of these to include the new covariates
m.out1 = matchit(X1PUBPRI ~ time + X1TCHAPP + X1TCHCON + X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET + X1BMI, data = ECLSK1, method = "nearest", ratio = 1)
ECLSK1$X1PUBPRI

plot(m.out1, type = "jitter")
plot(m.out1, type = "hist")

# Now getting descriptives
m.data1 <- match.data(m.out1)
m.dataMeans1 = apply(m.data1, 2, mean)
m.dataSD1 = apply(m.data1,2, sd)

# Need to change all of these to include the new covariates
library(Zelig)
m.out1$weights
# Grabbing the parameter estimates and se's
z.outSC1 = zelig(X1PRNCON ~ + S2REGSKL +  X1_CHSEX_R + X1HPARNT + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1PRNSAD + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls" , data = match.data(m.out1))
summary(z.outSC1)

SCCof1 =  z.outSC1$get_coef()
SCSes1 = z.outSC1$get_se()


# Here is for the social interaction variable.
z.outSI1 = zelig(X1PRNSOC ~ + S2REGSKL +  X1_CHSEX_R + X1HPARNT  + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON  + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls", data = match.data(m.out1))
summary(z.outSI1)
SICof1 =  z.outSI1$get_coef()
SISes1 = z.outSI1$get_se()

```
Now we are getting residual analyses for first the data set
```{r}
resZ.outSC1 = as.data.frame(z.outSC1$get_residuals())
resZ.outSC1 = resZ.outSC1[,1]
resZ.outSC1 = as.data.frame(resZ.outSC1)
resZ.outSC1 = as.data.frame(resZ.outSC1$resZ.outSC1)


fitZ.outSC1 = as.data.frame(z.outSC1$get_predict())
fitZ.outSC1 = fitZ.outSC1[,1]
fitZ.outSC1 = as.data.frame(fitZ.outSC1)
fitZ.outSC1 = as.data.frame(fitZ.outSC1$fitZ.outSC1)
fitResidSC1 = cbind(fitZ.outSC1, resZ.outSC1)
plot(fitResidSC1)
plot(resZ.outSC1)

resZ.outSC1 = scale(resZ.outSC1, center = TRUE, scale = TRUE)
hist(resZ.outSC1, xlim = c(-5,5))
above = sum(ifelse(resZ.outSC1 > 3, 1, 0))
below = sum(ifelse(resZ.outSC1 < -3, 1, 0))
totalS = sum(above, below)
totalS / (7694 + 19)*100
```
Now we get the estimates for the second data set
```{r}
setwd("~/Google Drive/PARCS/Projects/PropScore/Data")
ECLSK2  = read.csv("ECLSK2.csv", header = TRUE)
ECLSK2 = ECLSK2[c(-1)]
ECLSK2 = na.omit(ECLSK2)
ECLSK2 = as.data.frame(ECLSK2)

m.out2 = matchit(S2REGSKL ~ X1_CHSEX_R + X1HPARNT + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD + X1PRNIMP + X1RTHET + X1MTHET, data = ECLSK1, method = "nearest", ratio = 1)

plot(m.out2, type = "jitter")
plot(m.out2, type = "hist")

# Now getting descriptives
m.data2 <- match.data(m.out2)
m.dataMeans2 = apply(m.data2, 2, mean)
m.dataSD2 = apply(m.data2,2, sd)


library(Zelig)
# Grabbing the parameter estimates and se's
z.outSC2 = zelig(X1PRNCON ~ + S2REGSKL +  X1_CHSEX_R + X1HPARNT + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls", data = match.data(m.out2))
SCCof2 =  z.outSC2$get_coef()
SCSes2 = z.outSC2$get_se()


```
Now get the estimates for the third data set
```{r}
setwd("~/Google Drive/PARCS/Projects/PropScore/Data")
ECLSK3  = read.csv("ECLSK3.csv", header = TRUE)
ECLSK3 = ECLSK3[c(-1)]
ECLSK3 = na.omit(ECLSK3)
ECLSK3 = as.data.frame(ECLSK3)

m.out3 = matchit(S2REGSKL ~ X1_CHSEX_R + X1HPARNT + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD + X1PRNIMP + X1RTHET + X1MTHET, data = ECLSK1, method = "nearest", ratio = 1)

plot(m.out3, type = "jitter")
plot(m.out3, type = "hist")

# Now getting descriptives
m.data3 <- match.data(m.out3)
m.dataMeans3 = apply(m.data3, 2, mean)
m.dataSD3 = apply(m.data3,2, sd)


library(Zelig)
# Grabbing the parameter estimates and se's
z.outSC3 = zelig(X1PRNCON ~ + S2REGSKL +  X1_CHSEX_R + X1HPARNT  + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls", data = match.data(m.out3))
SCCof3 =  z.outSC3$get_coef()
SCSes3 = z.outSC3$get_se()


```
Now get the estimates for the fourth data set
```{r}
setwd("~/Google Drive/PARCS/Projects/PropScore/Data")
ECLSK4  = read.csv("ECLSK4.csv", header = TRUE)
ECLSK4 = ECLSK4[c(-1)]
ECLSK4 = na.omit(ECLSK4)
ECLSK4 = as.data.frame(ECLSK4)

m.out4 = matchit(S2REGSKL ~ X1_CHSEX_R + X1HPARNT + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD + X1PRNIMP + X1RTHET + X1MTHET, data = ECLSK1, method = "nearest", ratio = 1)
summary(m.out4)
plot(m.out4, type = "jitter")
plot(m.out4, type = "hist")

# Now getting descriptives
m.data4 <- match.data(m.out4)
m.dataMeans4 = apply(m.data4, 2, mean)
m.dataSD4 = apply(m.data4,2, sd)


library(Zelig)
# Grabbing the parameter estimates and se's
z.outSC4 = zelig(X1PRNCON ~ + S2REGSKL +  X1_CHSEX_R + X1HPARNT  + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls", data = match.data(m.out4))
SCCof4 =  z.outSC4$get_coef()
SCSes4 = z.outSC4$get_se()


```
Now we get the fifth data set estimates
```{r}
setwd("~/Google Drive/PARCS/Projects/PropScore/Data")
ECLSK5  = read.csv("ECLSK5.csv", header = TRUE)
ECLSK5 = ECLSK5[c(-1)]
ECLSK5 = na.omit(ECLSK5)
ECLSK5 = as.data.frame(ECLSK5)

m.out5 = matchit(S2REGSKL ~ X1_CHSEX_R + X1HPARNT + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD + X1PRNIMP + X1RTHET + X1MTHET, data = ECLSK1, method = "nearest", ratio = 1)
summary(m.out5)
plot(m.out5, type = "jitter")
plot(m.out5, type = "hist")

# Now getting descriptives
m.data5 <- match.data(m.out5)
m.dataMeans5 = apply(m.data5, 2, mean)
m.dataSD5 = apply(m.data5,2, sd)


library(Zelig)
# Grabbing the parameter estimates and se's
z.outSC5 = zelig(X1PRNCON ~ + S2REGSKL +  X1_CHSEX_R + X1HPARNT  + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls", data = match.data(m.out5))
SCCof5 =  z.outSC5$get_coef()
SCSes5 = z.outSC5$get_se()


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

