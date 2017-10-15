---
title: "ECLS-K-2011"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Here is an example using the Early Childhood Longitudinal Study Kindergarten 2011 study data to evaluate how kindergartens in regular public (i.e. kindergarteners attending the school they are geographically assigned) versus those in alternative regular public schools (e.g. private, public charter or magnet schools) over time on a self control construct.  I will go through how to grab and clean data, impute the missing data, match the data using propensity score matching, use multilevel modeling to track self control over time, then aggregate the results from the different imputted data sets to create a table of final results.  Below is list of the variables included in the model and their names from the ECLSK2011 data set.

X1TCHAPP = teacher report approaches to learning
X1TCHCON = teacher report self control
X1TCHPER = teacher report interperonsal
X1TCHEXT = teacher report external problem behaviors  
X1TCHINT = teacher report internal problem behaviors
X1BMI = Body mass index
X1PUBPRI = whether a student attended a regular public (i.e the school assinged the child based upon their address) or an alternative to regular public school (e.g. private school, public charter or magnet) 


The first step is to create a data set with each of the variables above over the four times points.  The first time point takes place in the fall of 2010, then spring 2011, then fall 2011, and finally spring 2012.  The 1 in each variable indicates fall 2010 and so on for 2 (spring 2011),3 (fall 2011),4 (spring 2012).

I then change all of the negative nine's to NA, so that I can impute then later on.

There are some time varying variables that are missing some time points (look for NAs in code) therefore I need to impute those to include them in the model
```{r}
#setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data")
#data = read.csv("ELCS-K-2011.csv", header = TRUE)

# Now collecting variables for all teacher self report
# The only level two variables that stable with the person are the child's and parent's race.
data1 = cbind(id = 1:length(data$X1TCHAPP), X1TCHAPP = data$X1TCHAPP, X2TCHAPP = data$X2TCHAPP, X3TCHAPP = data$X3TCHAPP, X4TCHAPP = data$X4TCHAPP, X1TCHCON = data$X1TCHCON, X2TCHCON = data$X2TCHCON, X3TCHCON = data$X3TCHCON, X4TCHCON = data$X4TCHCON,X1TCHPER = data$X1TCHPER, X2TCHPER = data$X2TCHPER, X3TCHPER = data$X3TCHPER, X4TCHPER = data$X4TCHPER, X1TCHEXT = data$X1TCHEXT, X2TCHEXT = data$X2TCHEXT, X3TCHEXT = data$X3TCHEXT, X4TCHEXT = data$X4TCHEXT, X1TCHINT = data$X1TCHINT, X2TCHINT = data$X2TCHINT, X3TCHINT = data$X3TCHINT, X4TCHINT = data$X4TCHINT,X1RTHET = data$X1RTHET, X2RTHET = data$X2RTHET, X3RTHET = data$X3RTHET, X4RTHET = data$X4RTHET, X1MTHET = data$X1MTHET, X2MTHET = data$X2MTHET, X3MTHET = data$X3MTHET, X4MTHET = data$X4MTHET, X1BMI = data$X1BMI, X2BMI = data$X2BMI, X3BMI = data$X3BMI, X4BMI = data$X4BMI, X1PUBPRI = data$X1PUBPRI, X2PUBPRI = data$X2PUBPRI, X3PUBPRI = data$X3PUBPRI, X4PUBPRI = data$X4PUBPRI, X1PAR1AGE = data$X1PAR1AGE, X2PAR1AGE= data$X2PAR1AGE, X3PAR1AGE = rep(NA, length(data$X1TCHAPP)), X4PAR1AGE = data$X4PAR1AGE, X1HTOTAL = data$X1HTOTAL,  X2HTOTAL = data$X2HTOTAL,  X3HTOTAL = rep(NA, length(data$X1TCHAPP)),  X4HTOTAL = data$X4HTOTAL, X1SESL =data$X12SESL, X2SESL =data$X12SESL, X12SESL = rep(NA, length(data$X1TCHAPP)), X4SESL = data$X4SESL, X1PAR1ED = data$X12PAR1ED_I, X2PAR1ED = data$X12PAR1ED_I, X3PAR1ED = rep(NA, length(data$X1TCHAPP)), X4PAR1ED = data$X4PAR1ED, X1LANGST = data$X12LANGST, X2LANGST = data$X12LANGST, X3LANGST = rep(NA, length(data$X1TCHAPP)), X4LANGST = data$X4LANGST, X1_CHSEX_R = data$X_CHSEX_R,  X2_CHSEX_R = data$X_CHSEX_R,  X3_CHSEX_R = data$X_CHSEX_R,  X4_CHSEX_R = data$X_CHSEX_R, X1HPARNT = data$X1HPARNT, X2HPARNT = data$X1HPARNT, X3HPARNT = data$X1HPARNT, X3HPARNT = data$X1HPARNT, X4HPARNT = data$X1HPARNT)
head(data1)

head(data1)
# Change the -9 to NAs
data1 = apply(data1, 2, function(x){ifelse(x == -9, NA, x)})
data1 = as.data.frame(data1)
head(data1)
dim(data1)
```
Here I am creating a separate data set for the indicator of interest PUBPRI where I am setting the variable to be a 1 is alternative to public school and 0 is a regular public school.  

I then get rid of the original PUBPRI variables from the data set created above and then replace them with new PUBPRI variables created here so that original data set has the PUBPRI variables that are all 1's, 0's, and NA's. 
```{r}
XPUBPRI = cbind(X1PUBPRI = data$X1PUBPRI, X2PUBPRI = data$X2PUBPRI, X3PUBPRI = data$X3PUBPRI, X4PUBPRI = data$X4PUBPRI)

XPUBPRI = as.data.frame(apply(XPUBPRI, 2, function(x){ifelse(x == 1, 0, 1)}))
head(XPUBPRI)
data1 = data1[-c(34:38)]
data1 = cbind(data1, XPUBPRI); head(data1)
```
Now I need to clean up the demographics.
```{r}
data2 = cbind(X12LANGST = data1$X12LANGST,  X_CHSEX_R = data1$X_CHSEX_R)

data2 = ifelse(is.na(data2), NA, ifelse(data2 == 1, 1,0))
data2 = as.data.frame(data2)
head(data2)

# Here is the ethnicity variable that needs to be transformed into the original variables that you used.  Remember that original variable were incorrect and just keeping the names the same here for consistency.
X_HISP_R = ifelse(is.na(data3), NA, ifelse(data3 == 3, 1, 0))
X_HISP_R = as.data.frame(X_HISP_R)
names(X_HISP_R) = c("X_HISP_R")

X_WHITE_R = ifelse(is.na(data3), NA, ifelse(data3 == 1, 1, 0))
X_WHITE_R = as.data.frame(X_WHITE_R)
names(X_WHITE_R) = c("X_WHITE_R")
sum(X_WHITE_R, na.rm =TRUE)

X_BLACK_R = ifelse(is.na(data3), NA, ifelse(data3 == 2, 1, 0))
X_BLACK_R = as.data.frame(X_BLACK_R)
names(X_BLACK_R) = c("X_BLACK_R")

X_ASIAN_R = ifelse(is.na(data3), NA, ifelse(data3 == 5, 1, 0))
X_ASIAN_R = as.data.frame(X_ASIAN_R)
names(X_ASIAN_R) = c("X_ASIAN_R")

X_AMINAN_R = ifelse(is.na(data3), NA, ifelse(data3 == 7, 1, 0))
X_AMINAN_R = as.data.frame(X_AMINAN_R)
names(X_AMINAN_R) = c("X_AMINAN_R")

X_HAWPI_R = ifelse(is.na(data3), NA, ifelse(data3 == 6, 1, 0))
X_HAWPI_R = as.data.frame(X_HAWPI_R)
names(X_HAWPI_R) = c("X_HAWPI_R")

X_MULTR_R = ifelse(is.na(data3), NA, ifelse(data3 == 8, 1, 0))
X_MULTR_R = as.data.frame(X_MULTR_R)
names(X_MULTR_R) = c("X_MULTR_R")


data4 = cbind(X_HISP_R, X_WHITE_R, X_BLACK_R, X_ASIAN_R, X_AMINAN_R, X_AMINAN_R, X_HAWPI_R, X_MULTR_R)
data4 = as.data.frame(data4)

# Need to change 2 through 8 to be 1 and 1 to be zero
data6 = cbind(X1PAR1RAC = data1$X1PAR1RAC)
data6 = ifelse(is.na(data6), NA, ifelse(data6 == 1, 0,1))
data6 = as.data.frame(data6)
```



Here is where I will use Amelia.  Amelia is an R package developed by Gary King at Harvard University.  If a person is missing a data point, Amelia uses information from that person's variables (i.e. demograpics, other test scores) to predict what that missing would have been.  Because there can be variability in imputting or predicting missing data values researchers like King recommend developing at least five data sets containing imputted values and then averaging them together (I average the results together at the end).  To use amelia, you can set x to the data set of interest, m equal to the number of data sets that you want to impute and for non-continuous variables you can the noms argument if they are nomial or the logs for count variables.  

Additionally, we 

Here we will use Amelia.  Need to set m as five for five imputed data sets.  Then we place each of the variables into their appropriate categories.

The summary provides information on the algorithm's convergence and the percentage of missing data per variable.

The compare density function compares the density of values for the self control, variable of interest over actual values.  So this function predicts what the nonmissing values would have been using Amelia's algorithm and then compares the density of a specified variable to the actual variable density.  If the density's match, then there is some evidence that the missing value algorithm is at least able to predict nonmissing values, which as close we as get to estimating how well the algorithm would predict the missing values.

The disperse function evaluates how quickly the chains converged or if they converged at all.  Essentally, if the chains all converge on the same location the algorithm was able to adequetly able to find maxiumum likelihood (i.e. find given the data find the value that maximizes the probability that paramter estimates predict the actual values).  

Finally, I write the data sets to csv files, which will be loaded in again for further analyses.

```{r}
library(Amelia)
library(mitools)
library(survey)
m = 5
a.out = amelia(x = data1, m=m, noms = c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI", "X4PUBPRI"))
# Now we can creat seperate data set and then analyze them seperately and combine them later with the mi.meld function in Ameila
summary(a.out)
compare.density(a.out, var = "X1TCHCON", main = "Observed and Imputed values of Self Control")
disperse(a.out, dims = 1, m = 5)
write.amelia(obj = a.out, file.stem = "ECLSK")
```
Here I am reading back in the imputted data sets.  Then I am creating data sets with only the fall 2010 (i.e. variables wit 1's in them) for each of the imputed data sets.  I am other gathering the first time points for each variable for two reasons.  First, my analysis is an intent to treat (ITT).  ITT in this context means that I a focusing students starting in the treatment (i.e. an alternative to public school) and not worrying about if they stay in that school.  Given that I have no control over whether students switch or move between schools focusing on students starting in alternative to regular public schools makes sense.  Second, we are unaware of propensity score models for matching on longitudinal data sets reliable and therefore want to use a reliable propensity scores matching package, which can only match students on cross section data.  We cannot use current propensity score matching tools, because we are it is not possible to match keep the assignement of the same person to the same treatment.  Additinally, to be consistent with the intent to treat we are those who start out with similar characterstics.
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
Now we are analyzing the data with only the first time points for each student.  We are matching students on the their fall 2010 characterstics included in the model below which are creating scores for how likely a student is to fall in their into the treatment category.  Then matchit finds those students in the control group that have the highest likliehood to be in the treatment (i.e. highest propensity scores) and matches them with a similar student in the treatment group.  Therefore, it also us to state that students in the treatment and control are similar on the inlcuded pretreatment covarites.  In this model, I am using the nearest neighbor algorithm where I am matching each student from the treatment (i.e. alternative to assigned public school) to a control student (i.e. assigned public school).  Then, for the first data, remember there are five imputted data sets, I examine the jitter plot which shows the distribution of matched students in the treatment and control.  In this example the two almost exactly align providing a good sign that I have students that are very similar on the included covariates.  Next I look at a histogram of the matched students from the treatment and control groups and again the distributions of propensity scores seems almost identical providing more evidence that students in the treatment and control are very similar on included covariates.

Then I need to compile a dataset with the weights, which include whether a student was matched or not, the treatment, which is whether or not the student is in an alternative to assigned public school or not, and only select those students who were matched.  
```{r}
library(MatchIt)
ECLSK11 = as.data.frame(ECLSK11)
m.out1 = matchit(X1PUBPRI ~ X1TCHAPP + X1TCHCON + X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET + X1BMI, data = ECLSK11, method = "nearest", ratio = 1)

#plot(m.out1, type = "jitter")
#plot(m.out1, type = "hist")

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

Repeat the process above for data set two.
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
Repeat the process above for data set three.
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
Repeat the process above for data set four.
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
Repeat the process above for data set five.
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
Now I need to conduct an inner join with the data set that has time points 2 through 4 with the matched data set on with time point 1.  This is pretty simple with the merge function in r, which by default with conduct an inner join (i.e. only include in the data set those id values that are present in both data sets).
```{r}

ECLSK1 = merge(ECLSK1, m.out1Data, by = c("id"))
dim(ECLSK1)
ECLSK2 = merge(ECLSK2, m.out2Data, by = c("id"))
ECLSK3 = merge(ECLSK3, m.out3Data, by = c("id"))
ECLSK4 = merge(ECLSK4, m.out4Data, by = c("id"))
ECLSK5 = merge(ECLSK5, m.out5Data, by = c("id"))
```

Here I can create transform my cross sectional data set into a long data set, where instead of having four columns for each variable, I have one column for each variable with a time variable that indicates, which time point that response is alligned with.
```{r}

ECLSK1= reshape(ECLSK1, varying = list(c("X1TCHAPP", "X2TCHAPP", "X3TCHAPP", "X4TCHAPP"), c("X1TCHCON", "X2TCHCON", "X3TCHCON", "X4TCHCON"), c("X1TCHPER", "X2TCHPER", "X3TCHPER", "X4TCHPER"), c("X1TCHEXT", "X2TCHEXT", "X3TCHEXT", "X4TCHEXT"), c("X1TCHINT", "X2TCHINT", "X3TCHINT", "X4TCHINT"), c("X1RTHET", "X2RTHET", "X3RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X3MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X3BMI", "X4BMI"), c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI","X4PUBPRI")), times = c(1,2,3,4), direction = "long")


ECLSK2= reshape(ECLSK2, varying = list(c("X1TCHAPP", "X2TCHAPP", "X3TCHAPP", "X4TCHAPP"), c("X1TCHCON", "X2TCHCON", "X3TCHCON", "X4TCHCON"), c("X1TCHPER", "X2TCHPER", "X3TCHPER", "X4TCHPER"), c("X1TCHEXT", "X2TCHEXT", "X3TCHEXT", "X4TCHEXT"), c("X1TCHINT", "X2TCHINT", "X3TCHINT", "X4TCHINT"), c("X1RTHET", "X2RTHET", "X3RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X3MTHET", "X4RTHET"), c("X1BMI", "X2BMI", "X3BMI", "X4BMI"), c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI","X4PUBPRI")), times = c(1,2,3,4), direction = "long")

ECLSK3= reshape(ECLSK3, varying = list(c("X1TCHAPP", "X2TCHAPP", "X3TCHAPP", "X4TCHAPP"), c("X1TCHCON", "X2TCHCON", "X3TCHCON", "X4TCHCON"), c("X1TCHPER", "X2TCHPER", "X3TCHPER", "X4TCHPER"), c("X1TCHEXT", "X2TCHEXT", "X3TCHEXT", "X4TCHEXT"), c("X1TCHINT", "X2TCHINT", "X3TCHINT", "X4TCHINT"), c("X1RTHET", "X2RTHET", "X3RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X3MTHET", "X4RTHET"), c("X1BMI", "X2BMI", "X3BMI", "X4BMI"), c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI","X4PUBPRI")), times = c(1,2,3,4), direction = "long")

ECLSK4= reshape(ECLSK4, varying = list(c("X1TCHAPP", "X2TCHAPP", "X3TCHAPP", "X4TCHAPP"), c("X1TCHCON", "X2TCHCON", "X3TCHCON", "X4TCHCON"), c("X1TCHPER", "X2TCHPER", "X3TCHPER", "X4TCHPER"), c("X1TCHEXT", "X2TCHEXT", "X3TCHEXT", "X4TCHEXT"), c("X1TCHINT", "X2TCHINT", "X3TCHINT", "X4TCHINT"), c("X1RTHET", "X2RTHET", "X3RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X3MTHET", "X4RTHET"), c("X1BMI", "X2BMI", "X3BMI", "X4BMI"), c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI","X4PUBPRI")), times = c(1,2,3,4), direction = "long")

ECLSK5= reshape(ECLSK5, varying = list(c("X1TCHAPP", "X2TCHAPP", "X3TCHAPP", "X4TCHAPP"), c("X1TCHCON", "X2TCHCON", "X3TCHCON", "X4TCHCON"), c("X1TCHPER", "X2TCHPER", "X3TCHPER", "X4TCHPER"), c("X1TCHEXT", "X2TCHEXT", "X3TCHEXT", "X4TCHEXT"), c("X1TCHINT", "X2TCHINT", "X3TCHINT", "X4TCHINT"), c("X1RTHET", "X2RTHET", "X3RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X3MTHET", "X4RTHET"), c("X1BMI", "X2BMI", "X3BMI", "X4BMI"), c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI","X4PUBPRI")), times = c(1,2,3,4), direction = "long")
head(ECLSK1)
```
Here is where I will describe the model in latex


Now I will put the model into nlme to analyze it longitudinally.  The first model is the null model that only contains the intercept for each person (i.e. their average value over time).  Then we compare the null model to a with the covariates of interest with variable of interest the interaction between time and the PUBPRI variable. 
```{r}
library(nlme)

model11 = lme(fixed = X1TCHCON ~1, random =  ~ 1 | id, data = ECLSK1, method = "ML")
summary(model1)

model21 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI, random = ~1 | id, data = ECLSK1, method = "ML")

model31 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI, random = ~time | id, data = ECLSK1, method = "ML")

anova(model11, model21, model31)

model12 = lme(fixed = X1TCHCON ~1, random =  ~ 1 | id, data = ECLSK2, method = "ML")

model22 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI, random = ~1 | id, data = ECLSK2, method = "ML")

model32 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI, random = ~time | id, data = ECLSK2, method = "ML")
summary(model32)

anova(model12, model22, model32)


model13 = lme(fixed = X1TCHCON ~1, random =  ~ 1 | id, data = ECLSK3, method = "ML")

model23 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI, random = ~1 | id, data = ECLSK3, method = "ML")


model33 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI, random = ~time | id, data = ECLSK3, method = "ML")

anova(model13, model23, model33)

model14 = lme(fixed = X1TCHCON ~1, random =  ~ 1 | id, data = ECLSK4, method = "ML")

model24 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI, random = ~1 | id, data = ECLSK4, method = "ML")


model34 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI, random = ~time | id, data = ECLSK4, method = "ML")

anova(model14, model24, model34)


model15 = lme(fixed = X1TCHCON ~1, random =  ~ 1 | id, data = ECLSK5, method = "ML")

model25 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI, random = ~1 | id, data = ECLSK5, method = "ML")


model35 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI, random = ~time | id, data = ECLSK5, method = "ML")

anova(model15, model25, model35)

```
Now I need to grab the coefficients for the multilevel model.  Need parameter and sd.
```{r}
model31 = summary(model31)
model31= model31$tTable[10,c(1:2)]

model32 = summary(model32)
model32= model32$tTable[10,c(1:2)]

model33 = summary(model33)
model33= model33$tTable[10,c(1:2)]

model34 = summary(model34)
model34= model34$tTable[10,c(1:2)]

model35 = summary(model35)
model35= model35$tTable[10,c(1:2)]

modelAll = as.data.frame(t(cbind(model31, model32, model33, model34, model35)))
rownames(modelAll) <- c()

modelPars = as.data.frame(modelAll$Value)
colnames(modelPars) = c("Pars")
modelSE = as.data.frame(modelAll$Std.Error)
colnames(modelSE) = c("SE")

allPars = mi.meld(q = modelPars, se = modelSE)

allParsPE = t(as.data.frame(allPars$q.mi))

allParsSE =  t(as.data.frame(allPars$se.mi))

allParSesPaperSC = cbind(allParsPE, allParsSE)
colnames(allParSesPaperSC) = c("ParameterEstimate", "StandardError")
write.csv(allParSesPaperSC, "allParSesPaperSC.csv")

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

