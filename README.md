---
title: "ECLS-K-2011"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Here is an example using the Early Childhood Longitudinal Study Kindergarten 2011 cohort study data to evaluate how kindergartens in assigned public (i.e. kindergarteners attending the school they are geographically assigned) versus those in alternative regular public schools (e.g. private, public charter or magnet schools) over time on a self control construct.  I will go through how to grab and clean data, impute the missing data, match the data using propensity score matching, use multilevel modeling to track self control over time, then aggregate the results from the different imputed data sets to create a table of final results.  Here is a link to a description of the variables: https://docs.google.com/spreadsheets/d/1UngXf85XP29WHhUx0f88b7ceGJ49ezPKTuUqjedgPlE/edit?usp=sharing

The three variables of interest will be the XPUBPRI, which is whether a student attended an assigned public (i.e the school assigned the child based upon their address) or an alternative to regular public school (e.g. private school, public charter or magnet).  Time, which indicates the four time points described below and XTCHCON, which is the teacher's reports of the student's self control.

The first step is to create a data set with each of the variables above over the four time points.  The first time point takes place in the fall of 2010, then spring 2011, then fall 2011, and finally spring 2012.  The 1 in each variable indicates fall 2010 and so on for 2 (spring 2011),3 (fall 2011), 4 (spring 2012).

I then change all of the negative -9’s to NA, so that I can impute then later on.
```{r}
# setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data")
# data = read.csv("ELCS-K-2011.csv", header = TRUE)
data1 = cbind(id = 1:length(data$X1TCHAPP), X1TCHAPP = data$X1TCHAPP, X2TCHAPP = data$X2TCHAPP, X3TCHAPP = data$X3TCHAPP, X4TCHAPP = data$X4TCHAPP, X1TCHCON = data$X1TCHCON, X2TCHCON = data$X2TCHCON, X3TCHCON = data$X3TCHCON, X4TCHCON = data$X4TCHCON,X1TCHPER = data$X1TCHPER, X2TCHPER = data$X2TCHPER, X3TCHPER = data$X3TCHPER, X4TCHPER = data$X4TCHPER, X1TCHEXT = data$X1TCHEXT, X2TCHEXT = data$X2TCHEXT, X3TCHEXT = data$X3TCHEXT, X4TCHEXT = data$X4TCHEXT, X1TCHINT = data$X1TCHINT, X2TCHINT = data$X2TCHINT, X3TCHINT = data$X3TCHINT, X4TCHINT = data$X4TCHINT,X1RTHET = data$X1RTHET, X2RTHET = data$X2RTHET, X3RTHET = data$X3RTHET, X4RTHET = data$X4RTHET, X1MTHET = data$X1MTHET, X2MTHET = data$X2MTHET, X3MTHET = data$X3MTHET, X4MTHET = data$X4MTHET, X1BMI = data$X1BMI, X2BMI = data$X2BMI, X3BMI = data$X3BMI, X4BMI = data$X4BMI)

data1 = apply(data1, 2, function(x){ifelse(x == -9, NA, x)})
data1 = as.data.frame(data1)
head(data1)
```
Here I am creating a separate data set for the indicator of interest PUBPRI where I am setting the variable to be a 1 is alternative to public school and 0 is a regular public school.  

Here I am creating a separate data set for the indicator of interest PUBPRI where I am setting the variable to be a 1 is alternative to public school and 0 is a regular public school.  I am making these changes to PUBPRI separately, because I need to make specific transformations to these variables and not to the other variables in the dataset.  I will combine this data set with the one described above.
```{r}
XPUBPRI = cbind(X1PUBPRI = data$X1PUBPRI, X2PUBPRI = data$X2PUBPRI, X3PUBPRI = data$X3PUBPRI, X4PUBPRI = data$X4PUBPRI)
XPUBPRI = apply(XPUBPRI, 2, function(x){ifelse(x == -9, NA, x)})
XPUBPRI = as.data.frame(XPUBPRI)
XPUBPRI = as.data.frame(apply(XPUBPRI, 2, function(x){ifelse(x == 1, 0, 1)}))
head(XPUBPRI)
data1 = cbind(data1, XPUBPRI); head(data1)
mean(data1$X1PUBPRI, na.rm = TRUE)
```
Here I am cleaning up the demographic variables that I want to include, which are the child's gender (female 1 male 0), the child's ethnicity (hispanic, black, asian, american indian, hawaiian, and multiracial with white as the reference category).  And the parent’s ethnicity (nonwhite 1 and white 0).

After each variable is loaded in, I then need to change all -9's to NA like I did above, because I want -9's as NA’s for imputation later.  Then I use the apply and ifelse inside a function within the apply function to change each data set to one's and zero's while maintaining the NA's because I want to impute those later.

At the end of this section, I am deleting each of the X1_RACETHP_R variables, because I no longer need that variable, since I broke it into the different ethnicities.
```{r}
childGender =cbind(X1_CHSEX_R = data$X_CHSEX_R)
childGender = as.data.frame(childGender)

childGender = apply(childGender, 2, function(x){ifelse(x == -9, NA, x)})
childGender = as.data.frame(childGender)

childGender = ifelse(is.na(childGender), NA, ifelse(childGender == 1, 1,0))
childGender = as.data.frame(childGender)
head(childGender)

childEth = cbind(X1_RACETHP_R = data$X_RACETHP_R)
childEth = apply(childEth, 2, function(x){ifelse(x == -9, NA, x)})
childEth = as.data.frame(childEth)
dim(childEth)

X_HISP_R = ifelse(is.na(childEth), NA, ifelse(childEth == 3,1, ifelse(childEth == 4, 1, 0)))
X_HISP_R = as.data.frame(X_HISP_R)
names(X_HISP_R) = paste0("X", 1:ncol(X_HISP_R),"_HISP_R")

X_BLACK_R = ifelse(is.na(childEth), NA, ifelse(childEth == 2, 1, 0))
X_BLACK_R = as.data.frame(X_BLACK_R)
names(X_BLACK_R) = paste0("X", 1:ncol(X_BLACK_R),"_BLACK_R")


X_ASIAN_R = ifelse(is.na(childEth), NA, ifelse(childEth == 5, 1, 0))
X_ASIAN_R = as.data.frame(X_ASIAN_R)
names(X_ASIAN_R) = paste0("X", 1:ncol(X_ASIAN_R),"_ASIAN_R")


X_AMINAN_R = ifelse(is.na(childEth), NA, ifelse(childEth == 7, 1, 0))
X_AMINAN_R = as.data.frame(X_AMINAN_R)
names(X_AMINAN_R) = paste0("X", 1:ncol(X_AMINAN_R),"_AMINAN_R")


X_HAWPI_R = ifelse(is.na(childEth), NA, ifelse(childEth == 6, 1, 0))
X_HAWPI_R = as.data.frame(X_HAWPI_R)
names(X_AMINAN_R) = paste0("X", 1:ncol(X_AMINAN_R),"_AMINAN_R")


X_MULTR_R = ifelse(is.na(childEth), NA, ifelse(childEth == 8, 1, 0))
X_MULTR_R = as.data.frame(X_MULTR_R)
names(X_MULTR_R) = paste0("X", 1:ncol(X_MULTR_R),"_MULTR_R")


childEth = cbind(X_HISP_R, X_BLACK_R, X_ASIAN_R, X_AMINAN_R, X_HAWPI_R, X_MULTR_R)
childEth = as.data.frame(childEth)

parEth = cbind(X1PAR1RAC = data$X1PAR1RAC)
parEth = apply(parEth, 2, function(x){ifelse(x == -9, NA, x)})
parEth = as.data.frame(parEth)
parEth = ifelse(is.na(parEth), NA, ifelse(parEth == 1, 0,1))
parEth = as.data.frame(parEth)
names(parEth) = paste0("X", 1:ncol(parEth),"PAR1RAC")

data1 = cbind(data1, childGender, childEth, parEth)

data1$X1_RACETHP_R = data1$X2_RACETHP_R = data1$X3_RACETHP_R = data1$X4_RACETHP_R = NULL
head(data1)
```
Here is where I will use Amelia.  Amelia is an R package developed by Gary King at Harvard University.  If a person is missing a data point, Amelia uses information from that person's variables (i.e. demographics, other test scores) to predict what that missing value would have been.  Because there can be variability in inputting or predicting missing data values researchers like King recommend developing at least five data sets containing imputed values and then averaging them together (I average the results together at the end).  To use amelia, you can set x to the data set of interest, m equal to the number of data sets that you want to impute and for non-continuous variables you can use the noms argument if you have nominal variables or you can use logs for count variables.  

The summary function provides information on the algorithm's convergence and the percentage of missing data per variable.

The compare density function compares the density of values for the self control, variable of interest over actual values.  So this function predicts what the nonmissing values would have been using Amelia's algorithm and then compares the density of a specified variable to the actual variable density.  If the density's match, then there is some evidence that the missing value algorithm is at least able to predict nonmissing values, which as close as we can get to estimating how well the algorithm would predict the missing values.  I only focus on the dependent variable self control, since it is the most important variable and there is evidence that the density’s match.

The disperse function evaluates how quickly the chains converged or if they converged at all.  Essentially, if the chains all converge on the same location the algorithm was able to adequately able to find a maximum likelihood (i.e. given the data find the parameter estimates that maximizes the probability that parameter estimates predict the actual values).  

Finally, I wrioe the data sets to csv files, which will be loaded in again for further analyses.
```{r}
library(Amelia)
library(mitools)
m = 5
a.out = amelia(x = data1, m=m, noms = c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI", "X4PUBPRI", "X1_CHSEX_R",  "X1_HISP_R", "X1_BLACK_R", "X1_ASIAN_R", "X1_AMINAN_R", "X1_MULTR_R", "X1PAR1RAC"))


#summary(a.out)
#compare.density(a.out, var = "X1TCHCON", main = "Observed and Imputed values of Self Control")
#disperse(a.out, dims = 1, m = 5)
write.amelia(obj = a.out, file.stem = "ECLSK")
head(data1)
```
Here I am reading back in the imputed data sets.  Then I am creating data sets with only the fall 2010 (i.e. variables with 1's in the beginning of the variable’s name) for each of the imputed data sets.  I am only gathering the first time points for each variable for two reasons.  First, my analysis is an intent to treat (ITT).  ITT in this context means that I am focusing on students starting in the treatment (i.e. an alternative to public school) and not worrying about if they stay in that school.  Given that I have no control over whether students switch or move between schools focusing on students starting in alternative to regular public schools makes sense.  Second, researchers such as Gary King who developed a propensity score matching software have recommended that when using propensity score matching people, students in this case, are matched on pretreatment covariates.  The Fall 2010 is at the beginning of kindergarten therefore it is as close to pretreatment as we can get with this data set.  It makes sense to match students on pretreatment variables, because we want to make sure they start off being similar and we expect that changes, some due to the dependent variable of interest will alter how similar they are over time. 

In the data sets below, the first number in the variable name means that it only contains the first time points for each variable and the second number in each variable name indicates which imputed data set within the first variable data set that I am referring to.  For example, ECLSK11 is the dataset with only the first time points for each variable with the first imputed data set.  
```{r}
# setwd("~/Google Drive/PARCS/Projects/PropScore/Data")
# ECLSK1  = read.csv("ECLSK1.csv", header = TRUE)
# ECLSK1 = na.omit(ECLSK1)
# ECLSK1 = as.data.frame(ECLSK1)
# 
# ECLSK2  = read.csv("ECLSK2.csv", header = TRUE)
# ECLSK2 = na.omit(ECLSK2)
# ECLSK2 = as.data.frame(ECLSK2)
# 
# ECLSK3  = read.csv("ECLSK3.csv", header = TRUE)
# ECLSK3 = na.omit(ECLSK3)
# ECLSK3 = as.data.frame(ECLSK3)
# 
# ECLSK4  = read.csv("ECLSK4.csv", header = TRUE)
# ECLSK4 = na.omit(ECLSK4)
# ECLSK4 = as.data.frame(ECLSK4)
# 
# ECLSK5  = read.csv("ECLSK5.csv", header = TRUE)
# ECLSK5 = na.omit(ECLSK5)
# ECLSK5 = as.data.frame(ECLSK5)

ECLSK11 = cbind(id = ECLSK1$id, X1TCHAPP= ECLSK1$X1TCHAPP, X1TCHCON =ECLSK1$X1TCHCON, X1TCHPER=ECLSK1$X1TCHPER, X1TCHEXT=ECLSK1$X1TCHEXT, X1TCHINT=ECLSK1$X1TCHINT, X1RTHET=ECLSK1$X1RTHET, X1MTHET=ECLSK1$X1MTHET, X1BMI=ECLSK1$X1BMI, X1PUBPRI=ECLSK1$X1PUBPRI, X1_CHSEX_R = ECLSK1$X1_CHSEX_R, X1_HISP_R = ECLSK1$X1_HISP_R, X1_BLACK_R = ECLSK1$X1_BLACK_R, X1_ASIAN_R = ECLSK1$X1_ASIAN_R, X1_AMINAN_R = ECLSK1$X1_AMINAN_R, X1_MULTR_R = ECLSK1$X1_MULTR_R, X1PAR1RAC = ECLSK1$X1PAR1RAC); 

ECLSK12 = cbind(id = ECLSK2$id, X1TCHAPP= ECLSK2$X1TCHAPP, X1TCHCON =ECLSK2$X1TCHCON, X1TCHPER=ECLSK2$X1TCHPER, X1TCHEXT=ECLSK2$X1TCHEXT, X1TCHINT=ECLSK2$X1TCHINT, X1RTHET=ECLSK2$X1RTHET, X1MTHET=ECLSK2$X1MTHET, X1BMI=ECLSK2$X1BMI, X1PUBPRI=ECLSK2$X1PUBPRI, X1_CHSEX_R = ECLSK2$X1_CHSEX_R, X1_HISP_R = ECLSK2$X1_HISP_R, X1_BLACK_R = ECLSK2$X1_BLACK_R, X1_ASIAN_R = ECLSK2$X1_ASIAN_R, X1_AMINAN_R = ECLSK2$X1_AMINAN_R, X1_MULTR_R = ECLSK2$X1_MULTR_R, X1PAR1RAC = ECLSK2$X1PAR1RAC); 

ECLSK13 = cbind(id = ECLSK3$id, X1TCHAPP= ECLSK3$X1TCHAPP, X1TCHCON =ECLSK3$X1TCHCON, X1TCHPER=ECLSK3$X1TCHPER, X1TCHEXT=ECLSK3$X1TCHEXT, X1TCHINT=ECLSK3$X1TCHINT, X1RTHET=ECLSK3$X1RTHET, X1MTHET=ECLSK3$X1MTHET, X1BMI=ECLSK3$X1BMI, X1PUBPRI=ECLSK3$X1PUBPRI, X1_CHSEX_R = ECLSK3$X1_CHSEX_R, X1_HISP_R = ECLSK3$X1_HISP_R, X1_BLACK_R = ECLSK3$X1_BLACK_R, X1_ASIAN_R = ECLSK3$X1_ASIAN_R, X1_AMINAN_R = ECLSK3$X1_AMINAN_R, X1_MULTR_R = ECLSK3$X1_MULTR_R, X1PAR1RAC = ECLSK3$X1PAR1RAC); 

ECLSK14 = cbind(id = ECLSK4$id, X1TCHAPP= ECLSK4$X1TCHAPP, X1TCHCON =ECLSK4$X1TCHCON, X1TCHPER=ECLSK4$X1TCHPER, X1TCHEXT=ECLSK4$X1TCHEXT, X1TCHINT=ECLSK4$X1TCHINT, X1RTHET=ECLSK4$X1RTHET, X1MTHET=ECLSK4$X1MTHET, X1BMI=ECLSK4$X1BMI, X1PUBPRI=ECLSK4$X1PUBPRI, X1_CHSEX_R = ECLSK4$X1_CHSEX_R, X1_HISP_R = ECLSK4$X1_HISP_R, X1_BLACK_R = ECLSK4$X1_BLACK_R, X1_ASIAN_R = ECLSK4$X1_ASIAN_R, X1_AMINAN_R = ECLSK4$X1_AMINAN_R, X1_MULTR_R = ECLSK4$X1_MULTR_R, X1PAR1RAC = ECLSK4$X1PAR1RAC); 

ECLSK15 = cbind(id = ECLSK5$id, X1TCHAPP= ECLSK5$X1TCHAPP, X1TCHCON =ECLSK5$X1TCHCON, X1TCHPER=ECLSK5$X1TCHPER, X1TCHEXT=ECLSK5$X1TCHEXT, X1TCHINT=ECLSK5$X1TCHINT, X1RTHET=ECLSK5$X1RTHET, X1MTHET=ECLSK5$X1MTHET, X1BMI=ECLSK5$X1BMI, X1PUBPRI=ECLSK5$X1PUBPRI, X1_CHSEX_R = ECLSK5$X1_CHSEX_R, X1_HISP_R = ECLSK5$X1_HISP_R, X1_BLACK_R = ECLSK5$X1_BLACK_R, X1_ASIAN_R = ECLSK5$X1_ASIAN_R, X1_AMINAN_R = ECLSK5$X1_AMINAN_R, X1_MULTR_R = ECLSK5$X1_MULTR_R, X1PAR1RAC = ECLSK5$X1PAR1RAC); 
```
Now I need to get rid of the first time points for each variable in the original imputed data sets, because I want to combine the students who are matched on their first time points with their data from other time points later on and not create duplicates.  
```{r}

ECLSK1$X1TCHAPP = ECLSK1$X1TCHCON = ECLSK1$X1TCHPER = ECLSK1$X1TCHEXT= ECLSK1$X1TCHINT= ECLSK1$X1RTHET= ECLSK1$X1MTHET= ECLSK1$X1BMI= ECLSK1$X1PUBPRI = ECLSK1$X1_CHSEX_R= ECLSK1$X1_HISP_R =ECLSK1$X1_BLACK_R=ECLSK1$X1_ASIAN_R=ECLSK1$X1_AMINAN_R=ECLSK1$X1_MULTR_R=ECLSK1$X1PAR1RAC= NULL; head(ECLSK1)
          
ECLSK2$X1TCHAPP = ECLSK2$X1TCHCON = ECLSK2$X1TCHPER = ECLSK2$X1TCHEXT= ECLSK2$X1TCHINT= ECLSK2$X1RTHET= ECLSK2$X1MTHET= ECLSK2$X1BMI= ECLSK2$X1PUBPRI = ECLSK2$X1_CHSEX_R= ECLSK2$X1_HISP_R =ECLSK2$X1_BLACK_R=ECLSK2$X1_ASIAN_R=ECLSK2$X1_AMINAN_R=ECLSK2$X1_MULTR_R=ECLSK2$X1PAR1RAC= NULL; head(ECLSK2)

ECLSK3$X1TCHAPP = ECLSK3$X1TCHCON = ECLSK3$X1TCHPER = ECLSK3$X1TCHEXT= ECLSK3$X1TCHINT= ECLSK3$X1RTHET= ECLSK3$X1MTHET= ECLSK3$X1BMI= ECLSK3$X1PUBPRI= ECLSK3$X1_CHSEX_R= ECLSK3$X1_HISP_R =ECLSK3$X1_BLACK_R=ECLSK3$X1_ASIAN_R=ECLSK3$X1_AMINAN_R=ECLSK3$X1_MULTR_R=ECLSK3$X1PAR1RAC= NULL; head(ECLSK3)

ECLSK4$X1TCHAPP = ECLSK4$X1TCHCON = ECLSK4$X1TCHPER = ECLSK4$X1TCHEXT= ECLSK4$X1TCHINT= ECLSK4$X1RTHET= ECLSK4$X1MTHET= ECLSK4$X1BMI= ECLSK4$X1PUBPRI = ECLSK4$X1_CHSEX_R= ECLSK4$X1_HISP_R =ECLSK4$X1_BLACK_R=ECLSK4$X1_ASIAN_R=ECLSK4$X1_AMINAN_R=ECLSK4$X1_MULTR_R=ECLSK4$X1PAR1RAC=NULL; head(ECLSK4)

ECLSK5$X1TCHAPP = ECLSK5$X1TCHCON = ECLSK5$X1TCHPER = ECLSK5$X1TCHEXT= ECLSK5$X1TCHINT= ECLSK5$X1RTHET= ECLSK5$X1MTHET= ECLSK5$X1BMI= ECLSK5$X1PUBPRI = ECLSK5$X1_CHSEX_R= ECLSK5$X1_HISP_R =ECLSK5$X1_BLACK_R=ECLSK5$X1_ASIAN_R=ECLSK5$X1_AMINAN_R=ECLSK5$X1_MULTR_R=ECLSK5$X1PAR1RAC= NULL; head(ECLSK5)
```
Now we are analyzing the data with only the first time points for each student.  We are matching students on their fall 2010 characteristics included in the model below which are creating scores for how likely a student is to fall into the treatment category.  Then matchit finds those students in the control group that have the highest likelihood to be in the treatment (i.e. highest propensity scores) and matches them with a student with a similar propensity score in the treatment group.  Propensity score matching allows us to say that students in the treatment and control are at least similar on the included pretreatment covariates, because matchit will select students for a final data set that similar on characteristics.  In this model, I am using the nearest neighbor algorithm where I am matching each student from the treatment (i.e. alternative to assigned public school) to a control student (i.e. assigned public school).  Then, for the first data, remember there are five imputed datasets, I examine the jitter plot which shows the distribution of matched students in the treatment and control.  In this example the two almost exactly align providing a good sign that I have students that are very similar on the included covariates.  Next I look at a histogram of the matched students from the treatment and control groups and again the distributions of propensity scores seems almost identical providing more evidence that students in the treatment and control are very similar on included covariates.

Then I need to compile a dataset with the weights, which include whether a student was matched or not, the treatment, which is whether or not the student is in an alternative to assigned public school or not, and only select those students who were matched (i.e. if they have a 1 on the weight variable). 
```{r}
library(MatchIt)

ECLSK11 = as.data.frame(ECLSK11)
m.out1 = matchit(X1PUBPRI ~ X1TCHAPP + X1TCHCON + X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET + X1BMI +X1_CHSEX_R + X1_HISP_R + X1_BLACK_R + X1_ASIAN_R + X1_AMINAN_R + X1_MULTR_R + X1PAR1RAC, data = ECLSK11, method = "nearest", ratio = 1)

#plot(m.out1, type = "jitter")
#plot(m.out1, type = "hist")


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
ECLSK12 = as.data.frame(ECLSK12)

m.out2 = matchit(X1PUBPRI ~ X1TCHAPP + X1TCHCON + X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET + X1BMI +X1_CHSEX_R + X1_HISP_R + X1_BLACK_R + X1_ASIAN_R + X1_AMINAN_R + X1_MULTR_R + X1PAR1RAC, data = ECLSK12, method = "nearest", ratio = 1)


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
ECLSK13 = as.data.frame(ECLSK13)

m.out3 = matchit(X1PUBPRI ~ X1TCHAPP + X1TCHCON + X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET + X1BMI +X1_CHSEX_R + X1_HISP_R + X1_BLACK_R + X1_ASIAN_R + X1_AMINAN_R + X1_MULTR_R + X1PAR1RAC, data = ECLSK13, method = "nearest", ratio = 1)


m.out3Data = as.data.frame(cbind(m.out3$X, weights = m.out3$weights, X1PUBPRI = m.out3$treat))
m.out3Data = m.out3Data[which(m.out3Data$weights == 1),]
head(m.out3Data)

write.csv(m.out3Data, "m.out3Data.csv")
m.out3Data = read.csv("m.out3Data.csv", header = TRUE)
head(m.out3Data)
colnames(m.out3Data)[1] = c("id")
m.out1

```
Repeat the process above for data set four.
```{r}
ECLSK14 = as.data.frame(ECLSK14)

m.out4 = matchit(X1PUBPRI ~ X1TCHAPP + X1TCHCON + X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET + X1BMI +X1_CHSEX_R + X1_HISP_R + X1_BLACK_R + X1_ASIAN_R + X1_AMINAN_R + X1_MULTR_R + X1PAR1RAC, data = ECLSK14, method = "nearest", ratio = 1)


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
ECLSK15 = as.data.frame(ECLSK15)

m.out5 = matchit(X1PUBPRI ~ X1TCHAPP + X1TCHCON + X1TCHPER + X1TCHEXT + X1TCHINT + X1RTHET + X1MTHET + X1BMI +X1_CHSEX_R + X1_HISP_R + X1_BLACK_R + X1_ASIAN_R + X1_AMINAN_R + X1_MULTR_R + X1PAR1RAC, data = ECLSK15, method = "nearest", ratio = 1)


m.out5Data = as.data.frame(cbind(m.out5$X, weights = m.out5$weights, X1PUBPRI = m.out5$treat))
m.out5Data = m.out5Data[which(m.out5Data$weights == 1),]
head(m.out5Data)

write.csv(m.out5Data, "m.out5Data.csv")
m.out5Data = read.csv("m.out5Data.csv", header = TRUE)
head(m.out5Data)
colnames(m.out5Data)[1] = c("id")
```
Now I need to gather the final sample.  The final sample size is difficult, because the number of control and treatment units differs, because in the imputation process in some cases codes missing values for public and private schools was different for different datas.  Therefore, the number of private schools varied slightly meaning that total number sample size differs depending on how many missing value in the public private variable were coded as private (each private school is matched with one public school so the number of students in private school is the final sample size).

Therefore, I took the mean of the total sample from the matched control (i.e. private) units for each of the five data sets.  I then calculated the standard deviation for each of them as well.
```{r}
n = t(cbind(m.out1$nn[1,2], m.out2$nn[1,2], m.out3$nn[1,2], m.out4$nn[1,2], m.out5$nn[1,2]))
nMean = round(mean(n),0); nMean
nSD = round(sd(n),0); nSD
```
Now I need to get the means and sds for the private and public students.  Need to create two data sets one with private and one with public. Then get means and sds for each group. 
```{r}
m.out1DataPri = m.out1Data[which(m.out1Data$X1PUBPRI == 1),]
m.out2DataPri = m.out2Data[which(m.out2Data$X1PUBPRI == 1),]
m.out3DataPri = m.out3Data[which(m.out3Data$X1PUBPRI == 1),]
m.out4DataPri = m.out4Data[which(m.out4Data$X1PUBPRI == 1),]
m.out5DataPri = m.out5Data[which(m.out5Data$X1PUBPRI == 1),]


mean1Pri =apply(m.out1DataPri, 2, mean)
mean2Pri =apply(m.out2DataPri, 2, mean)
mean3Pri =apply(m.out3DataPri, 2, mean)
mean4Pri =apply(m.out4DataPri, 2, mean)
mean5Pri =apply(m.out5DataPri, 2, mean)

sd1Pri = apply(m.out1DataPri, 2, sd)
sd2Pri = apply(m.out2DataPri, 2, sd)
sd3Pri = apply(m.out3DataPri, 2, sd)
sd4Pri = apply(m.out4DataPri, 2, sd)
sd5Pri = apply(m.out5DataPri, 2, sd)


allMeansPri = t(as.matrix(cbind(mean1Pri, mean2Pri, mean3Pri, mean4Pri, mean5Pri)))

allSDsPri = t(as.matrix(cbind(sd1Pri, sd2Pri, sd3Pri, sd4Pri, sd5Pri)))

allMeansSDsComPri = mi.meld(q = allMeansPri, se = allSDsPri)
allMeansSDsComPri = as.data.frame(allMeansSDsComPri)
write.csv(allMeansSDsComPri, "allMeansSDsComPri.csv")


m.out1DataPub = m.out1Data[which(m.out1Data$X1PUBPRI == 0),]
m.out2DataPub = m.out2Data[which(m.out2Data$X1PUBPRI == 0),]
m.out3DataPub = m.out3Data[which(m.out3Data$X1PUBPRI == 0),]
m.out4DataPub = m.out4Data[which(m.out4Data$X1PUBPRI == 0),]
m.out5DataPub = m.out5Data[which(m.out5Data$X1PUBPRI == 0),]

mean1Pub =apply(m.out1DataPub, 2, mean)
mean2Pub =apply(m.out2DataPub, 2, mean)
mean3Pub =apply(m.out3DataPub, 2, mean)
mean4Pub =apply(m.out4DataPub, 2, mean)
mean5Pub =apply(m.out5DataPub, 2, mean)

sd1Pub = apply(m.out1DataPub, 2, sd)
sd2Pub = apply(m.out2DataPub, 2, sd)
sd3Pub = apply(m.out3DataPub, 2, sd)
sd4Pub = apply(m.out4DataPub, 2, sd)
sd5Pub = apply(m.out5DataPub, 2, sd)

allMeansPub = t(as.matrix(cbind(mean1Pub, mean2Pub, mean3Pub, mean4Pub, mean5Pub)))

allSDsPub = t(as.matrix(cbind(sd1Pub, sd2Pub, sd3Pub, sd4Pub, sd5Pub)))

allMeansSDsComPub = mi.meld(q = allMeansPub, se = allSDsPub)
allMeansSDsComPub = as.data.frame(allMeansSDsComPub)
write.csv(allMeansSDsComPub, "allMeansSDsComPub.csv")
```


Now I need to conduct an inner join with the data set that has time points 2 through 4 with the matched data set which has the time point 1 variable.  This is pretty simple with the merge function in r, which by default with conduct an inner join (i.e. only include in the data set those id values that are present in both data sets).
```{r}
ECLSK1 = merge(ECLSK1, m.out1Data, by = c("id"))
ECLSK2 = merge(ECLSK2, m.out2Data, by = c("id"))
ECLSK3 = merge(ECLSK3, m.out3Data, by = c("id"))
ECLSK4 = merge(ECLSK4, m.out4Data, by = c("id"))
ECLSK5 = merge(ECLSK5, m.out5Data, by = c("id"))
```
Next, because I am including several fixed variables (i.e. variables that do not change over time),  I need to create those values for time points 2 through 4.  Therefore, I replicate each column three times and then use the paste0 function to produce names that go from 1 to the number of columns in the data set, so 1 through 4.  I need to repeat this process five times once for each imputed data set. 
 
```{r}
X1_CHSEX_R = data.frame(ECLSK1$X1_CHSEX_R)
colnames(X1_CHSEX_R) = c("X1_CHSEX_R")
X1_CHSEX_R = cbind(X1_CHSEX_R, replicate(3, X1_CHSEX_R$X1_CHSEX_R))
names(X1_CHSEX_R) = paste0("X", 1:ncol(X1_CHSEX_R),"_CHSEX_R")


X1_HISP_R = data.frame(ECLSK1$X1_HISP_R)
colnames(X1_HISP_R) = c("X1_HISP_R")
X1_HISP_R = cbind(X1_HISP_R, replicate(3, X1_HISP_R$X1_HISP_R))
names(X1_HISP_R) = paste0("X", 1:ncol(X1_HISP_R),"_HISP_R")


X1_BLACK_R = data.frame(ECLSK1$X1_BLACK_R)
colnames(X1_BLACK_R) = c("X1_BLACK_R")
X1_BLACK_R = cbind(X1_BLACK_R, replicate(3, X1_BLACK_R$X1_BLACK_R))
names(X1_BLACK_R) = paste0("X", 1:ncol(X1_BLACK_R),"_BLACK_R")


X1_ASIAN_R = data.frame(ECLSK1$X1_ASIAN_R)
colnames(X1_ASIAN_R) = c("X1_ASIAN_R")
X1_ASIAN_R = cbind(X1_ASIAN_R, replicate(3, X1_ASIAN_R$X1_ASIAN_R))
names(X1_ASIAN_R) = paste0("X", 1:ncol(X1_ASIAN_R),"_ASIAN_R")


X1_AMINAN_R = data.frame(ECLSK1$X1_AMINAN_R)
colnames(X1_AMINAN_R) = c("X1_AMINAN_R")
X1_AMINAN_R = cbind(X1_AMINAN_R, replicate(3, X1_AMINAN_R$X1_AMINAN_R))
names(X1_AMINAN_R) = paste0("X", 1:ncol(X1_AMINAN_R),"_AMINAN_R")


X1_MULTR_R = data.frame(ECLSK1$X1_MULTR_R)
colnames(X1_MULTR_R) = c("X1_MULTR_R")
X1_MULTR_R = cbind(X1_MULTR_R, replicate(3, X1_MULTR_R$X1_MULTR_R))
names(X1_MULTR_R) = paste0("X", 1:ncol(X1_MULTR_R),"_MULTR_R")


X1PAR1RAC = data.frame(ECLSK1$X1PAR1RAC)
colnames(X1PAR1RAC) = c("X1PAR1RAC")
X1PAR1RAC = cbind(X1PAR1RAC, replicate(3, X1PAR1RAC$X1PAR1RAC))
names(X1PAR1RAC) = paste0("X", 1:ncol(X1PAR1RAC),"PAR1RAC")


ECLSK1 = cbind(ECLSK1, X1_CHSEX_R, X1_HISP_R, X1_BLACK_R, X1_ASIAN_R, X1_AMINAN_R, X1_MULTR_R, X1PAR1RAC)

X1_CHSEX_R = data.frame(ECLSK2$X1_CHSEX_R)
colnames(X1_CHSEX_R) = c("X1_CHSEX_R")
X1_CHSEX_R = cbind(X1_CHSEX_R, replicate(3, X1_CHSEX_R$X1_CHSEX_R))
names(X1_CHSEX_R) = paste0("X", 1:ncol(X1_CHSEX_R),"_CHSEX_R")


X1_HISP_R = data.frame(ECLSK2$X1_HISP_R)
colnames(X1_HISP_R) = c("X1_HISP_R")
X1_HISP_R = cbind(X1_HISP_R, replicate(3, X1_HISP_R$X1_HISP_R))
names(X1_HISP_R) = paste0("X", 1:ncol(X1_HISP_R),"_HISP_R")


X1_BLACK_R = data.frame(ECLSK2$X1_BLACK_R)
colnames(X1_BLACK_R) = c("X1_BLACK_R")
X1_BLACK_R = cbind(X1_BLACK_R, replicate(3, X1_BLACK_R$X1_BLACK_R))
names(X1_BLACK_R) = paste0("X", 1:ncol(X1_BLACK_R),"_BLACK_R")


X1_ASIAN_R = data.frame(ECLSK2$X1_ASIAN_R)
colnames(X1_ASIAN_R) = c("X1_ASIAN_R")
X1_ASIAN_R = cbind(X1_ASIAN_R, replicate(3, X1_ASIAN_R$X1_ASIAN_R))
names(X1_ASIAN_R) = paste0("X", 1:ncol(X1_ASIAN_R),"_ASIAN_R")


X1_AMINAN_R = data.frame(ECLSK2$X1_AMINAN_R)
colnames(X1_AMINAN_R) = c("X1_AMINAN_R")
X1_AMINAN_R = cbind(X1_AMINAN_R, replicate(3, X1_AMINAN_R$X1_AMINAN_R))
names(X1_AMINAN_R) = paste0("X", 1:ncol(X1_AMINAN_R),"_AMINAN_R")


X1_MULTR_R = data.frame(ECLSK2$X1_MULTR_R)
colnames(X1_MULTR_R) = c("X1_MULTR_R")
X1_MULTR_R = cbind(X1_MULTR_R, replicate(3, X1_MULTR_R$X1_MULTR_R))
names(X1_MULTR_R) = paste0("X", 1:ncol(X1_MULTR_R),"_MULTR_R")


X1PAR1RAC = data.frame(ECLSK2$X1PAR1RAC)
colnames(X1PAR1RAC) = c("X1PAR1RAC")
X1PAR1RAC = cbind(X1PAR1RAC, replicate(3, X1PAR1RAC$X1PAR1RAC))
names(X1PAR1RAC) = paste0("X", 1:ncol(X1PAR1RAC),"PAR1RAC")


ECLSK2 = cbind(ECLSK2, X1_CHSEX_R, X1_HISP_R, X1_BLACK_R, X1_ASIAN_R, X1_AMINAN_R, X1_MULTR_R, X1PAR1RAC)

X1_CHSEX_R = data.frame(ECLSK3$X1_CHSEX_R)
colnames(X1_CHSEX_R) = c("X1_CHSEX_R")
X1_CHSEX_R = cbind(X1_CHSEX_R, replicate(3, X1_CHSEX_R$X1_CHSEX_R))
names(X1_CHSEX_R) = paste0("X", 1:ncol(X1_CHSEX_R),"_CHSEX_R")


X1_HISP_R = data.frame(ECLSK3$X1_HISP_R)
colnames(X1_HISP_R) = c("X1_HISP_R")
X1_HISP_R = cbind(X1_HISP_R, replicate(3, X1_HISP_R$X1_HISP_R))
names(X1_HISP_R) = paste0("X", 1:ncol(X1_HISP_R),"_HISP_R")


X1_BLACK_R = data.frame(ECLSK3$X1_BLACK_R)
colnames(X1_BLACK_R) = c("X1_BLACK_R")
X1_BLACK_R = cbind(X1_BLACK_R, replicate(3, X1_BLACK_R$X1_BLACK_R))
names(X1_BLACK_R) = paste0("X", 1:ncol(X1_BLACK_R),"_BLACK_R")


X1_ASIAN_R = data.frame(ECLSK3$X1_ASIAN_R)
colnames(X1_ASIAN_R) = c("X1_ASIAN_R")
X1_ASIAN_R = cbind(X1_ASIAN_R, replicate(3, X1_ASIAN_R$X1_ASIAN_R))
names(X1_ASIAN_R) = paste0("X", 1:ncol(X1_ASIAN_R),"_ASIAN_R")


X1_AMINAN_R = data.frame(ECLSK3$X1_AMINAN_R)
colnames(X1_AMINAN_R) = c("X1_AMINAN_R")
X1_AMINAN_R = cbind(X1_AMINAN_R, replicate(3, X1_AMINAN_R$X1_AMINAN_R))
names(X1_AMINAN_R) = paste0("X", 1:ncol(X1_AMINAN_R),"_AMINAN_R")


X1_MULTR_R = data.frame(ECLSK3$X1_MULTR_R)
colnames(X1_MULTR_R) = c("X1_MULTR_R")
X1_MULTR_R = cbind(X1_MULTR_R, replicate(3, X1_MULTR_R$X1_MULTR_R))
names(X1_MULTR_R) = paste0("X", 1:ncol(X1_MULTR_R),"_MULTR_R")


X1PAR1RAC = data.frame(ECLSK3$X1PAR1RAC)
colnames(X1PAR1RAC) = c("X1PAR1RAC")
X1PAR1RAC = cbind(X1PAR1RAC, replicate(3, X1PAR1RAC$X1PAR1RAC))
names(X1PAR1RAC) = paste0("X", 1:ncol(X1PAR1RAC),"PAR1RAC")


ECLSK3 = cbind(ECLSK3, X1_CHSEX_R, X1_HISP_R, X1_BLACK_R, X1_ASIAN_R, X1_AMINAN_R, X1_MULTR_R, X1PAR1RAC)

X1_CHSEX_R = data.frame(ECLSK4$X1_CHSEX_R)
colnames(X1_CHSEX_R) = c("X1_CHSEX_R")
X1_CHSEX_R = cbind(X1_CHSEX_R, replicate(3, X1_CHSEX_R$X1_CHSEX_R))
names(X1_CHSEX_R) = paste0("X", 1:ncol(X1_CHSEX_R),"_CHSEX_R")


X1_HISP_R = data.frame(ECLSK4$X1_HISP_R)
colnames(X1_HISP_R) = c("X1_HISP_R")
X1_HISP_R = cbind(X1_HISP_R, replicate(3, X1_HISP_R$X1_HISP_R))
names(X1_HISP_R) = paste0("X", 1:ncol(X1_HISP_R),"_HISP_R")


X1_BLACK_R = data.frame(ECLSK4$X1_BLACK_R)
colnames(X1_BLACK_R) = c("X1_BLACK_R")
X1_BLACK_R = cbind(X1_BLACK_R, replicate(3, X1_BLACK_R$X1_BLACK_R))
names(X1_BLACK_R) = paste0("X", 1:ncol(X1_BLACK_R),"_BLACK_R")


X1_ASIAN_R = data.frame(ECLSK4$X1_ASIAN_R)
colnames(X1_ASIAN_R) = c("X1_ASIAN_R")
X1_ASIAN_R = cbind(X1_ASIAN_R, replicate(3, X1_ASIAN_R$X1_ASIAN_R))
names(X1_ASIAN_R) = paste0("X", 1:ncol(X1_ASIAN_R),"_ASIAN_R")


X1_AMINAN_R = data.frame(ECLSK4$X1_AMINAN_R)
colnames(X1_AMINAN_R) = c("X1_AMINAN_R")
X1_AMINAN_R = cbind(X1_AMINAN_R, replicate(3, X1_AMINAN_R$X1_AMINAN_R))
names(X1_AMINAN_R) = paste0("X", 1:ncol(X1_AMINAN_R),"_AMINAN_R")


X1_MULTR_R = data.frame(ECLSK4$X1_MULTR_R)
colnames(X1_MULTR_R) = c("X1_MULTR_R")
X1_MULTR_R = cbind(X1_MULTR_R, replicate(3, X1_MULTR_R$X1_MULTR_R))
names(X1_MULTR_R) = paste0("X", 1:ncol(X1_MULTR_R),"_MULTR_R")


X1PAR1RAC = data.frame(ECLSK4$X1PAR1RAC)
colnames(X1PAR1RAC) = c("X1PAR1RAC")
X1PAR1RAC = cbind(X1PAR1RAC, replicate(3, X1PAR1RAC$X1PAR1RAC))
names(X1PAR1RAC) = paste0("X", 1:ncol(X1PAR1RAC),"PAR1RAC")


ECLSK4 = cbind(ECLSK4, X1_CHSEX_R, X1_HISP_R, X1_BLACK_R, X1_ASIAN_R, X1_AMINAN_R, X1_MULTR_R, X1PAR1RAC)

X1_CHSEX_R = data.frame(ECLSK5$X1_CHSEX_R)
colnames(X1_CHSEX_R) = c("X1_CHSEX_R")
X1_CHSEX_R = cbind(X1_CHSEX_R, replicate(3, X1_CHSEX_R$X1_CHSEX_R))
names(X1_CHSEX_R) = paste0("X", 1:ncol(X1_CHSEX_R),"_CHSEX_R")


X1_HISP_R = data.frame(ECLSK5$X1_HISP_R)
colnames(X1_HISP_R) = c("X1_HISP_R")
X1_HISP_R = cbind(X1_HISP_R, replicate(3, X1_HISP_R$X1_HISP_R))
names(X1_HISP_R) = paste0("X", 1:ncol(X1_HISP_R),"_HISP_R")


X1_BLACK_R = data.frame(ECLSK5$X1_BLACK_R)
colnames(X1_BLACK_R) = c("X1_BLACK_R")
X1_BLACK_R = cbind(X1_BLACK_R, replicate(3, X1_BLACK_R$X1_BLACK_R))
names(X1_BLACK_R) = paste0("X", 1:ncol(X1_BLACK_R),"_BLACK_R")


X1_ASIAN_R = data.frame(ECLSK5$X1_ASIAN_R)
colnames(X1_ASIAN_R) = c("X1_ASIAN_R")
X1_ASIAN_R = cbind(X1_ASIAN_R, replicate(3, X1_ASIAN_R$X1_ASIAN_R))
names(X1_ASIAN_R) = paste0("X", 1:ncol(X1_ASIAN_R),"_ASIAN_R")


X1_AMINAN_R = data.frame(ECLSK5$X1_AMINAN_R)
colnames(X1_AMINAN_R) = c("X1_AMINAN_R")
X1_AMINAN_R = cbind(X1_AMINAN_R, replicate(3, X1_AMINAN_R$X1_AMINAN_R))
names(X1_AMINAN_R) = paste0("X", 1:ncol(X1_AMINAN_R),"_AMINAN_R")


X1_MULTR_R = data.frame(ECLSK5$X1_MULTR_R)
colnames(X1_MULTR_R) = c("X1_MULTR_R")
X1_MULTR_R = cbind(X1_MULTR_R, replicate(3, X1_MULTR_R$X1_MULTR_R))
names(X1_MULTR_R) = paste0("X", 1:ncol(X1_MULTR_R),"_MULTR_R")


X1PAR1RAC = data.frame(ECLSK5$X1PAR1RAC)
colnames(X1PAR1RAC) = c("X1PAR1RAC")
X1PAR1RAC = cbind(X1PAR1RAC, replicate(3, X1PAR1RAC$X1PAR1RAC))
names(X1PAR1RAC) = paste0("X", 1:ncol(X1PAR1RAC),"PAR1RAC")


ECLSK5 = cbind(ECLSK5, X1_CHSEX_R, X1_HISP_R, X1_BLACK_R, X1_ASIAN_R, X1_AMINAN_R, X1_MULTR_R, X1PAR1RAC)
```
Here I am going to grab the means and standard deviations across the variables across time points.
```{r}
mean1 =apply(ECLSK1, 2, mean)
mean2 =apply(ECLSK2, 2, mean)
mean3 =apply(ECLSK3, 2, mean)
mean4 =apply(ECLSK4, 2, mean)
mean5 =apply(ECLSK5, 2, mean)

sd1 = apply(ECLSK1, 2, sd)
sd2 = apply(ECLSK2, 2, sd)
sd3 = apply(ECLSK3, 2, sd)
sd4 = apply(ECLSK4, 2, sd)
sd5 = apply(ECLSK5, 2, sd)


allMeans = t(as.matrix(cbind(mean1, mean2, mean3, mean4, mean5)))

allSDs = t(as.matrix(cbind(sd1, sd2, sd3, sd4, sd5)))

allMeansSDsCom = mi.meld(q = allMeans, se = allSDs)
allMeansSDsCom = as.data.frame(allMeansSDsCom)
write.csv(allMeansSDsCom, "allMeansSDsCom.csv")
```



Here I can create transform my cross sectional data set into a longitudinal data set, where instead of having four columns for each variable, I have one column for each variable with a time variable that indicates, which time point each response is aligned with.  I do this for each of the five imputed datasets.

Here overwrite the X1PUBPRI indicator to be the same for each year.
```{r}

ECLSK1= reshape(ECLSK1, varying = list(c("X1TCHAPP", "X2TCHAPP", "X3TCHAPP", "X4TCHAPP"), c("X1TCHCON", "X2TCHCON", "X3TCHCON", "X4TCHCON"), c("X1TCHPER", "X2TCHPER", "X3TCHPER", "X4TCHPER"), c("X1TCHEXT", "X2TCHEXT", "X3TCHEXT", "X4TCHEXT"), c("X1TCHINT", "X2TCHINT", "X3TCHINT", "X4TCHINT"), c("X1RTHET", "X2RTHET", "X3RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X3MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X3BMI", "X4BMI"), c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI","X4PUBPRI"), c("X1_CHSEX_R", "X2_CHSEX_R", "X3_CHSEX_R", "X4_CHSEX_R"), c("X1_HISP_R", "X2_HISP_R", "X3_HISP_R", "X4_HISP_R"), c("X1_BLACK_R", "X2_BLACK_R", "X3_BLACK_R", "X4_BLACK_R"), c("X1_ASIAN_R", "X2_ASIAN_R", "X3_ASIAN_R", "X4_ASIAN_R"), c("X1_AMINAN_R", "X2_AMINAN_R", "X3_AMINAN_R", "X4_AMINAN_R"), c("X1_MULTR_R", "X2_MULTR_R", "X3_MULTR_R", "X4_MULTR_R"), c("X1PAR1RAC", "X2PAR1RAC", "X3PAR1RAC", "X4PAR1RAC")), times = c(0,1,2,3), direction = "long")

ECLSK2= reshape(ECLSK2, varying = list(c("X1TCHAPP", "X2TCHAPP", "X3TCHAPP", "X4TCHAPP"), c("X1TCHCON", "X2TCHCON", "X3TCHCON", "X4TCHCON"), c("X1TCHPER", "X2TCHPER", "X3TCHPER", "X4TCHPER"), c("X1TCHEXT", "X2TCHEXT", "X3TCHEXT", "X4TCHEXT"), c("X1TCHINT", "X2TCHINT", "X3TCHINT", "X4TCHINT"), c("X1RTHET", "X2RTHET", "X3RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X3MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X3BMI", "X4BMI"), c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI","X4PUBPRI"), c("X1_CHSEX_R", "X2_CHSEX_R", "X3_CHSEX_R", "X4_CHSEX_R"), c("X1_HISP_R", "X2_HISP_R", "X3_HISP_R", "X4_HISP_R"), c("X1_BLACK_R", "X2_BLACK_R", "X3_BLACK_R", "X4_BLACK_R"), c("X1_ASIAN_R", "X2_ASIAN_R", "X3_ASIAN_R", "X4_ASIAN_R"), c("X1_AMINAN_R", "X2_AMINAN_R", "X3_AMINAN_R", "X4_AMINAN_R"), c("X1_MULTR_R", "X2_MULTR_R", "X3_MULTR_R", "X4_MULTR_R"), c("X1PAR1RAC", "X2PAR1RAC", "X3PAR1RAC", "X4PAR1RAC")), times = c(0,1,2,3), direction = "long")

ECLSK3= reshape(ECLSK3, varying = list(c("X1TCHAPP", "X2TCHAPP", "X3TCHAPP", "X4TCHAPP"), c("X1TCHCON", "X2TCHCON", "X3TCHCON", "X4TCHCON"), c("X1TCHPER", "X2TCHPER", "X3TCHPER", "X4TCHPER"), c("X1TCHEXT", "X2TCHEXT", "X3TCHEXT", "X4TCHEXT"), c("X1TCHINT", "X2TCHINT", "X3TCHINT", "X4TCHINT"), c("X1RTHET", "X2RTHET", "X3RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X3MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X3BMI", "X4BMI"), c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI","X4PUBPRI"), c("X1_CHSEX_R", "X2_CHSEX_R", "X3_CHSEX_R", "X4_CHSEX_R"), c("X1_HISP_R", "X2_HISP_R", "X3_HISP_R", "X4_HISP_R"), c("X1_BLACK_R", "X2_BLACK_R", "X3_BLACK_R", "X4_BLACK_R"), c("X1_ASIAN_R", "X2_ASIAN_R", "X3_ASIAN_R", "X4_ASIAN_R"), c("X1_AMINAN_R", "X2_AMINAN_R", "X3_AMINAN_R", "X4_AMINAN_R"), c("X1_MULTR_R", "X2_MULTR_R", "X3_MULTR_R", "X4_MULTR_R"), c("X1PAR1RAC", "X2PAR1RAC", "X3PAR1RAC", "X4PAR1RAC")), times = c(0,1,2,3), direction = "long")

ECLSK4= reshape(ECLSK4, varying = list(c("X1TCHAPP", "X2TCHAPP", "X3TCHAPP", "X4TCHAPP"), c("X1TCHCON", "X2TCHCON", "X3TCHCON", "X4TCHCON"), c("X1TCHPER", "X2TCHPER", "X3TCHPER", "X4TCHPER"), c("X1TCHEXT", "X2TCHEXT", "X3TCHEXT", "X4TCHEXT"), c("X1TCHINT", "X2TCHINT", "X3TCHINT", "X4TCHINT"), c("X1RTHET", "X2RTHET", "X3RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X3MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X3BMI", "X4BMI"), c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI","X4PUBPRI"), c("X1_CHSEX_R", "X2_CHSEX_R", "X3_CHSEX_R", "X4_CHSEX_R"), c("X1_HISP_R", "X2_HISP_R", "X3_HISP_R", "X4_HISP_R"), c("X1_BLACK_R", "X2_BLACK_R", "X3_BLACK_R", "X4_BLACK_R"), c("X1_ASIAN_R", "X2_ASIAN_R", "X3_ASIAN_R", "X4_ASIAN_R"), c("X1_AMINAN_R", "X2_AMINAN_R", "X3_AMINAN_R", "X4_AMINAN_R"), c("X1_MULTR_R", "X2_MULTR_R", "X3_MULTR_R", "X4_MULTR_R"), c("X1PAR1RAC", "X2PAR1RAC", "X3PAR1RAC", "X4PAR1RAC")), times = c(0,1,2,3), direction = "long")

ECLSK5= reshape(ECLSK5, varying = list(c("X1TCHAPP", "X2TCHAPP", "X3TCHAPP", "X4TCHAPP"), c("X1TCHCON", "X2TCHCON", "X3TCHCON", "X4TCHCON"), c("X1TCHPER", "X2TCHPER", "X3TCHPER", "X4TCHPER"), c("X1TCHEXT", "X2TCHEXT", "X3TCHEXT", "X4TCHEXT"), c("X1TCHINT", "X2TCHINT", "X3TCHINT", "X4TCHINT"), c("X1RTHET", "X2RTHET", "X3RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X3MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X3BMI", "X4BMI"), c("X1PUBPRI", "X2PUBPRI", "X3PUBPRI","X4PUBPRI"), c("X1_CHSEX_R", "X2_CHSEX_R", "X3_CHSEX_R", "X4_CHSEX_R"), c("X1_HISP_R", "X2_HISP_R", "X3_HISP_R", "X4_HISP_R"), c("X1_BLACK_R", "X2_BLACK_R", "X3_BLACK_R", "X4_BLACK_R"), c("X1_ASIAN_R", "X2_ASIAN_R", "X3_ASIAN_R", "X4_ASIAN_R"), c("X1_AMINAN_R", "X2_AMINAN_R", "X3_AMINAN_R", "X4_AMINAN_R"), c("X1_MULTR_R", "X2_MULTR_R", "X3_MULTR_R", "X4_MULTR_R"), c("X1PAR1RAC", "X2PAR1RAC", "X3PAR1RAC", "X4PAR1RAC")), times = c(0,1,2,3), direction = "long")

```
Equations 1.1 through 1.3 describe the multilevel model.  First there is level one, which has a intercept Beta(0j) and the variables of interest  Beta(1j) which is the coefficient for the time variable and  Beta(2j) is the coefficient for the binary variable indicating whether a student is in private (1) or public (0) school at each time point (i) over each person (j).  The i's in time indicate each time point, which are nested in each person j where j can vary across each person's intercept and slope, which are described in equations 1.2 and 1.3.  Beta(1j) interacts with Beta(2j) and is the variable of interest and represents whether or not students are in a private school and are different from public school students on teacher's reported self control scores for students over time.  Then there is Beta(xj)(X), where X represents a vector of time varying covariates.  Finally there is the individual error term e(ij), which is the difference between the predicted and observed value for each person over each time point.  

Next, the intercept is broken down into the different factors influencing its value for each student.  Beta(0j) represents the average intercept value and  Beta(0j)(Q) represents the level two time invariant covariates including the in the model where Q is a vector of time invariant variables.  Then there is the individual variation that each student has from the average intercept value represented by u0j allowing each student to have their intercept value.  

Finally, there is the random slope component, which allows each student (i) to have their own trajectory or slope over self control over the vector of time varying variables X across time j.  The X’s represent the same vector of beta’s  from the level one equation, but are broken down into two components an average slope coefficient and a unique error term (uxj) that allows each time varying beta to be different for each time point across self control scores.  


Level one covariates X1TCHAPP, X1TCHPER, X1TCHEXT, X1TCHINT, X1RTHET, X1MTHET, X1BMI, X1PUBPRI

Level two variables: X1_CHSEX_R, X1_HISP_R, X1_BLACK_R, X1_ASIAN_R, X1_AMINAN_R, X1_MULTR_R, X1PAR1RAC

$$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{1j}Time_{ij} + \beta_{xj}X_{ij} + e_{ij}}~~~ (1.1)$$

$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + \gamma_{0Q}Q_{j} + u_{0j}} ~~~ (1.2)$$

$$ Level~2~Slope Private:~~~{\beta_{1j} = \gamma_{10} + \gamma_{11}Private_{j} + u_{1j}} ~~~ (1.3)$$

$$ Level~2~Slope~X's:~~~{\beta_{x} = \gamma_{x0} + u_{xj}} ~~~ (1.4)$$




$$Mixed~model: ~~~{y_{ij} = \gamma_{00}+ \gamma_{0Q}(Q_{j}) +  \gamma_{10}(Time_{j}) + \gamma_{11}(Time_{ij})(Private_{j}) + \gamma_{x0}(X_{ij}) +u_{1j}(Time_{ij}) + u_{1j} + u_{xj}(X_{ij}) + u_{0j} + e_{ij}} ~~~(1.5)$$



Here I am going to mean center the continuous variables to reduce the potential effect of multicollinearity.
```{r}
head(ECLSK1)
#X1TCHAPP,X1TCHCON, X1TCHPER, X1TCHEXT, X1TCHINT, X1RTHET, X1MTHET, X1BMI, X1PUBPRI, X1_CHSEX_R, X1_HISP_R, X1_BLACK_R, X1_ASIAN_R, X1_AMINAN_R, X1_MULTR_R, X1PAR1RAC

ECLSK1Scale = ECLSK1[,c(5:12)]
ECLSK1Scale = scale(ECLSK1Scale, scale = FALSE)
ECLSK1NoScale = cbind(ECLSK1[c(1:4)], ECLSK1[c(13:20)])
ECLSK1 = cbind(ECLSK1NoScale, ECLSK1Scale)

ECLSK2Scale = ECLSK2[,c(5:12)]
ECLSK2Scale = scale(ECLSK2Scale, scale = FALSE)
ECLSK2NoScale = cbind(ECLSK2[c(1:4)], ECLSK2[c(13:20)])
ECLSK2 = cbind(ECLSK2NoScale, ECLSK2Scale)

ECLSK3Scale = ECLSK3[,c(5:12)]
ECLSK3Scale = scale(ECLSK3Scale, scale = FALSE)
ECLSK3NoScale = cbind(ECLSK3[c(1:4)], ECLSK3[c(13:20)])
ECLSK3 = cbind(ECLSK3NoScale, ECLSK3Scale)

ECLSK4Scale = ECLSK4[,c(5:12)]
ECLSK4Scale = scale(ECLSK4Scale, scale = FALSE)
ECLSK4NoScale = cbind(ECLSK4[c(1:4)], ECLSK4[c(13:20)])
ECLSK4 = cbind(ECLSK4NoScale, ECLSK4Scale)

ECLSK5Scale = ECLSK5[,c(5:12)]
ECLSK5Scale = scale(ECLSK5Scale, scale = FALSE)
ECLSK5NoScale = cbind(ECLSK5[c(1:4)], ECLSK5[c(13:20)])
ECLSK5 = cbind(ECLSK5NoScale, ECLSK5Scale)
```
Now I will put the model into nlme to analyze it longitudinally.  The first model is the null model that only contains the intercept for each person (i.e. their average self control value over time).  Then we compare the null model to a model with the covariates of interest with variable of interest the interaction between time and the PUBPRI variable where each person receives their own intercept and then compare that model to a model where each person receives their and intercept and slope (i.e. trajectory of a person’s self control over time).  I then compare each model using the anova package, which in all cases indicates that the model with random intercepts and slopes is a better fit relative to the null and random intercepts only models.  In order to compare the models, I need to use restricted maximum likelihood estimation to ensure the degrees of freedom are comparable.  

```{r}
head(ECLSK1)
library(nlme)

model11 = lme(fixed = X1TCHCON ~1, random =  ~ 1 | id, data = ECLSK1, method = "ML")
summary(model11)

model21 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI +X1_CHSEX_R + X1_HISP_R +X1_BLACK_R+X1_ASIAN_R+X1_AMINAN_R+X1_MULTR_R+X1PAR1RAC
, random = ~1 | id, data = ECLSK1, method = "ML")

model31 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI +X1_CHSEX_R + X1_HISP_R +X1_BLACK_R+X1_ASIAN_R+X1_AMINAN_R+X1_MULTR_R+X1PAR1RAC
, random = ~time | id, data = ECLSK1, method = "ML")
summary(model31)


anova(model11, model21, model31)

model12 = lme(fixed = X1TCHCON ~1, random =  ~ 1 | id, data = ECLSK2, method = "ML")


model22 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI +X1_CHSEX_R + X1_HISP_R +X1_BLACK_R+X1_ASIAN_R+X1_AMINAN_R+X1_MULTR_R+X1PAR1RAC
              , random = ~1 | id, data = ECLSK2, method = "ML")

model32 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI +X1_CHSEX_R + X1_HISP_R +X1_BLACK_R+X1_ASIAN_R+X1_AMINAN_R+X1_MULTR_R+X1PAR1RAC, random = ~time | id, data = ECLSK2, method = "ML")
anova(model12, model22, model32)

model13 = lme(fixed = X1TCHCON ~1, random =  ~ 1 | id, data = ECLSK3, method = "ML")


model23 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI +X1_CHSEX_R + X1_HISP_R +X1_BLACK_R+X1_ASIAN_R+X1_AMINAN_R+X1_MULTR_R+X1PAR1RAC
              , random = ~1 | id, data = ECLSK3, method = "ML")

model33 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI +X1_CHSEX_R + X1_HISP_R +X1_BLACK_R+X1_ASIAN_R+X1_AMINAN_R+X1_MULTR_R+X1PAR1RAC, random = ~time | id, data = ECLSK3, method = "ML")
anova(model13, model23, model33)

model14 = lme(fixed = X1TCHCON ~1, random =  ~ 1 | id, data = ECLSK4, method = "ML")


model24 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI +X1_CHSEX_R + X1_HISP_R +X1_BLACK_R+X1_ASIAN_R+X1_AMINAN_R+X1_MULTR_R+X1PAR1RAC, random = ~1 | id, data = ECLSK4, method = "ML")

model34 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI +X1_CHSEX_R + X1_HISP_R +X1_BLACK_R+X1_ASIAN_R+X1_AMINAN_R+X1_MULTR_R+X1PAR1RAC, random = ~time | id, data = ECLSK4, method = "ML")

anova(model14, model24, model34)

model15 = lme(fixed = X1TCHCON ~1, random =  ~ 1 | id, data = ECLSK5, method = "ML")


model25 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI +X1_CHSEX_R + X1_HISP_R +X1_BLACK_R+X1_ASIAN_R+X1_AMINAN_R+X1_MULTR_R+X1PAR1RAC, random = ~1 | id, data = ECLSK5, method = "ML")

model35 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI +X1_CHSEX_R + X1_HISP_R +X1_BLACK_R+X1_ASIAN_R+X1_AMINAN_R+X1_MULTR_R+X1PAR1RAC, random = ~time | id, data = ECLSK5, method = "ML")

model45 = lme(fixed = X1TCHCON ~ time*X1PUBPRI  +X1MTHET +  X1TCHAPP + X1TCHPER + X1TCHEXT + X1TCHAPP+X1TCHPER +X1TCHEXT+ X1TCHINT+X1MTHET+X1BMI +X1_CHSEX_R + X1_HISP_R +X1_BLACK_R+X1_ASIAN_R+X1_AMINAN_R+X1_MULTR_R+X1PAR1RAC, random = ~time | id, data = ECLSK5, correlation = corAR1(), method = "ML")

anova(model15, model25, model35, model45)
```
Next I need to grab the parameter estimate and standard error for the PUBPRI interaction with time variable, because this is the variable that can tell us if students in alternative to assigned public school change on the self control over time relative to students in assigned public schools.  I will also get the degrees of freedom for later use.  

Then I use the mi.meld function in Amelia to “average” the parameter estimates and standard errors across the five imputed datasets to create one parameter estimate and standard error for the interaction effect between PUBPRI and time.  
```{r}
n = model31$fixDF$X[3]
model31 = summary(model31)
model31= model31$tTable[17,c(1:2)]

model32 = summary(model32)
model32= model32$tTable[17,c(1:2)]

model33 = summary(model33)
model33= model33$tTable[17,c(1:2)]

model34 = summary(model34)
model34= model34$tTable[17,c(1:2)]

model35 = summary(model35)
model35= model35$tTable[17,c(1:2)]

modelAll = as.data.frame(t(cbind(model31, model32, model33, model34, model35)))
rownames(modelAll) <- c()

modelPars = as.data.frame(modelAll$Value)
colnames(modelPars) = c("Pars")
modelSE = as.data.frame(modelAll$Std.Error)
colnames(modelSE) = c("SE")
library(Amelia)
allPars = mi.meld(q = modelPars, se = modelSE)

allParsPE = t(as.data.frame(allPars$q.mi))

allParsSE =  t(as.data.frame(allPars$se.mi))

allParSesPaperSC = cbind(allParsPE, allParsSE)
colnames(allParSesPaperSC) = c("ParameterEstimate", "StandardError")
write.csv(allParSesPaperSC, "allParSesPaperSC.csv")
```
Here I am creating the t-statistic and p-value using the parameter estimate for the interaction of the alternative with time variable for teacher reported self control by dividing the parameter estimate by the standard error to get the t-statistic, then using the degrees of freedom for the interaction effect to get the associated p-value and confidence interval.  
```{r}
allParSesPaperSC = data.frame(allParSesPaperSC)
allParSesPaperSC$TStatistic = allParSesPaperSC$ParameterEstimate / allParSesPaperSC$StandardError

pValue = 2*pt(-abs(allParSesPaperSC$TStatistic), df = n)
allParSesPaperSC$pValue = pValue
allParSesPaperSC
Upper = 1.96*allParSesPaperSC$StandardError+allParSesPaperSC$ParameterEstimate
Lower = -(1.96*allParSesPaperSC$StandardError-allParSesPaperSC$ParameterEstimate)
allParSesPaperSC$Upper = Upper
allParSesPaperSC$Lower = Lower
allParSesPaperSC
```
