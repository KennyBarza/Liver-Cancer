getwd()
setwd("C:/Users/User/Desktop/New folder")
install.packages("readxl")
install.packages("ggplot2")
install.packages("MASS")
install.packages("EnvStats")
library(readxl)
library(ggplot2)
library(MASS)
library(EnvStats)
install.packages("bbmle")
library(bbmle)
data<-read_excel("Data3.xlsx")
data
dt<-read_excel("DoublingTime.xlsx")
dt
names(data)
gender<-table(data$Gender)
lbls1 <- c("M", "W")
pie(gender,labels = lbls1, col=rainbow(2),main="Gender_pie_chart")
barplot(gender)
total = sum(gender)

###Gender
percentages = gender/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
sum(gender[gender="Male"])
sum(gender[gender="Female"])
slices <- c(545,55 )
lbls <- c("Male", "Female")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Genders")
###

###Agegrp
AgeGroup<-table(data$AgeGroup)
lbls1 <- c(">70years", "<=70years")
pie(AgeGroup,labels = lbls1, col=rainbow(2),main="AgeGroup_pie_chart")
total = sum(AgeGroup)
percentages = AgeGroup/total*100
sum(AgeGroup[AgeGroup="> 70 years"])
sum(AgeGroup[AgeGroup="<=70 years"])
slices <- c(235, 364)
lbls <- c("> 70 years", "<=70 years")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Pie Chart of age group")

barplot(AgeGroup)
######

###absstatus

abs<-table(data$AbstinenceStatus)
x<-names(abs)
x
sum(abs[abs=x[1]])
sum(abs[abs=x[2]])
percentages = abs/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(312, 255)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Abstinence status")
barplot(abs)
#####


#####screening
screening<-table(data$Screening)
x<-names(screening)
x
sum(screening[screening=x[1]])
sum(screening[screening=x[2]])
percentages = screening/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(123, 470)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Screening")
barplot(screening)
#################
###diab
diab<-table(data$Diabetes)
x<-names(diab)
x
sum(diab[diab=x[1]])
sum(diab[diab=x[2]])

percentages = diab/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(412,183)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Diabetes")
barplot(diab)
###
###########################smoker
smok<-table(data$Smokers)
x<-names(smok)
x
sum(smok[smok=x[1]])
sum(smok[smok=x[2]])

percentages = smok/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(302,266)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Smokers")
barplot(smok)
#################################
#########Trombose
thr<-table(data$Thrombose)
x<-names(thr)
x
sum(thr[thr=x[1]])
sum(thr[thr=x[2]])

percentages = thr/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(408,183)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of thrombose")
barplot(thr)
###########################
#############cirrhosis
cir<-table(data$Cirrhosis)
x<-names(cir)
x
sum(cir[cir=x[1]])
sum(cir[cir=x[2]])

percentages =cir /total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(35,560)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Cirrhosis")
barplot(cir)
#############################
############milan
mil<-table(data$MilanCriteria)
x<-names(mil)
x
sum(mil[mil=x[1]])
sum(mil[mil=x[2]])

percentages = mil/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(181,418)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Milan criteria")
barplot(mil)
#####################
###########cancer stages
cs<-table(data$CancerStages)
x<-names(cs)
x
sum(cs[cs=x[1]])
sum(cs[cs=x[2]])
sum(cs[cs=x[3]])
sum(cs[cs=x[4]])


percentages = cs/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(68,30,252,189)
lbls <- c(x[1], x[2],x[3],x[4])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of cancer stages")
barplot(cs)
###############
#################################difusecancer

df<-table(data$DifuseCancer)
x<-names(df)
x
sum(df[df=x[1]])
sum(df[df=x[2]])
percentages = df/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(483,110)
lbls <- c(x[1], x[2],x[3],x[4])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of diffuse cancer")
barplot(df)
############################
################meta

mc<-table(data$MetastaticCancer)
x<-names(mc)
x
sum(mc[mc=x[1]])
sum(mc[mc=x[2]])
percentages = mc/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(514,81)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of diffuse metastatic cancer")
barplot(mc)
####################################\
#############curativetrea

ctr<-table(data$CurativeTreatment)
x<-names(ctr)
x
sum(ctr[ctr=x[1]])
sum(ctr[ctr=x[2]])
percentages = ctr/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(95,489)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of diffuse curative treatment")
barplot(ctr)
###########################
#####################treat1

t1<-table(data$Treatment1)
x<-names(t1)
x
sum(t1[t1=x[1]])
sum(t1[t1=x[2]])
percentages = t1/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(536,53)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of treatment1")
barplot(t1)
######################
##########treat2

t2<-table(data$Treatment2)
x<-names(t2)
x
sum(t2[t2=x[1]])
sum(t2[t2=x[2]])
percentages = t2/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(549,39)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of treatment2")
barplot(t2)
#####################################
#####treat3

t3<-table(data$Treatment3)
x<-names(t3)
x
sum(t3[t3=x[1]])
sum(t3[t3=x[2]])
percentages = t3/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(491,98)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of treatment3")
barplot(t3)
########################
##################treat4

t4<-table(data$Treatment4)
x<-names(t4)
x
sum(t4[t4=x[1]])
sum(t4[t4=x[2]])
percentages = t4/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(486,101)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of treatment4")
barplot(t4)
#########################
###############treat 5

t5<-table(data$Treatment5)
x<-names(t5)
x
sum(t5[t5=x[1]])
sum(t5[t5=x[2]])
percentages = t5/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(352,235)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of treatment5")
barplot(t5)
#########################
#####################treat6

t6<-table(data$Treatment6)
x<-names(t6)
x
sum(t6[t6=x[1]])
sum(t6[t6=x[2]])
percentages = t6/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(546,42)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of treatment6")
barplot(t6)
####################################
#################death

ds<-table(data$DeathStatus)
x<-names(ds)
x
sum(ds[ds=x[1]])
sum(ds[ds=x[2]])
percentages = ds/total*100
cat("Les valeurs en % sont de :",percentages ,"\n") 
slices <- c(431,120)
lbls <- c(x[1], x[2])
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of death status")
barplot(ds)

#######################
#####tumorsize
tumorsize<-table(data$TumorSize)
summary(data$TumorSize)
boxplot(data$TumorSize,col="light blue") 

X<- data$TumorSize
summary(X)
Y<-summary(X)
X[is.na(X)]<-Y[4]
summary(X)
t.test(X,conf.level=0.95)
X
var(X)
sd(X)
boxplot(X,col="light blue")
Hist1<-hist(X,col="light blue", main="...", xlab="...")
X<- replace (X, X<=0|X>95,mean(X)) 
summary(X)
t.test(X,conf.level=0.95)
var(X)
sd(X)
boxplot(X,col="light blue")
Hist1<-hist(X,col="light blue", main="...", xlab="...")
Hist1<-hist(X,col="light blue", main="...", xlab="...", proba = TRUE)

###########################
############age
age1<-table(data$Age)
X<-data$Age
Y<-summary(X)
Y
X[is.na(X)]<- Y[4]
summary(x)
t.test(X,conf.level=0.95) 
boxplot(X,col="light blue")
Hist1<-hist(X,col="light blue", main="...", xlab="...")
X<-replace(X,X<=42|X>=92,mean(X))
boxplot(X,col="light blue")
Hist1<-hist(X,col="light blue", main="...", xlab="...")
summary(X)
t.test(X,conf.level=0.95) 



###################################
###########SURV IN DAYS
sid<-data$SurvivalInDays
summary(data$SurvivalInDays)
sid[is.na(sid)]<-Y[4]
summary(sid)
t.test(sid,conf.level=0.95)
var(sid)
sd(sid)
boxplot(sid,col="light blue") 
Hist1<-hist(sid,col="light blue", main="...", xlab="...")
sid<- replace (sid, sid<=0|sid>600,mean(sid)) 

boxplot(sid,col="light blue")
Hist1<-hist(sid,col="light blue", main="...", xlab="...")
summary(sid)
#############################
############surv in mont
sim<-data$SurvivalInMonths
summary(data$SurvivalInMonths)
boxplot(data$SurvivalInMonths,col="light blue") 
Hist1<-hist(sim,col="light blue", main="...", xlab="...")
Y<-summary(data$SurvivalInMonths)
sim[is.na(sim)]<-Y[4]
summary(sim)
sim<- replace (sim, sim<=0|sim>20,mean(sim)) 

boxplot(sim,col="light blue")
Hist1<-hist(sim,col="light blue", main="...", xlab="...")








###############test d hypothese qui influence la survie en jours?????????????
##age
cor.test(data$Age,data$SurvivalInDays)#pas d influence


##sex
M<-unlist(na.omit(data[data$Gender=="Male","SurvivalInDays"]))
FE<-unlist(na.omit(data[data$Gender=="Female","SurvivalInDays"]))
#equality of variances
var.test(M,FE)
t.test(M,FE,var.equal = T)#Donc il ya pas influence
#######
##age group
p<-unlist(na.omit(data[data$AgeGroup=="<=70 years","SurvivalInDays"]))
g<-unlist(na.omit(data[data$AgeGroup=="> 70 years","SurvivalInDays"]))
#equality of variances
var.test(p,g)
t.test(p,g, var.equal = T)#Donc il ya pas influence
##abstinence status
yes<-unlist(na.omit(data[data$AbstinenceStatus=="Abstinet Alcohol related HCC","SurvivalInDays"]))
no<-unlist(na.omit(data[data$AbstinenceStatus=="Non Abstinet Alcohol related HCC","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
wilcox.test(yes,no)##il ya pas influence
##screening
yes<-unlist(na.omit(data[data$Screening=="Screened","SurvivalInDays"]))
no<-unlist(na.omit(data[data$Screening=="Unscreened","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
wilcox.test(yes,no)##il ya influence
##diabetes
yes<-unlist(na.omit(data[data$Diabetes=="Yes","SurvivalInDays"]))
no<-unlist(na.omit(data[data$Diabetes=="No","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
wilcox.test(yes,no)#pas d influence
##smokers
yes<-unlist(na.omit(data[data$Smokers=="Yes","SurvivalInDays"]))
no<-unlist(na.omit(data[data$Smokers=="No","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
wilcox.test(yes,no)#pas dinfluence
##thrombosis
yes<-unlist(na.omit(data[data$Thrombose=="Yes","SurvivalInDays"]))
no<-unlist(na.omit(data[data$Thrombose=="No","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
wilcox.test(yes,no)#il ya influence
##cirrhosis
yes<-unlist(na.omit(data[data$Cirrhosis=="Yes","SurvivalInDays"]))
no<-unlist(na.omit(data[data$Cirrhosis=="No","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
t.test(yes,no,var.equal = T)#pas d inluence
##diffuse cancer
yes<-unlist(na.omit(data[data$DifuseCancer=="Yes","SurvivalInDays"]))
no<-unlist(na.omit(data[data$DifuseCancer=="No","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
wilcox.test(yes,no)#il ya influence
##metastatic cancer
yes<-unlist(na.omit(data[data$MetastaticCancer=="Yes","SurvivalInDays"]))
no<-unlist(na.omit(data[data$MetastaticCancer=="No","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
wilcox.test(yes,no)#il ya influence
##curative treatment
yes<-unlist(na.omit(data[data$CurativeTreatment=="Treatment with a curative intent","SurvivalInDays"]))
no<-unlist(na.omit(data[data$CurativeTreatment=="Treatment without a curative intent","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
wilcox.test(yes,no)#il ya influence
##treatment 1
yes<-unlist(na.omit(data[data$Treatment1=="Yes","SurvivalInDays"]))
no<-unlist(na.omit(data[data$Treatment1=="No","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
t.test(yes,no,var.equal = T)#pas dinfluence
##treatment 2
yes<-unlist(na.omit(data[data$Treatment2=="Yes","SurvivalInDays"]))
no<-unlist(na.omit(data[data$Treatment2=="No","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
t.test(yes,no,var.equal = T)#pas dinfluence
##treatment 3
yes<-unlist(na.omit(data[data$Treatment3=="Yes","SurvivalInDays"]))
no<-unlist(na.omit(data[data$Treatment3=="No","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
t.test(yes,no,var.equal = T)#pas d influence
##treatment 4
yes<-unlist(na.omit(data[data$Treatment4=="Yes","SurvivalInDays"]))
no<-unlist(na.omit(data[data$Treatment4=="No","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
wilcox.test(yes,no)#pas d inlfuence
##treatment 5
yes<-unlist(na.omit(data[data$Treatment5=="Yes","SurvivalInDays"]))
no<-unlist(na.omit(data[data$Treatment5=="No","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
t.test(yes,no,var.equal = T)#pas d influence
##treatment 6
yes<-unlist(na.omit(data[data$Treatment6=="Yes","SurvivalInDays"]))
no<-unlist(na.omit(data[data$Treatment6=="No","SurvivalInDays"]))
#equality of variances
var.test(yes,no)
t.test(yes,no,var.equal = T)#no influence
##cancer stage
A<-unlist(na.omit(data[data$CancerStages=="A","SurvivalInDays"]))
B<-unlist(na.omit(data[data$CancerStages=="B","SurvivalInDays"]))
C<-unlist(na.omit(data[data$CancerStages=="C","SurvivalInDays"]))
D<-unlist(na.omit(data[data$CancerStages=="D","SurvivalInDays"]))
#equality of variances
var.test(A,B)#a=b
var.test(A,C)#a=/c
kruskal.test(list(A,B,C,D))#IL YA INFLUENCE
##tumor size
cor.test(data$TumorSize,data$SurvivalInDays)#il ya influence


########test d hypothese qui influence la variable death ?????????

##gender
x<-table(data$Gender,data$DeathStatus)
chisq.test(x)##pas  dinfluence
##age group
x<-table(data$AgeGroup,data$DeathStatus)
chisq.test(x)#pas d influence
##abstinence status
x<-table(data$AbstinenceStatus,data$DeathStatus)
chisq.test(x)##pas d influence
##screening
x<-table(data$Screening,data$DeathStatus)
chisq.test(x)#il ya influence
##diabetes
x<-table(data$Diabetes,data$DeathStatus)
chisq.test(x)# pas d influence
##smokers
x<-table(data$Smokers,data$DeathStatus)
chisq.test(x)#pas d influnce
##thrombosis
x<-table(data$Thrombose,data$DeathStatus)
chisq.test(x)#il y a influence
##cirrhosis
x<-table(data$Cirrhosis,data$DeathStatus)
chisq.test(x)#no influence
##diffuse cancer
x<-table(data$DifuseCancer,data$DeathStatus)
chisq.test(x)#il y a influence
##metastatic cancer
x<-table(data$MetastaticCancer,data$DeathStatus)
chisq.test(x)#il ya influence
##curative treatment
x<-table(data$CurativeTreatment,data$DeathStatus)
chisq.test(x)###il ya influence
##treatment 1
x<-table(data$Treatment1,data$DeathStatus)
chisq.test(x)#pas
##treatment 2
x<-table(data$Treatment2,data$DeathStatus)
chisq.test(x)#pas
##treatment 3
x<-table(data$Treatment3,data$DeathStatus)
chisq.test(x)#pas
##treatment 4
x<-table(data$Treatment4,data$DeathStatus)
chisq.test(x)#pas
##treatment 5
x<-table(data$Treatment5,data$DeathStatus)
chisq.test(x)#pas
##treatment 6
x<-table(data$Treatment6,data$DeathStatus)
chisq.test(x)#pas
##cancerstage
chisq.test(data$CancerStages,data$DeathStatus)#il y a influence
##tumor size
x<-data$TumorSize
x<- replace (x, x<=0|x>95,mean(x)) 
Y<-summary(x)
Y
x[is.na(x)]<- Y[4]
x<-replace(x,x<31.25,1)
x<-replace(x,x>=31.25 & x<52.5,2)
x<-replace(x,x>=52.5 & x<73.75,3)
x<-replace(x,x>=73.75 & x<=95,4)
chisq.test(x,data$DeathStatus)#il ya influence
##age
x<-data$Age
x<-replace(x, x<51.75,1)
x<-replace(x,x>=51.75 & x<67.5,2)
x<-replace(x,x>=67.5 & x<83.25,3)
x<-replace(x,x>=83.25 & x<=99,4)
chisq.test(x,data$DeathStatus)#no influence
######################################################estimation de la loi
#survie en ours
#gamma
x<-data$SurvivalInDays
x<-na.omit(x)
h<-fitdistr(x,"gamma")
h
y<-rgamma(600,0.7346835748,0.0024390440 )
#mle
emv<-function(x)
{logvraisemblance<-function(par,e=x)
{
    a<-par[1]
    lambda<-par[2]
    n<-length(e)
    v<--n*a*log(lambda)+n*log(gamma(a))+lambda*sum(e)-(a-1)*sum(log(e))}
emv <- optim(c(1, 1), logvraisemblance)
emv <-emv$par
return(emv)}
emv(x)
#mme
E=mean(x)
V=var(x)
alphaMM=E^2/V
betaMM=E/V
alphaMM
betaMM
#########################
#exponentielle
x1<-fitdistr(x,"exponential")
x1
x2<-rexp(600,0.003319891)
#mle 
n=length(x)
L<-function(beta){
    logL<-(n*log(beta)-beta*sum(x))
    return (-logL)
}
o=optim(0.5,L)
betaMV=o$par[1]
betaMV
#mme
E=mean(x)
beta=1/E
beta
#######################
#normal
x3<-fitdistr(x,"normal")
x3
x4<-rnorm(600,301.2147 , 370.6467)
#mle
normal<-function(x){
    mu=x[1];
    sigma=x[2];
    normal<-(-n*(log(sigma)+log(sqrt(2*pi)))-(1/2)*sum(((x-mu)/sigma)^2))
}
o=optim(c(1,1),normal)
mu=o$par[1]
sigma=o$par[2]
mu
sigma
#mme
n=length(x)
mu=mean(x)
mu
sigma2=(1/n)*sum((x-mu)^2)
sigma=sqrt(sigma2)
sigma
########################
#lognormal
x5<-fitdistr(x,"lognormal")
x5
x6<-rlnorm(600, 4.89071695 ,  1.47487613)
#mle
normal<-function(x){
    mu=x[1];
    sigma=x[2];
    normal<-(-n*(log(sigma)+log(sqrt(2*pi)))-(1/2)*sum(((log(x)-mu)/sigma)^2))
}
o=optim(c(1,1),normal)
mu=o$par[1]
sigma=o$par[2]
mu
sigma
#mme
n=length(x)
mu=mean(x)
mu
sigma2=(1/n)*sum((x-mu)^2)
sigma=sqrt(sigma2)
sigma
#weibull
#fitdistr
x7<-fitdistr(x,"weibull")
x7
x8<-rweibull(600,0.81093996  , 268.90427836  )
#mle
n=length(x)
weibul<-function(x){
    alpha=x[1]
    beta=x[2]
    weibul<-(n*(log(alpha)-alpha*log(beta))+(alpha-1)*sum(log(x))-(1/beta^alpha)*sum(x^alpha))
}
o=optim(c(1,1),weibul)
alpha=o$par[1]
beta=o$par[2]
alpha
beta


###########graph
hist(x,freq=F,col="light blue", main="survie en jours", xlab="...")
lines(density(y),main="Density estimate of data",col="blue", lwd=2)
lines(density(x2),main="Density estimate of data",col="red", lwd=2)
lines(density(x4),main="Density estimate of data",col="black", lwd=2)
lines(density(x6),main="Density estimate of data",col="grey", lwd=2)
lines(density(x8),main="Density estimate of data",col="purple", lwd=2)
legend("topright",legend=c("Gamma","Exponential","Normal","Log-Normal","Weibull"),lty=1,col=c("blue","red","black","grey","purple"),bty="o",pt.cex=2,cex=0.8,text.col="black")

#quelle loi???
#kolmogrov
ks.test(x,"pgamma",0.7346835748,0.0024390440)#elle suit gamma
ks.test(x,"pexp",0.003319891)#elle ne suit pas
ks.test(x,"pnorm",301.2147 , 370.6467)#ne suit pas la loi normal
ks.test(x,"plnorm",4.89071695 ,  1.47487613)#ne suit pas la loi lognormal
ks.test(x,"pweibull",0.81093996  , 268.90427836 )#elle suit la loi de weibull
#gamma ou weibull???
logLik(x7)#WEIBULL
logLik(h)
##############################################################
#taille de la tumeur



#gamma
X<-data$TumorSize
X<-X[X<95]
X<-na.omit(X)

X
x1<-fitdistr(X,"gamma")
x1
x2<-rgamma(600,3.669940410,0.088043760)
#mle
L<-function(x){
    alpha<-x[1]
    beta<-x[2]
    L1<-(n*(alpha*log(beta)-log(gamma(alpha)))+(alpha-1)*sum(log(X))-beta*sum(X))
}
o=optim(c(1,1),L)
alphaMV=o$par[1]
betaMV=o$par[2]
#mme
E=mean(X)
V=var(X)
alphaMM=E^2/V
betaMM=E/V
alphaMM
betaMM
#exponentielle
x3<-fitdistr(X,"exponential")
x3
x4<-rexp(600,0.023990512)
#mle 
n=length(X)
L<-function(beta){
    logL<-(n*log(beta)-beta*sum(X))
    return (-logL)
}
o=optim(0.5,L)
betaMV=o$par[1]
betaMV
#mme
E=mean(X)
beta=1/E
beta
#normal
x5<-fitdistr(X,"normal")
x5
x6<-rnorm(600,41.6831461,21.474387)
#mle
normal<-function(x){
    mu=x[1];
    sigma=x[2];
    -(-n*(log(sigma)+log(sqrt(2*pi)))-(1/2)*sum(((X-mu)/sigma)^2))
}
o=optim(c(1,1),normal)
mu=o$par[1]
sigma=o$par[2]
#mme
n=length(X)
mu=mean(X)
mu
sigma2=(1/n)*sum((X-mu)^2)
sigma=sqrt(sigma2)
sigma
#lognormal
x7<-fitdistr(X,"lognormal")
x7
x8<-rlnorm(600, 3.58771070  , 0.55232003)
#mle
normal<-function(x){
    mu=x[1];
    sigma=x[2];
    -(-n*(log(sigma)+log(sqrt(2*pi)))-(1/2)*sum(((log(X)-mu)/sigma)^2))
}
o=optim(c(1,1),normal)
mu=o$par[1]
sigma=o$par[2]
#mme
K=log(X)
n=length(K)
mu=mean(K)
mu
sigma2=(1/n)*sum((K-mu)^2)
sigma=sqrt(sigma2)
sigma
#weibull
x9<-fitdistr(X,"weibull")
x9
x10<-rweibull(600,2.07621520 ,  47.26196639 )
#mle
n=length(X)
weibull<-function(x){
    alpha=x[1]
    beta=x[2]
    -(n*(log(alpha)-alpha*log(beta))+(alpha-1)*sum(log(X))-(1/beta^alpha)*sum(X^alpha))
}
o=optim(c(1,1),weibull)
alpha=o$par[1]
beta=o$par[2]
alpha
beta

#graphe
hist(X,freq=F,col="light blue", main="taille de la tumeur", xlab="...")
lines(density(x2),main="Density estimate of data",col="red", lwd=2)
lines(density(x4),main="Density estimate of data",col="blue", lwd=2)
lines(density(x6),main="Density estimate of data",col="green", lwd=2)
lines(density(x8),main="Density estimate of data",col="black", lwd=2)
lines(density(x10),main="Density estimate of data",col="purple", lwd=2)
legend("topright",legend=c("Gamma","Exponential","Normal","Log-Normal","Weibull"),lty=1,col=c("red","blue","green","black","purple"),bty="o",pt.cex=2,cex=0.8,text.col="black")

##goodness of fit
#kolmogrov
ks.test(X,"pgamma",3.669940410,0.088043760)#ne suit pas
ks.test(X,"pexp",0.023990512)#ne suit pas
ks.test(X,"pnorm",41.6831461,21.474387)#ne suit pas
ks.test(X,"plnorm",3.58771070  , 0.55232003)#ne suit pas
ks.test(X,"pweibull",2.07621520 ,  47.26196639)#ne suit pas
#anderson darling
ad.test(X,pgamma,2.52164991,0.047834395)#p-value = 0.02015
ad.test(X,pexp,0.0189696242)#p-value = 1.156e-06
ad.test(X,pnorm,52.715857,35.058261)#p-value = 1.156e-06
ad.test(X,plnorm,3.75372253,0.66008715)#p-value = 0.2188
ad.test(X,pweibull,1.26169450 ,  61.97157407)#p-value = 0.002485
#LOGNORMAl













#############################2.3
doublingtime <- dt$`Dt in days`
doublingtime
##weibull###
dtw<-fitdistr(doublingtime, "weibull")
dtw
set.seed(7)
rdtw<-rweibull(1000,  1.43849737 ,  140.35103767  )

#####Kolmogrov Smirnov test######
ks.test(doublingtime, rdtw)   #0.8
#####Anderson Darling test######
ad.test(doublingtime,pweibull,1.43849737,140.35103767)   #0.3
######elle suit weibull
logLik(dtw)   #747.08
#################exponential
dte<-fitdistr(doublingtime, "exponential")
dte
set.seed(7)
rdte<-rexp(1000,   0.0078931390  )

#####Kolmogrov Smirnov test######
ks.test(doublingtime, rdte)     #0.1452
#####Anderson Darling test######
ad.test(doublingtime,pexp, 0.0078931390 )#0.003

logLik(dte)  #759 elle ne suit pas
###################Gamma
dtg<-fitdistr(doublingtime, "gamma")
dtg
set.seed(7)
rdtg<-rgamma(1000,    1.962174676,   0.015487722   )

#####Kolmogrov Smirnov test######
ks.test(doublingtime, rdtg)  #0.7
#####Anderson Darling test######
ad.test(doublingtime,pgamma, 1.962174676,   0.015487722   ) #0.5

logLik(dtg)
#745
dtln<-fitdistr(doublingtime, "lognormal")
dtln
set.seed(7)
rdtln<-rlnorm(1000,   4.56580355  , 0.77142052   )

#####Kolmogrov Smirnov test######
ks.test(doublingtime, rdtln)  #0.5
#####Anderson Darling test######
ad.test(doublingtime,plnorm,  4.56580355  , 0.77142052    )  #0.6

logLik(dtln)
#744 elle suit lognormal 
####################################################################################################
###########################################################################################
unsc<-na.omit(data[data$Screening=="Unscreened","TumorSize"])
unsc<-as.vector(unlist(unsc))
unsc<-unsc[unsc<95]
#normal
x1<-fitdistr(unsc,"normal")
x1
rx1<-rnorm(1000,44.1,22.9134454)
ks.test(unsc, rx1) 
#lognormal
x2<-fitdistr(unsc,"lognormal")
x2
rx2<-rnorm(1000,3.646651,0.55541635)
ks.test(unsc,rx2)
#weibull
x3<-fitdistr(unsc,"weibull")
x3
rx3<-rweibull(1000,2.15286,49.96275443)
ks.test(unsc,rx3)#0.072
#exp
x4<-fitdistr(unsc,"exponential")
x4
rx4<-rexp(1000,0.022675737)
ks.test(unsc,rx4)
#gamma
x5<-fitdistr(unsc,"gamma")
x5
rx5<-rgamma(1000,3.73482,0.0846899)
ks.test(unsc,rx5)
###################################################################################################################

dt1<-rweibull(1000,2.15286,49.96275443)
DT<-rlnorm(1000,   4.56580355  , 0.77142052   )
dt0<-rtri(1000,1,1.4,1.2)
mst<-3*DT*(log(dt1/dt0)/log(2))
#gamma
x1<-fitdistr(mst,"gamma")
x1
rx1<-rgamma(1000,1825.63,1642.23)
ks.test(mst,rx1)#does not follow
#weibull
x3<-fitdistr(mst,"weibull")
x3
rx3<-rweibull(1000, 1.273620e+00,1.958174e+03 )
ks.test(mst,rx3)#0.033
#exp
x4<-fitdistr(mst,"exponential")
x4
rx4<-rexp(1000,5.477549e-04)
ks.test(mst,rx4)

#lognormal
x2<-fitdistr(mst,"lognormal")
x2
rx2<-rlnorm(1000,7.19670399,0.79922621)
ks.test(mst,rx2)#lognormal
###########################
