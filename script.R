##------------------------ scatterplot matrix
require(SciViews)
library("KernSmooth") 
### box and regression line
panel.box <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[3:4],0, 2 ))
  #par(usr = c(min(x)-1.5*IQR(x),max(x)+1.5*IQR(x), 0,2 ))
  boxplot(x,horizontal = TRUE, add=TRUE)
}

pairs(data[,c(3,4,5,12)],panel =function(x,y,...){
  points(x,y,...)
  abline(lm(y~x),col = 'red')
},
cex = 1.5,pch = 16, col = "light blue",
diag.panel = panel.box, cex.labels = 2,font.labels = 2)

######-----------------------
rm(list = ls())

data = read.csv('BodyFat.csv')
data$IDNO = NULL
#data$DENSITY = NULL

#   ADIPOSITY = weight,height


ml = lm(BODYFAT ~.,data)

###  studentized
library(MASS)
## fit vs res ##

plot(ml$fitted.values ,studres(ml),xlab="Fitted values",
     ylab="Studentized residuals",
     main="Residual vs Fitted") 
abline(h=0);abline(h=3,lty=2);abline(h=-3,lty=2)
## res QQ ##
qqnorm(studres(ml),ylab="Studentized residuals",
       ylim=c(-2,2));qqline(studres(lsfit))

##  plot for standard / studentized res. vs fitted value
index= rownames(data)
par(mfrow=c(1,1))
plot(ml$fitted, stdres(ml), xlab="Fitted values",
     ylab="Internally studentized residual") 
abline(h=0, lty=2)
text(ml$fitted, stdres(ml), labels=index, cex=1, pos=2)


plot(ml$fitted, studres(ml), type = "h", xlab="Fitted values",
     ylab="Externally studentized residual")

qt(1-9.920635e-05,236)


abline(h=c(0,-3.7,3.7), lty=2)
text(ml$fitted, studres(ml), labels=index, cex=1, pos=2)

## leverage 
reg_hats = hatvalues(ml)
plot(reg_hats, type = "h", ylab = "Leverage") 

text(reg_hats, labels = rownames(data), cex = 1) 

abline(h=2*14/252, lty = 2) # h=2 times p / n =2 times 14 / 252

reg_cooks = cooks.distance(ml)
plot(reg_cooks, type = "h", ylab="Cook's Distance",ylim=c(0,2.5)) 
text(reg_cooksD, labels = index, cex = 1)
abline(h=qf(0.50,15,237), lty=2) #check whether D_i > f_0.5,p,n-p


attach(data)
str(data)
summary(data)


library(ggplot2)
ggplot(data,aes(x = BODYFAT)) + geom_histogram(stat = 'bin',bins = 40)
ggplot(data,aes(x = AGE)) + geom_histogram(stat = 'bin',bins = 252)
ggplot(data,aes(x = HeHEIGHT)) + geom_histogram(stat = 'bin',bins = 40)
ggplot(data,aes(x = NECK)) + geom_histogram(stat = 'bin',bins = 40)
ggplot(data,aes(x = KNEE)) + geom_histogram(stat = 'bin',bins = 40)
ggplot(data,aes(x = WRIST)) + geom_histogram(stat = 'bin',bins = 40)

te = data[data$HEIGHT != min(data$HEIGHT),]

mlte = lm(HEIGHT~ WEIGHT + ADIPOSITY, te)
predict(mlte, data[data$HEIGHT == min(data$HEIGHT),c(3,5)])

a = data[data$HEIGHT == min(data$HEIGHT),c(3,5)]




te = data[data$BODYFAT != 0,1:2]

mlte = lm(BODYFAT ~ DENSITY, te)
predict(mlte, newdata = data.frame(DENSITY = data[data$BODYFAT == 0,2]))



data[data$BODYFAT == 0,2]

te = data[c(2,3,4),]
