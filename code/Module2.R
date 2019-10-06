### initialize

setwd("~/R/UW/STAT628 Module 2")
library("ggplot2")
library("ggpubr")
library("car")
library(MASS)

### sumamry the data

data.original = read.csv("BodyFat.csv")
head(data.original)
dim(data.original)
colnames(data.original)

data = data.original[,c(-1,-3)]

summary(data)

par(mfrow = c(3,5))
for(i in 1:15){
  plot(data[,i], main = colnames(data)[i], ylab = "", xlab = "", 
       pch = 20, col = "gray")
}
par(mfrow = c(1,1))

### scatter plot matrix

pcolor = "cadetblue"
sp1 = ggplot(data, aes(y=BODYFAT, x = index)) + geom_point(color = pcolor) + 
  ggtitle("BODYFAT") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp2 = ggplot(data, aes(y=AGE, x = index)) + geom_point(color = pcolor) + 
  ggtitle("AGE") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp3 = ggplot(data, aes(y=WEIGHT, x = index)) + geom_point(color = pcolor) + 
  ggtitle("WEIGHT") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp4 = ggplot(data, aes(y=HEIGHT, x = index)) + geom_point(color = pcolor) + 
  ggtitle("HEIGHT") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp5 = ggplot(data, aes(y=ADIPOSITY, x = index)) + geom_point(color = pcolor) + 
  ggtitle("ADIPOSITY") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp6 = ggplot(data, aes(y=NECK, x = index)) + geom_point(color = pcolor) + 
  ggtitle("NECK") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp7 = ggplot(data, aes(y=CHEST, x = index)) + geom_point(color = pcolor) + 
  ggtitle("CHEST") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp8 = ggplot(data, aes(y=ABDOMEN, x = index)) + geom_point(color = pcolor) + 
  ggtitle("ABDOMEN") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp9 = ggplot(data, aes(y=HIP, x = index)) + geom_point(color = pcolor) + 
  ggtitle("HIP") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp10 = ggplot(data, aes(y=THIGH, x = index)) + geom_point(color = pcolor) + 
  ggtitle("THIGH") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp11 = ggplot(data, aes(y=KNEE, x = index)) + geom_point(color = pcolor) + 
  ggtitle("KNEE") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp12 = ggplot(data, aes(y=ANKLE, x = index)) + geom_point(color = pcolor) + 
  ggtitle("ANKLE") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp13 = ggplot(data, aes(y=BICEPS, x = index)) + geom_point(color = pcolor) + 
  ggtitle("BICEPS") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp14 = ggplot(data, aes(y=FOREARM, x = index)) + geom_point(color = pcolor) + 
  ggtitle("FOREARM") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
sp15 = ggplot(data, aes(y=WRIST, x = index)) + geom_point(color = pcolor) + 
  ggtitle("WRIST") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
ggarrange(sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8,sp9,sp10,sp11,sp12,sp13,sp14,sp15,ncol=5,nrow=3)

### remove outliers

RMIndex = which(data[,1]==0 | data[,1]==max(data[,1]) | 
                data[,3]==max(data[,3]) | data[,4]==min(data[,4]) | 
                data[,6]==max(data[,6]) | data[,7]>125 | 
                data[,8]==max(data[,8]) | data[,9]>120 | 
                data[,10]==max(data[,10]) | data[,11]==max(data[,11]) | 
                data[,11]==max(data[,11]) | data[,12]>29 | 
                data[,13]==max(data[,13]))

data[RMIndex,]

### modify height of NO.42

c1 = 0.4535922921969
c2 = 0.0254
height42 = sqrt(data[42,3]*c1/data[42,5])/c2
data[42, 4] = height42
RMIndex = c(31, 39, 41, 86, 182, 216)
index = 1:252
index = index[-RMIndex]

### compare recalculated bmi and ADIPOSITY

bmi = data[,3]*c1/(data[,4]*c2)^2
bmi_error_index = which(abs(bmi-data[,5])>1)
data[bmi_error_index,5] = bmi[bmi_error_index]

### simple linear regression

lm.naive = lm(BODYFAT~., data = data[-RMIndex,])
summary(lm.naive)
vif(lm.naive)
mean(vif(lm.naive))

# scatter plot
df = data.frame(fit = lm.naive$fitted.values, res = lm.naive$residuals)
rp1 = ggplot(df,aes(x = fit, y = res)) + geom_point(color = pcolor, cex = 2) + 
  ggtitle("Fitted Values v.s. Studentized Residuals") +
  xlab("Fitted Values") + ylab("Studentized residuals") + 
  theme(plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
        axis.title.x = element_text(color="#993333", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold")) + 
  geom_smooth(method = lm, se = TRUE)

# qq plot
q1 = quantile(lm.naive$residuals, c(0.25, 0.75), type = 5)
q2 = qnorm(c(0.25, 0.75))
slope = diff(q1)/diff(q2)
int = q1[1]-slope*q2[1]
qp1 = ggplot() +
  geom_qq(aes(sample = lm.naive$residuals), color = pcolor, cex = 2) +
  geom_abline(intercept = int, slope = slope,
              color = "brown2", size = 1, alpha = 0.8)+
  ggtitle("QQ-Plot") + ylab("Standardized Residuals") + xlab("Theoretical Quantiles") + 
  theme(plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
        axis.title.x = element_text(color="#993333", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))

ggarrange(rp1, qp1, ncol=2, nrow=1)

### Studentized Residual

lm.stdres = stdres(lm.naive)
df = data.frame(fit = lm.naive$fitted.values, str = lm.stdres)
srp1 = ggplot(df,aes(x = fit, y = str)) + geom_point(color = pcolor, cex = 2) + 
  ggtitle("Fitted Values v.s. Studentized Residuals") +
  xlab("Fitted Values") + ylab("Studentized residuals") + 
  theme( plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
         axis.title.x = element_text(color="#993333", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))

### Leverage Value

lm.hats=hatvalues(lm.naive)
df = data.frame(x = 1:length(index), y = lm.hats)
h0 = 28/246 # Rule of thumb for judging outliers
srp2 = ggplot(df,aes(x=x,xend=x,y=0,yend=y,label = index)) + geom_segment() + 
  annotate("text", x=which(lm.hats>h0), y=lm.hats[lm.hats>h0] + 0.01,  label= index[which(lm.hats>h0)]) +
  annotate("text", x = 255, y = h0-0.007, label = expression(2*p/n)) + 
  geom_hline(yintercept=h0, linetype= "dashed", color = "navyblue", size=1) + 
  ggtitle("Leverage Value") + xlab("Index") + ylab("Leverage Value") + 
  theme( plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
         axis.title.x = element_text(color="#993333", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))

### DFFITS

lm.dffits = dffits(lm.naive)
df = data.frame(x = 1:length(index), y = lm.dffits)
d0 = 2*sqrt(14/246) # Rule of thumb for judging influential points
srp3 = ggplot(df,aes(x=x,xend=x,y=0,yend=y,label = index)) + geom_segment() + 
  annotate("text", x = which(abs(lm.dffits)>d0), y=lm.dffits[abs(lm.dffits)>d0],  label= index[which(abs(lm.dffits)>d0)]) +
  geom_hline(yintercept=c(-1*d0, 0, d0), linetype=c("dashed","solid","dashed"), 
             color = "navyblue", size=1) + 
  annotate("text", x = 255, y = d0-0.05, label = expression(2*sqrt(p/n))) +
  ggtitle("DFFITS") + xlab("Index") + ylab("DFFITS") + 
  theme( plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
         axis.title.x = element_text(color="#993333", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))

### Cook's Distance

lm.cooksD=cooks.distance(lm.naive)
df = data.frame(x = 1:length(index), y = lm.cooksD)
c0 = qf(0.5, 14, 232)
srp4 = ggplot(df,aes(x=x,xend=x,y=0,yend=y,label = index)) + geom_segment() + 
  geom_hline(yintercept=c0, linetype= "dashed", color = "navyblue", size=1) + 
  annotate("text", x = 255, y = c0-0.05, label = expression(F[paste("p,n-p", sep="")])) +
  ggtitle("Cook's Distance") + xlab("Index") + ylab("Cook's Distance") + 
  theme( plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
         axis.title.x = element_text(color="#993333", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))

ggarrange(srp1, srp2, srp3, srp4, ncol=2, nrow=2)

### strong influential points

data[index[which(lm.hats>h0 & abs(lm.dffits)>d0)],]

### save modified data

data.mod = data.original[-RMIndex,]
data.mod[,c(-1,-3)] = data[-RMIndex,]
write.csv(data.mod, file = "data/Modyfied_BodyFat.csv")

### forward AIC selection

biggest = formula(lm(BODYFAT~., data[-RMIndex,]))
step(lm(BODYFAT~1, data = data[-RMIndex,]), direction = "both", scope = biggest)
summary(lm(BODYFAT~ABDOMEN+WEIGHT+WRIST+BICEPS, data[-RMIndex,]))

### global optimum 2 variable pair

results = matrix(rep(0, 91*3), ncol = 3, nrow = 91)
colnames(results) = c("V1", "V2", "R^2")
k = 1
for(i in 2:14){
  s = i+1
  for(j in s:15){
    results[k, c(1,2)] = colnames(data)[c(i,j)]
    results[k, 3] = round(summary(lm(BODYFAT~., data = data[-RMIndex,c(1,i,j)]))$r.squared, 3)
    k = k+1
  }
}
results[results[,3]==max(results[,3]),]

### global optimum 3 variable pair

results = matrix(rep(0, 364*4), ncol = 4, nrow = 364)
colnames(results) = c("V1", "V2", "V3", "R^2")
k = 1
for(i in 2:13){
  s = i+1
  for(j in s:14){
    t = j+1
    for(l in t:15){
      results[k, c(1,2,3)] = colnames(data)[c(i,j,l)]
      results[k, 4] = round(summary(lm(BODYFAT~., data = data[-RMIndex,c(1,i,j,l)]))$r.squared, 3)
      k = k+1
    }
  }
}
results[results[,4]==max(results[,4]),]

lm.final = lm(BODYFAT~WEIGHT+ABDOMEN, data[-RMIndex,])
summary(lm.final)

# scatter plot
df = data.frame(fit = lm.final$fitted.values, res = lm.final$residuals)
rp2 = ggplot(df,aes(x = fit, y = res)) + geom_point(color = pcolor, cex = 2) + 
  ggtitle("Fitted Values v.s. Studentized Residuals") +
  xlab("Fitted Values") + ylab("Studentized residuals") + 
  theme(plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
        axis.title.x = element_text(color="#993333", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold")) + 
  geom_smooth(method = lm, se = TRUE)

# qq plot
q1 = quantile(lm.final$residuals, c(0.25, 0.75), type = 5)
q2 = qnorm(c(0.25, 0.75))
slope = diff(q1)/diff(q2)
int = q1[1]-slope*q2[1]
qp2 = ggplot() +
  geom_qq(aes(sample = lm.final$residuals), color = pcolor, cex = 2) +
  geom_abline(intercept = int, slope = slope,
              color = "brown2", size = 1, alpha = 0.8)+
  ggtitle("QQ-Plot") + ylab("Standardized Residuals") + xlab("Theoretical Quantiles") + 
  theme(plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
        axis.title.x = element_text(color="#993333", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))

ggarrange(rp2, qp2, ncol=2, nrow=1)

# boxcox

boxcox(lm.final)

# pairwise comparison

library("KernSmooth") 
### box and regression line
panel.box <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[3:4],0, 2 ))
  #par(usr = c(min(x)-1.5*IQR(x),max(x)+1.5*IQR(x), 0,2 ))
  boxplot(x,horizontal = TRUE, add=TRUE)
}

pairs(df,panel =function(x,y,...){
  points(x,y,...)
  abline(lm(y~x),col = 'red')
},
cex = 1.5,pch = 16, col = "light blue",
diag.panel = panel.box, cex.labels = 2,font.labels = 2)

### influential points diag

# lm.stdres = stdres(lm.final)
# df = data.frame(fit = lm.final$fitted.values, str = lm.stdres)
# srpf1 = ggplot(df,aes(x = fit, y = str)) + geom_point(color = pcolor, cex = 2) + 
#   ggtitle("Fitted Values v.s. Studentized Residuals") +
#   xlab("Fitted Values") + ylab("Studentized residuals") + 
#   theme( plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
#          axis.title.x = element_text(color="#993333", size=14, face="bold"),
#          axis.title.y = element_text(color="#993333", size=14, face="bold"))
# lm.hats=hatvalues(lm.final)
# df = data.frame(x = 1:length(index), y = lm.hats)
# h0 = 4/246 # Rule of thumb for judging outliers
# srpf2 = ggplot(df,aes(x=x,xend=x,y=0,yend=y,label = index)) + geom_segment() + 
#   annotate("text", x=which(lm.hats>h0), y=lm.hats[lm.hats>h0] + 0.01,  label= index[which(lm.hats>h0)]) +
#   annotate("text", x = 255, y = h0-0.007, label = expression(2*p/n)) + 
#   geom_hline(yintercept=h0, linetype= "dashed", color = "navyblue", size=1) + 
#   ggtitle("Leverage Value") + xlab("Index") + ylab("Leverage Value") + 
#   theme( plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
#          axis.title.x = element_text(color="#993333", size=14, face="bold"),
#          axis.title.y = element_text(color="#993333", size=14, face="bold"))
# lm.dffits = dffits(lm.final)
# df = data.frame(x = 1:length(index), y = lm.dffits)
# d0 = 2*sqrt(2/246) # Rule of thumb for judging influential points
# srpf3 = ggplot(df,aes(x=x,xend=x,y=0,yend=y,label = index)) + geom_segment() + 
#   annotate("text", x = which(abs(lm.dffits)>d0), y=lm.dffits[abs(lm.dffits)>d0],  label= index[which(abs(lm.dffits)>d0)]) +
#   geom_hline(yintercept=c(-1*d0, 0, d0), linetype=c("dashed","solid","dashed"), 
#              color = "navyblue", size=1) + 
#   annotate("text", x = 255, y = d0-0.05, label = expression(2*sqrt(p/n))) +
#   ggtitle("DFFITS") + xlab("Index") + ylab("DFFITS") + 
#   theme( plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
#          axis.title.x = element_text(color="#993333", size=14, face="bold"),
#          axis.title.y = element_text(color="#993333", size=14, face="bold"))
# lm.cooksD=cooks.distance(lm.final)
# df = data.frame(x = 1:length(index), y = lm.cooksD)
# c0 = qf(0.5, 2, 244)
# srpf4 = ggplot(df,aes(x=x,xend=x,y=0,yend=y,label = index)) + geom_segment() + 
#   geom_hline(yintercept=c0, linetype= "dashed", color = "navyblue", size=1) + 
#   annotate("text", x = 255, y = c0-0.05, label = expression(F[paste("p,n-p", sep="")])) +
#   ggtitle("Cook's Distance") + xlab("Index") + ylab("Cook's Distance") + 
#   theme( plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
#          axis.title.x = element_text(color="#993333", size=14, face="bold"),
#          axis.title.y = element_text(color="#993333", size=14, face="bold"))
# 
# ggarrange(srpf1, srpf2, srpf3, srpf4, ncol=2, nrow=2)
mean(data$WEIGHT)
