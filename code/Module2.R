### initialize

if (!require("ggplot2")) {      # If loading package fails,
  install.packages("ggplot2")   # download it from internet,
  stopifnot(require("ggplot2")) # and insist that it load.
}
if (!require("ggpubr")) {
  install.packages("ggpubr")
  stopifnot(require("ggpubr"))
}
if (!require("car")) {
  install.packages("car")
  stopifnot(require("car"))
}
if (!require("MASS")) {
  install.packages("MASS")
  stopifnot(require("MASS"))
}
if (!require("leaps")) {
  install.packages("leaps")
  stopifnot(require("leaps"))
}

### sumamrize the data

data.original = read.csv("BodyFat.csv")
head(data.original)
dim(data.original)
colnames(data.original)

data = data.original[,c(-1,-3)]
index = 1:252

summary(data)

par(mfrow = c(3,5))
for(i in 1:15){
  plot(data[,i], main = colnames(data)[i], ylab = "", xlab = "", 
       pch = 20, col = "gray")
}
par(mfrow = c(1,1))

### remove outliers

RMIndex = which(data[,1]==0 | data[,1]==max(data[,1]) | 
                  data[,3]==max(data[,3]) | data[,4]==min(data[,4]) | 
                  data[,5] > 35 | 
                  data[,6]==max(data[,6]) | data[,7]>125 | 
                  data[,8]==max(data[,8]) | data[,9]>120 | 
                  data[,10]==max(data[,10]) | data[,11]==max(data[,11]) | 
                  data[,11]==max(data[,11]) | data[,12]>29 | 
                  data[,13]==max(data[,13]))

data[RMIndex,]
pcolor = "cadetblue"

### scatter plot matrix

color = rep(pcolor, 252)
color[data[,1]==0 | data[,1]==max(data[,1])] = "red"
sp1 = ggplot(data, aes(y=BODYFAT, x = index)) + geom_point(color = color) + 
  ggtitle("BODYFAT") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
sp2 = ggplot(data, aes(y=AGE, x = index)) + geom_point(color = color) + 
  ggtitle("AGE") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
color[data[,3]==max(data[,3])] = "red"
sp3 = ggplot(data, aes(y=WEIGHT, x = index)) + geom_point(color = color) + 
  ggtitle("WEIGHT") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
color[data[,4]==min(data[,4])] = "red"
sp4 = ggplot(data, aes(y=HEIGHT, x = index)) + geom_point(color = color) + 
  ggtitle("HEIGHT") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
color[data[,5] > 35] = "red"
sp5 = ggplot(data, aes(y=ADIPOSITY, x = index)) + geom_point(color = color) + 
  ggtitle("ADIPOSITY") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
color[data[,6]==max(data[,6])] = "red"
sp6 = ggplot(data, aes(y=NECK, x = index)) + geom_point(color = color) + 
  ggtitle("NECK") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
color[data[,7]>125] = "red"
sp7 = ggplot(data, aes(y=CHEST, x = index)) + geom_point(color = color) + 
  ggtitle("CHEST") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
color[data[,8]==max(data[,8])] = "red"
sp8 = ggplot(data, aes(y=ABDOMEN, x = index)) + geom_point(color = color) + 
  ggtitle("ABDOMEN") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
color[data[,9]>120] = "red"
sp9 = ggplot(data, aes(y=HIP, x = index)) + geom_point(color = color) + 
  ggtitle("HIP") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
color[data[,10]==max(data[,10])] = "red"
sp10 = ggplot(data, aes(y=THIGH, x = index)) + geom_point(color = color) + 
  ggtitle("THIGH") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
color[data[,11]==max(data[,11])] = "red"
sp11 = ggplot(data, aes(y=KNEE, x = index)) + geom_point(color = color) + 
  ggtitle("KNEE") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
color[data[,12]>29] = "red"
sp12 = ggplot(data, aes(y=ANKLE, x = index)) + geom_point(color = color) + 
  ggtitle("ANKLE") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
color[data[,13]==max(data[,13])] = "red"
sp13 = ggplot(data, aes(y=BICEPS, x = index)) + geom_point(color = color) + 
  ggtitle("BICEPS") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
sp14 = ggplot(data, aes(y=FOREARM, x = index)) + geom_point(color = color) + 
  ggtitle("FOREARM") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())

color = rep(pcolor, 252)
sp15 = ggplot(data, aes(y=WRIST, x = index)) + geom_point(color = color) + 
  ggtitle("WRIST") + xlab("") + ylab("") + 
  theme(plot.title = element_text(color="#993333", size=10, face="bold",hjust = 0.5), 
        axis.text = element_blank(), axis.ticks = element_blank())
ggarrange(sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8,sp9,sp10,sp11,sp12,sp13,sp14,sp15,ncol=5,nrow=3)

### modify height of NO.42

c1 = 0.4535922921969
c2 = 0.0254
height42 = sqrt(data[42,3]*c1/data[42,5])/c2
data[42, 4] = height42
RMIndex = c(31, 39, 86, 182, 216)
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
h0 = 28/247 # Rule of thumb for judging outliers
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
d0 = 2*sqrt(14/247) # Rule of thumb for judging influential points
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
c0 = qf(0.5, 14, 233)
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

### Pairwise Plot
panel.hist_line = function(x,...)
{
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2],0,1.5))
  h = hist(x,plot = FALSE)
  breaks = h$breaks;nB = length(breaks)
  y = h$counts;y = y/max(y)
  rect(breaks[-nB],0,breaks[-1],y,col = "cyan")
}
panel_cor = function(x,y,digits = 2,prefix = "",cex.cor,...)
{
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0,1,0,1))
  r = abs(cor(x,y))
  txt = format(c(r,0.123456789),digits = digits)[1]
  txt = paste0(prefix,txt)
  if(missing(cex.cor)) cex.cor = 0.8/strwidth(txt)
  text(0.5,0.5,txt,cex = 4)
}
pairs(data[,c(3,8,13,15)],upper.panel = panel.smooth,lower.panel = panel_cor,
      cex = 1.5,pch = 16,col = "dodgerblue3", bg = "navy blue",
      diag.panel = panel.hist_line, cex.labels = 2,font.labels = 2)

### R^2, Cp, etc
my.regsub <- function(matrix,y,nbest,method,nvmax=8){
  temp <- regsubsets(matrix,y,nbest=nbest,method=method,nvmax=nvmax)
  temp.mat <- cbind(summary(temp)$which,
                    summary(temp)$rsq,summary(temp)$rss,
                    summary(temp)$adjr2,summary(temp)$cp,
                    summary(temp)$bic)
  dimnames(temp.mat)[[2]] <- c(dimnames(summary(temp)$which)[[2]],
                               "rsq", "rss", "adjr2", "cp", "bic")
  return(temp.mat)
}

N = 3
sele = my.regsub(data[-RMIndex,c(3,8,13,15)],y=data[-RMIndex,1],nbest=N,nvmax = N,method="exhaustive")
sele[order(sele[,which(colnames(sele) == 'rsq')],decreasing = TRUE),]

lm.final = lm(BODYFAT~WEIGHT+ABDOMEN, data[-RMIndex,])
summary(lm.final)

### global pairs R^2, Cp, etc 

sele = my.regsub(data[-RMIndex,2:15],y=data[-RMIndex,1],nbest=N,nvmax = N,method="exhaustive")
sele[order(sele[,which(colnames(sele) == 'rsq')],decreasing = TRUE),]

### global optimum 2 variable pair

results = matrix(rep(0, 91*4), ncol = 4, nrow = 91)
colnames(results) = c("V1", "V2", "R^2", "VIF")
k = 1
for(i in 2:14){
  s = i+1
  for(j in s:15){
    results[k, c(1,2)] = colnames(data)[c(i,j)]
    results[k, 3] = round(summary(lm(BODYFAT~., data = data[-RMIndex,c(1,i,j)]))$r.squared, 3)
    results[k, 4] = mean(vif(lm(BODYFAT~., data = data[-RMIndex,c(1,i,j)])))
    k = k+1
  }
}
results[results[,3]==max(results[,3]),]

### global optimum 3 variable pair

results2 = matrix(rep(0, 364*5), ncol = 5, nrow = 364)
colnames(results2) = c("V1", "V2", "V3", "R^2", "VIF")
k = 1
for(i in 2:13){
  s = i+1
  for(j in s:14){
    t = j+1
    for(l in t:15){
      results2[k, c(1,2,3)] = colnames(data)[c(i,j,l)]
      results2[k, 4] = round(summary(lm(BODYFAT~., data = data[-RMIndex,c(1,i,j,l)]))$r.squared, 3)
      results2[k, 5] = mean(vif(lm(BODYFAT~., data = data[-RMIndex,c(1,i,j,l)])))
      k = k+1
    }
  }
}
results2[results2[,4]==max(results2[,4]),]

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

lm.stdres = stdres(lm.final)
df = data.frame(fit = lm.final$fitted.values, str = lm.stdres)
srpf1 = ggplot(df,aes(x = fit, y = str)) + geom_point(color = pcolor, cex = 2) +
  ggtitle("Fitted Values v.s. Studentized Residuals") +
  xlab("Fitted Values") + ylab("Studentized residuals") +
  theme( plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
         axis.title.x = element_text(color="#993333", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))
lm.hats=hatvalues(lm.final)
df = data.frame(x = 1:length(index), y = lm.hats)
h0 = 4/247 # Rule of thumb for judging outliers
srpf2 = ggplot(df,aes(x=x,xend=x,y=0,yend=y,label = index)) + geom_segment() +
  annotate("text", x=which(lm.hats>h0), y=lm.hats[lm.hats>h0] + 0.01,  label= index[which(lm.hats>h0)]) +
  annotate("text", x = 255, y = h0-0.007, label = expression(2*p/n)) +
  geom_hline(yintercept=h0, linetype= "dashed", color = "navyblue", size=1) +
  ggtitle("Leverage Value") + xlab("Index") + ylab("Leverage Value") +
  theme( plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
         axis.title.x = element_text(color="#993333", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))
lm.dffits = dffits(lm.final)
df = data.frame(x = 1:length(index), y = lm.dffits)
d0 = 2*sqrt(2/247) # Rule of thumb for judging influential points
srpf3 = ggplot(df,aes(x=x,xend=x,y=0,yend=y,label = index)) + geom_segment() +
  annotate("text", x = which(abs(lm.dffits)>d0), y=lm.dffits[abs(lm.dffits)>d0],  label= index[which(abs(lm.dffits)>d0)]) +
  geom_hline(yintercept=c(-1*d0, 0, d0), linetype=c("dashed","solid","dashed"),
             color = "navyblue", size=1) +
  annotate("text", x = 255, y = d0-0.05, label = expression(2*sqrt(p/n))) +
  ggtitle("DFFITS") + xlab("Index") + ylab("DFFITS") +
  theme( plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
         axis.title.x = element_text(color="#993333", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))
lm.cooksD=cooks.distance(lm.final)
df = data.frame(x = 1:length(index), y = lm.cooksD)
c0 = qf(0.5, 2, 245)
srpf4 = ggplot(df,aes(x=x,xend=x,y=0,yend=y,label = index)) + geom_segment() +
  geom_hline(yintercept=c0, linetype= "dashed", color = "navyblue", size=1) +
  annotate("text", x = 255, y = c0-0.05, label = expression(F[paste("p,n-p", sep="")])) +
  ggtitle("Cook's Distance") + xlab("Index") + ylab("Cook's Distance") +
  theme( plot.title = element_text(color="#993333", size=16, face="bold",hjust = 0.5),
         axis.title.x = element_text(color="#993333", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))

ggarrange(srpf1, qp2, srpf2, srpf3, ncol=2, nrow=2)
