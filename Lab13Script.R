library(tidyverse)
library(e1071)



#STEP 1

data = read_csv("data.csv")


skewn = skewness(data$farther)

tstat = t.test(data$farther,alternative = "less")[["statistic"]][["t"]]

pdf = dnorm(tstat)

(error = (pdf)*(1/5)*skewn*(2*(tstat)^2 + 1)*(1/6))

errorgraph = tibble(t = seq(-10,10, length.out = 1000), error = (dnorm(t))*(1/5)*skewn*(2*(t)^2 + 1)*(1/6))

ggplot(errorgraph)+
  geom_line(aes(x=t,y=error))+
  theme_minimal()+
  xlab("T-Statistic")+
  ylab("Error")

alpha = 0.05

(size = ((skewness(data$farther)/(6*0.1*alpha))*(2*qnorm(alpha)^2 + 1)*dnorm(qnorm(alpha)))^2)



#STEP 2
#FARTHER

farther.resampled.t = c()
farther.resamples.shifted = c()
farther.resampled.p = c()
farther.resampled.mean= c()

for(i in 1:10000){
  curr.sample = sample(data$farther, size = length(data$farther), replace = T)
  farther.resampled.mean[i] = mean(curr.sample)
  farther.resampled.t[i] = 5*farther.resampled.mean[i]/sd(data$farther)
  
}
for(i in 1:10000){
  farther.resamples.shifted[i] = farther.resampled.t[i] - mean(farther.resampled.t)
}

for(i in 1:10000){
  farther.resampled.p[i] = length(which(farther.resamples.shifted <= farther.resampled.t[i]))/length(farther.resamples.shifted)
}
mean(farther.resampled.p)



(farther.quantile = quantile(farther.resamples.shifted, 0.05)) #-1.710882
quantile(farther.resampled.mean, c(0.025,0.975)) #-9.778089 -5.970878


#CLOSER



closer.resampled.t = c()
closer.resamples.shifted = c()
closer.resampled.p = c()
closer.resampled.mean = c()

for(i in 1:10000){
  curr.sample = sample(data$closer, size = length(data$closer), replace = T)
  closer.resampled.mean[i] = mean(curr.sample)
  closer.resampled.t[i] = 5*closer.resampled.mean[i]/sd(data$closer)
  
}
for(i in 1:10000){
  closer.resamples.shifted[i] = closer.resampled.t[i] - mean(closer.resampled.t)
}

for(i in 1:10000){
  closer.resampled.p[i] = length(which(closer.resamples.shifted >= closer.resampled.t[i]))/length(closer.resamples.shifted)
}
mean(closer.resampled.p)
#ALL 0


(closer.quantile = quantile(closer.resamples.shifted, 0.95)) #1.710882
quantile(closer.resampled.mean, c(0.025,0.975)) #6.401469 10.210214 

#DIFFERENCE


dif.resampled.t = c()
dif.resamples.shifted = c()
dif.resampled.p = c()
dif.resampled.mean = c()

for(i in 1:10000){
  curr.sample = sample(data$difference, size = length(data$difference), replace = T)
  dif.resampled.mean[i] = mean(curr.sample)
  dif.resampled.t[i] = 5*dif.resampled.mean[i]/sd(data$difference)
  
}
for(i in 1:10000){
  dif.resamples.shifted[i] = dif.resampled.t[i] - mean(dif.resampled.t)
}

for(i in 1:10000){
  dif.resampled.p[i] = length(which(dif.resamples.shifted >= abs(dif.resampled.t[i]) | dif.resamples.shifted <= -abs(dif.resampled.t[i])))/length(dif.resamples.shifted)
}
mean(dif.resampled.p)
#ALL 0


(dif.quantile = quantile(dif.resamples.shifted, c(0.025,0.975))) #-1.890905  1.917840 
quantile(dif.resampled.mean, c(0.025,0.975)) #0.2814377 0.4427011



#STEP 3
#FARTHER

farther.xbars = c()

for(i in 1:10000){
  curr.rand = data$farther*sample(c(-1,1),length(data$farther),replace = T)
  farther.xbars[i] = mean(curr.rand)
}

delta = abs(mean(data$farther))
low = -delta
high = delta
mean(farther.xbars <= low) #PVAL

mu0.iterate = 0.001
mu0 = mean(data$farther)
ci.data <- data$farther
repeat{
  curr.shifted.dat <- ci.data - mu0
  for(i in 1:1000){
    curr.rand = curr.shifted.dat*sample(c(-1,1),length(curr.shifted.dat),replace = T)
    farther.xbars[i] = mean(curr.rand)
  }
  farther.xbars <- farther.xbars + mu0
  
  delta = abs(mean(data$farther) - mu0) 
  low = mu0 - delta
  high = mu0 + delta
  p.val = mean(farther.xbars <= low) +
    mean(farther.xbars >= high)
  
  if(p.val < 0.05){
    break
    mu0 <- mu0 + mu0.iterate
  }else{
    mu0 <- mu0 - mu0.iterate
  }
  
}
farther.rand.lbound = mu0


mu0.iterate = 0.001
mu0 = mean(data$farther)
ci.data <- data$farther
repeat{
  curr.shifted.dat <- ci.data - mu0
  for(i in 1:1000){
    curr.rand = curr.shifted.dat*sample(c(-1,1),length(curr.shifted.dat),replace = T)
    farther.xbars[i] = mean(curr.rand)
  }
  farther.xbars <- farther.xbars + mu0
  
  delta = abs(mean(data$farther) - mu0) 
  low = mu0 - delta
  high = mu0 + delta
  p.val = mean(farther.xbars <= low) +
    mean(farther.xbars >= high)
  
  if(p.val < 0.05){
    mu0 = mu0 - mu0.iterate
    break
  }else{
    mu0 <- mu0 + mu0.iterate
  }
}
farther.rand.ubound = mu0

farther.rand.lbound
farther.rand.ubound


#Closer###############################

closer.xbars = c()

for(i in 1:10000){
  curr.rand = data$closer*sample(c(-1,1),length(data$closer),replace = T)
  closer.xbars[i] = mean(curr.rand)
}

delta = abs(mean(data$closer))
low = -delta
high = delta
mean(closer.xbars >= high) 

mu0.iterate = 0.001
mu0 = mean(data$closer)
ci.data <- data$closer
repeat{
  curr.shifted.dat <- ci.data - mu0
  for(i in 1:1000){
    curr.rand = curr.shifted.dat*sample(c(-1,1),length(curr.shifted.dat),replace = T)
    closer.xbars[i] = mean(curr.rand)
  }
  closer.xbars <- closer.xbars + mu0
  
  delta = abs(mean(data$closer) - mu0) 
  low = mu0 - delta
  high = mu0 + delta
  p.val = mean(closer.xbars <= low) +
    mean(closer.xbars >= high)
  
  if(p.val < 0.05){
    break
    mu0 <- mu0 + mu0.iterate
  }else{
    mu0 <- mu0 - mu0.iterate
  }
  
}
closer.rand.lbound = mu0


mu0.iterate = 0.001
mu0 = mean(data$closer)
ci.data <- data$closer
repeat{
  curr.shifted.dat <- ci.data - mu0
  for(i in 1:1000){
    curr.rand = curr.shifted.dat*sample(c(-1,1),length(curr.shifted.dat),replace = T)
    closer.xbars[i] = mean(curr.rand)
  }
  closer.xbars <- closer.xbars + mu0
  
  delta = abs(mean(data$closer) - mu0) 
  low = mu0 - delta
  high = mu0 + delta
  p.val = mean(closer.xbars <= low) +
    mean(closer.xbars >= high)
  
  if(p.val < 0.05){
    mu0 = mu0 - mu0.iterate
    break
  }else{
    mu0 <- mu0 + mu0.iterate
  }
}
closer.rand.ubound = mu0

closer.rand.lbound
closer.rand.ubound

#DIF##############################

dif.xbars = c()

for(i in 1:10000){
  curr.rand = data$difference*sample(c(-1,1),length(data$difference),replace = T)
  dif.xbars[i] = mean(curr.rand)
}

delta = abs(mean(data$difference))
low = -delta
high = delta
mean(dif.xbars >= high) + mean(dif.xbars <= low) #PVAL


mu0.iterate = 0.001
mu0 = mean(data$difference)
ci.data <- data$difference
repeat{
  curr.shifted.dat <- ci.data - mu0
  for(i in 1:1000){
    curr.rand = curr.shifted.dat*sample(c(-1,1),length(curr.shifted.dat),replace = T)
    dif.xbars[i] = mean(curr.rand)
  }
  dif.xbars <- dif.xbars + mu0
  
  delta = abs(mean(data$difference) - mu0) 
  low = mu0 - delta
  high = mu0 + delta
  p.val = mean(dif.xbars <= low) +
    mean(dif.xbars >= high)
  
  if(p.val < 0.05){
    break
    mu0 <- mu0 + mu0.iterate
  }else{
    mu0 <- mu0 - mu0.iterate
  }
  
}
dif.rand.lbound = mu0


mu0.iterate = 0.001
mu0 = mean(data$difference)
ci.data <- data$difference
repeat{
  curr.shifted.dat <- ci.data - mu0
  for(i in 1:1000){
    curr.rand = curr.shifted.dat*sample(c(-1,1),length(curr.shifted.dat),replace = T)
    dif.xbars[i] = mean(curr.rand)
  }
  dif.xbars <- dif.xbars + mu0
  
  delta = abs(mean(data$difference) - mu0) 
  low = mu0 - delta
  high = mu0 + delta
  p.val = mean(dif.xbars <= low) +
    mean(dif.xbars >= high)
  
  if(p.val < 0.05){
    mu0 = mu0 - mu0.iterate
    break
  }else{
    mu0 <- mu0 + mu0.iterate
  }
}
dif.rand.ubound = mu0

dif.rand.lbound
dif.rand.ubound

