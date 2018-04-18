#Q2:
pop = dpois(100000, 4)
plot( dpois( x=0:10, lambda=1 ), type="l", col  = "chocolate", main = "Possion distribution for range [1,10]" , lwd=3, xlab="Specified Range", ylab="Value according to posson distribution")
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q3:
hist(pop,col=c('brown', 'orange', 'chocolate'),main = "Histogram for a random poisson distribution", xlab="Polulation", ylab="Frequency for each sample")
p = par(lwd=6, lty="dashed")
abline(v=mean(pop))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q4:
question4 <- function(pop){
  s = sample(pop,1000)
  hist(s,col=c('brown', 'orange', 'chocolate'), xlab="Polulation", ylab="Frequency for each sample")
  p = par(lwd=3, lty="dashed")
  abline(v=mean(s))
  par(lwd = 1, lty="solid")
}

repeat_quest4 <- function(pop){
  i=1
  par(mfrow=c(3,5))
  repeat{
    question4(pop)
    i= i + 1
    if( i == 16 ){
      break
    }
  }
  par(mfrow=c(1,1))
}

repeat_quest4()

#Comparing the sample with the distributions
qqplot(s, pop,col=c("red","blue"),lwd=3,type="p",main="QQPlot comparing the sample with the poisson distribution", xlab = "Sample points", ylab = "Population points",xlim=c(0,15), ylim=c(0,15))
legend("bottomright", inset=.05, title="Distributions", c("Sample","Population"), lwd=3, lty=c(3, 3), col=c("red","blue"))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Q5:
samples = lapply(1:50, function(x){ c(sample(pop,500)) })
means = lapply(samples, function(x){mean(x)})
means = unlist( means )
hist(means, col=c('brown', 'orange', 'chocolate'))

qqplot(means, pop,col=c("red","blue"),lwd=3,type="p",main="QQPlot comparing the sample with the poisson distribution", xlab = "Sample points means", ylab = "Population points")
qqnorm(means, col=c("red","blue"), lwd=3)
