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
sample_means = lapply(samples, function(x){mean(x)})
sample_means = unlist( sample_means )
hist(sample_means, col=c('brown', 'orange', 'chocolate'))

qqplot(sample_means, pop,col=c("red","blue"),lwd=3,type="p",main="QQPlot comparing the sample with the poisson distribution", xlab = "Sample points means", ylab = "Population points")
legend("bottomright", inset=.05, title="Distributions", c("Sample","Population"), lwd=3, lty=c(3, 3), col=c("red","blue"))

qqnorm(sample_means, col=c("red","blue"), lwd=3)
legend("bottomright", inset=.05, title="Distributions", c("Sample","Population"), lwd=3, lty=c(3, 3), col=c("red","blue"))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q7:
#Mean an Standard error for samples_mean
x = mean( samples_mean )
SE = sd( samples_mean  )/sqrt( 50 )

#Mean an Standard error for population
x_p = mean( pop )
SE_p = sd( pop  )/sqrt( 100000 )


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q8:
heights = mosaicData::Galton$height
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q9:
question_9 <- function(){
  #Sampling
  s = sample(pop,50)
  
  #mean and SD
  m = mean(s)
  s = sd(s)
  me = (s/sqrt(50))*abs( qnorm(0.015) )
  pop_mean = mean(pop)
  
  if( pop_mean < (m+me) && pop_mean > (m-me) ){
    return(1)
  }else{
    return(0)
  }
}

c = lapply(1:10000, function(x){ question_9() })
result = mean( unlist(c)  )

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q10:
question_10 <- function(){
  #Sampling
  s = sample(pop,10)
  
  #mean and SD
  m = mean(s)
  s = sd(s)
  me = (s/sqrt(50))*abs( qnorm(0.05) )
  pop_mean = mean(pop)
  
  if( pop_mean < (m+me) && pop_mean > (m-me) ){
    return(1)
  }else{
    return(0)
  }
}

c = lapply(1:10000, function(x){ question_10() })
result = mean( unlist(c)  )

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q11:

calculate_ci = function(sample, cl){
  
  #Checkin the conditions
  if( length(sample) < 30 ){
    message("This sample is not large enough. CLT is not applicable.")
    return(NA)
  }
  
  if( ( mean(sampe) - median(sample) )/sd(sample) != 0 ){
    message("This sample is skewed. CLT is not applicable.")
    return(NA)
  }
  
  m = mean( sample )
  s = sd( sample )
  me = (s/sqrt(length(sample)))*abs(qnorm( (1-cl)/2 ))
  
  print( paste( "The lower bound is: ", (m-me) ) )
  print( paste( "The upper bound is: ", (m+me) ) )
  
  return( 2*me )
}


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q12:
ci = seq(0,1, by=0.1)
sample = sample( heights , 60 )

samples = rep( list(sample) , 11 )

result = sapply( samples, calculate_ci, ci )
y = unlist( lapply(1:length(ci),function(x,y){ return( y[x] ) },y=result) )

plot(ci,y, type="l", lwd=2, col=c("red"),xlim=c(0.5,1.0),main="Length of confidence intervals corresponding to their value", xlab="values for CI", ylab="Corresponding length")


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q13:
outcomes = c(1:10)
c = lapply(1:15000, function(x){sample(outcomes, 1, replace=TRUE)})
hist(unlist(c), col=c('brown', 'orange', 'chocolate'),xlab="Dicess of the toss", ylab="Occurance frequency", main="Conculations of simulating  rolling a toss 15000 times")

f = sapply(c(1:10),function(x,y){ sum( y==x )/length(y) },c)
table = data.table(Dices_of_toss = c(1:10), percentage_of_occurance = f*100)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q14:
sample(c(1,2),1,prob=c(5/16,11/16))
c = lapply(1:1000, function(x){ sample(c(1,2),1,prob=c(5/16,11/16))})
hist(unlist(c), main=("Distributions of outcomes from simulation perform for Morphy toast"),xlab=" Outcomes: 1:Dropping on lighter side 2: dropping on heavier side", ylab="frequency of each outcome", col=c("brown","orange"))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q15:

question_16 = function(){
  t = sample(c(1:6),2)
  s = sum(t)
  
  if( s>8 ){
    return( 1L )
  }else{
    return(0L)
  }
}

c=lapply(1:100000,function(x){question_16()})
sum( unlist(c) )/length( unlist(c) )

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Q16:
d = data.table( mosaicData::Weather )
g = d[city=="Beijing",.(H=avg_humidity),]
mean( unlist(g) )
