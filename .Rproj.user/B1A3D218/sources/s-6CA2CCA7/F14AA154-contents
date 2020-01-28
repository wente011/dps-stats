


idx<-seq(0,10000,0.1)
fne<-function(n){
 x<-(1 + 1/n)^n 
  
}

fnr<- fne(idx) %>% tibble() %>% mutate(n=idx) %>% rename(e.approx=".")

fnr %>% ggplot(aes(x=n,y=e.approx)) + geom_point() + ggtitle("Approximation of 'e'")

p.fun<-function(P,r,n,t){
y<-c(1:t)
  for (i in 0:t){
  y[i+1]<-P*(1+r/n)^(n*i)   
} 
  x2<-cbind.data.frame(y,seq(0,t,1))
names(x2)<-c(paste0("P for n=",12/n," months"),"Years")
x2 %<>% as_tibble()
  return(x2)
  }

y1<-p.fun(P=1000,r=0.20,n=1,t=10)
y6<-p.fun(P=1000,r=0.20,n=2,t=10) 
y4<-p.fun(P=1000,r=0.20,n=4,t=10) 
ycont<-p.fun(P=1000,r=0.20,n=525600,t=10) 

df<-bind_cols(y1,y6[,1],y4[,1],ycont[,1]) %>% gather("P","value",-Years)

df %>% ggplot(aes(x=Years,y=value)) + geom_line() + scale_y_continuous(breaks=seq(0,60000,1000)) + facet_wrap(~P)

df %>% filter(P=="P for n=12 months") %>% ggplot(aes(x=Years,y=value,color=P)) + geom_line() + scale_y_continuous(breaks=seq(0,60000,1000)) #+ geom_text(aes(y=Years,x=value))     )
df %>% filter(P=="P for n=6 months") %>% ggplot(aes(x=Years,y=value,color=P)) + geom_line() + scale_y_continuous(breaks=seq(0,60000,1000))
df %>% filter(P=="P for n=3 months") %>% ggplot(aes(x=Years,y=value,color=P)) + geom_line() + scale_y_continuous(breaks=seq(0,60000,1000))
df %>% filter(P=="P for n=2.28310502283105e-05 months") %>% ggplot(aes(x=Years,y=value,color=P)) + geom_line() + scale_y_continuous(breaks=seq(0,60000,1000))
df %>% ggplot(aes(x=Years,y=value,color=P)) + geom_line() + scale_y_continuous(breaks=seq(0,60000,1000))


logistic.map <- function(r, x, N, M){
  ## r: bifurcation parameter
  ## x: initial value
  ## N: Number of iteration
  ## M: Number of iteration points to be returned
  z <- 1:N
  z[1] <- x
  for(i in c(1:(N-1))){
    z[i+1] <- r *z[i]  * (1 - z[i])
  }
  ## Return the last M iterations 
  z[c((N-M):N)]
}

## Set scanning range for bifurcation parameter r
my.r <- seq(2.5, 4, by=0.005)
system.time(Orbit <- sapply(my.r, logistic.map,  x=0.001, N=1000, M=300))
##   user  system elapsed (on a 2.4GHz Core2Duo)
##  1.834   0.011   1.840 

x=0.001 
N=1000 
M=300

Orbit <- as.vector(Orbit)
r <- sort(rep(my.r, (M+1)))

plot(Orbit ~ r, pch=".")

