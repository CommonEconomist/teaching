z.test<-function(x,mu,var){
  zeta=(mean(x)-mu)/(sqrt(var/length(x)))
  return(zeta)
}