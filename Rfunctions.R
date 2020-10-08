# R functions


############################### functions
wy<-function(x)
{
  x$year<-year(x$date)
  x$month<-month(x$date)
  x$day<-day(x$date)
  x$wy<-ifelse(x$month>=10,x$year+1,x$year)
  x
}


# model accuracy

lognse = function(m,o) {
  m = log(m+0.000001)
  o = log(o+0.000001)
  err = m-o
  ns = 1-var(err)/var(o)
  ns
}

mper.err = function(m,o) {
  tmp = (mean(m)-mean(o))/mean(o)*100
  tmp = (tmp)
  tmp}


bias=function(m,o){
  bias<-abs(mean(m-o))
  bias
}

rmse=function(m,o){
  rmse<-sqrt(mean((m - o)^2))
  rmse
}


nse=function(m,o){
  
  NSE<- 1 - sum((m - o)^2) / sum((mean(o) - o)^2) #NSE
  NSE
}