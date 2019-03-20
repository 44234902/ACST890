#Q1
#n is the number of total payments received from the bond
#pv=C*exp(-yield*t1)+C*exp(-yield*t2)+...+C*exp(-yield*tn)+F*exp(-yield*tn)
#tn=n/2 since the bond is half-yearly payment

pvfunction<-function(c,yield,f,n){
  sum=0
  for (i in 1:n) {
    sum=sum+c*exp(-yield[i]*(i/2))     
    i=i+1
  }
  pv<-sum+f*exp(-yield[n]*(n/2))
  return(pv)
}

