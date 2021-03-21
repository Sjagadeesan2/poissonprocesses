#generate poisson processes
rpoip<-function(lambda,WW){
  nn<-rpois(1,lambda*WW)
  rr<-c(0,runif(nn,min=0,max=WW))
  rr<-sort(rr)#sort
  return (list(poip=(cbind(rr,0:(nn))),lambda=lambda))
}
#plot poisson process
plotpoip<-function(poip){
  poip<-poip$poip
  plot(stepfun(poip[,1],c(poip[,2],poip[nrow(poip),2]+1)))
}


poip<-rpoip(10,100)
plot(poip$poip)
plotpoip(poip)

rcompenpoiph<-function(poip){
  lambda<-poip$lambda
  poip<-poip$poip
  compenpoip<-poip
  compenpoip[,2]<-poip[,2]-lambda*poip[,1]
  return (list(compenpoip=compenpoip,lambda=lambda))
}
#generate compensated poisson processes
rcompenpoip<-function(lambda,WW){
  poip<-rpoip(lambda,WW)
  return (rcompenpoiph(poip))
}
#plot compensated poisson processes 
plotcompenpoip<-function(compenpoip){
  lambda<-compenpoip$lambda
  compenpoip<-compenpoip$compenpoip
  
  ll<-nrow(compenpoip)
    
  tt<-vector(mode="numeric",length=2*ll)
  tt[(1:ll)*2-1]<-compenpoip[,1]
  tt[(1:ll)*2]<-c(tail(compenpoip[,1],-1),compenpoip[ll,1])
  
  vv<-vector(mode="numeric",length=2*ll)
  vv[(1:ll)*2-1]<-compenpoip[,2]
  vv[(1:ll)*2]<-compenpoip[,2]-lambda*(tt[(1:ll)*2]-tt[(1:ll)*2-1])
  
  xlim=c(min(tt),max(tt))
  ylim=c(min(vv),max(vv))
  plot(x=tt[1],y=vv[1], xlim=xlim,ylim=ylim,xlab="time",ylab="value")
  for(ii in 1:ll){
    lines(tt[(ii*2-1):(ii*2)],vv[(ii*2-1):(ii*2)],type="l")
  }
  lines(x=tt[ll*2-1],y=vv[ll*2-1],type="p")
}

compenpoip<-rcompenpoiph(poip)
plot(compenpoip$compenpoip)
plotcompenpoip(compenpoip)