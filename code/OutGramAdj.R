library("robustbase")
combinat=function(n,p){
  if (n<p){combinat=0}
  else {combinat=exp(lfactorial(n)-(lfactorial(p)+lfactorial(n-p)))}
}
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

OutGramAdj<- function(x,t=NULL, mag=T,factormg=1.5, p1=1, p2=0, nsim=200, nf=10, FDR=0.007, plotting=TRUE, title="Observations",ylabel="x(t)",linecol=2,legend=T, type="l", lty=2, lwd=1, cex=1, col=NULL, cold=NULL, ylim=NULL, colRef=NULL,...)
{
# x: matrix with n rows and p columns. Each row represents a curve  
# t: vector of length p with the time points at which curves are measured
# mag: whether to look for magnitude outliers (mag=T) or not (mag=F)
# factormg: value of the factor for the detection of magnitude outlier  
# p1: at each time point, curves with values in the p1 percentile while be shifted downwards and the (mbd,mei) 
#     will be recalculated in the new position  
# p2: at each time point, curves with values in the p2 percentile while be shifted upwards and the (mbd,mei) 
#     will be recalculated in the new position  
# nsim: in the adjusted version, number of data sets to simulate for the determination of the factor for shape outlier detection  
# nf: in the adjusted version, number of points in the search grid for the value of the factor for shape outlier detection 
# FDR: in the adjusted version, expected proportion of detected outliers in a non-contaminated sample
# plotting: whether to produce a plot with the observed curves and detected outliers and the outliergram (plotting=T) or not (plotting=F)
# title: main title for the graphics with the observed curves and detected outliers
# ylabel: ylabel for the graphics with the observed curves and detected outliers
# linecol: color for line in graphics 1= gray and black, 2= multicolor
# legend: whether to produce a legend in the graphics with the observed curves and detected outliers
#         to distinguish shape and magnitude outliers   
  
n <- nrow(x); p <- ncol(x) # n: number of observations (curves);  p: dimension of the data
x <- as.matrix(x)

if (n>1) {
  if (p == 1) {x <- t(x)}  
  
# Detrend data by substracting the sample median at each time point
   medx<-rep.row(apply(x,2,median),n)
   dx<- x-medx
   
# Robust covariance matrix estimation with the Orthogonalized
# Gnanadesikan-Kettenring (OGK) method (Maronna and Zamar, 2002) covOGK function, robustbase package
   cov.est<-covOGK(dx,sigmamu=s_Qn)$cov
   L=chol(cov.est)
   
   a0=-2/(n*(n-1))
   a1=2*(n+1)/(n-1)
   a2=a0
   
   cn2<-combinat(n,2)
  # Factor determination
   rmat=apply(t(x),1,rank,ties.method="max")  #in ties, both curves get higher rank
   down=rmat-1
   up=n-rmat
   mbd=(rowSums(up*down)/p+n-1)/cn2
   epi=rowSums(up+1)/(n*p)
   s.epi<-sort(epi,index.return=T)  
   P=a0+a1*epi+a2*n^2*epi^2
   d<-P-mbd

   q1<-quantile(d,0.25)  
   fa1<-quantile(d,0.75)/q1
   fa2<-1.5*max(d)/q1

   fa<-seq(fa1,fa2,,nf)
   fac.og<-matrix(ncol=length(fa),nrow=nsim)  
   
   for (rep in 1:nsim){
# Generation of n curves at p time points from a Gaussian process with zero mean 
# and estimated covariance function, with no outliers 
   e=matrix(rnorm(n*p),p,n)
   y=t(t(L)%*%e)

# Fast Modified Band Depth computation (Sun and Genton) for J=2
   rmat.s=apply(t(y),1,rank,ties.method="max")  #in ties, both curves get higher rank
   down.s=rmat.s-1
   up.s=n-rmat.s
   mbd.s=(rowSums(up.s*down.s)/p+n-1)/cn2
	       
# Modified Epigraph Index computation 	       
   epi.s=rowSums(up.s+1)/(n*p)   
       
# Shape outlier detection  based on the quadratic relation between mbd and epi      
   P.s=a0+a1*epi.s+a2*n^2*epi.s^2
   d.s<-P.s-mbd.s
   q1.s<-quantile(d.s,0.25)
   for (j in 1:length(fa)){
   	   limit<-fa[j]*q1.s
   	   fac.og[rep,j]<-length(which(as.vector(d.s>=limit)))
   }	      
}

out<-abs(apply(fac.og,2,mean)/n-FDR)
factorsh<-fa[which.min(out)]
	
# Shape outlier detection with the selected factor 

   limit<-factorsh*q1
   sh.out<-which(as.vector(d>=limit))
  
# Magnitude outlier detection as in Functional Boxplots by Sun and Genton using MBD    
  if (mag) { 
     s.mbd<-sort(mbd,index.return=T)
     m=ceiling(n*0.5)#at least 50%
     center=x[s.mbd$ix[(m+1):n],]
    	   
     inf=apply(center,2,min)
     sup=apply(center,2,max)
  		
     dist=factormg*(sup-inf)
     upper=sup+dist
     lower=inf-dist
     upper<-rep(upper,n)
     dim(upper)<-c(p,n)
     upper<-t(upper)
     lower<-rep(lower,n)
     dim(lower)<-c(p,n)
     lower<-t(lower)
  	       
     outly=(x<=lower)+(x>=upper)
     outrow=rowSums(outly)
  
     mag.out<-which(as.vector(outrow>0))
  }
  else{mag.out<-c()}
      
  #Level variation for further detection of shape outliers 
  x.or<-x
  sh.out2<-c()
  me.mb<-c()
  non.out<-setdiff(1:n,sh.out)
  for (i in non.out) {
    x<-x.or
    if (epi[i]<0.5){         
      s<-sort(x[i,]-apply(x[-i,,drop=F],2,quantile,p=p1),index.return=T)  #p1 percentile of each column compared to values in x[i,] 
      if (s$x[p] >0){
        x[i,]<-x[i,]-s$x[p]  
        # Fast Modified Band Depth computation (Sun and Genton) for J=2
        rmat=apply(t(x),1,rank,ties.method="max")  
        down=rmat-1
        up=n-rmat
        mbd2=(rowSums(up*down)/p+n-1)/cn2
        # Modified Epigraph Index computation 	       
        epi2=rowSums(up+1)/(n*p)
        s.epi2=sort(epi2,index.return=T)         
        if (mbd2[i]<(a0+a1*epi2[i]+a2*n^2*epi2[i]^2)-limit) {
          sh.out2<-append(sh.out2,i)
          me.mb<-cbind(me.mb,c(epi2[i],mbd2[i]))
          d[i]<-mbd2[i]-(a0+a1*epi2[i]+a2*n^2*epi2[i]^2)
        }
      }
    }
    if (epi[i]>=0.5){
      s<-sort(x[i,]-apply(x[-i,,drop=F],2,quantile,p=p2),index.return=T)  #p2 percentile of each column compared to values in x[i,] 
      if (s$x[1] <0){
        x[i,]<-x[i,]-s$x[1]  
        # Fast Modified Band Depth computation (Sun and Genton) for J=2
        rmat=apply(t(x),1,rank,ties.method="max") 
        down=rmat-1
        up=n-rmat
        mbd2=(rowSums(up*down)/p+n-1)/cn2	       
        # Modified Epigraph Index computation 	       
        epi2=rowSums(up+1)/(n*p)
        s.epi2=sort(epi2,index.return=T)        
        if (mbd2[i]<(a0+a1*epi2[i]+a2*n^2*epi2[i]^2)-limit) {
          sh.out2<-append(sh.out2,i)
          me.mb<-cbind(me.mb,c(epi2[i],mbd2[i]))
          d[i]<-mbd2[i]-(a0+a1*epi2[i]+a2*n^2*epi2[i]^2)
        }
      }
    }
  }
  sh.out<-sort(append(sh.out,sh.out2))
  d.sh<-d[sh.out]
  sd.sh<-sort(d.sh,decreasing=T,index.return=T)
  sh.out<-sh.out[sd.sh$ix]
  x<-x.or
  
  if (plotting) {  
    if (linecol==1){
      color<-rep(8,n)
      color[sh.out]<-1
    }
    else{
    hues = seq(15, 375, length=n+1)
    color<-hcl(h=hues, l=65, c=100)[1:n]
    }
    dev.new(width=12,height=6)
    layout(matrix(c(1,2), 1, 2, byrow = T))
    
    if (length(t)<1) {t<-1:p}
    
    ### plot 1: curves
    matplot(t,t(x),type="l",col =color,xlab="",ylab=ylabel,main=title,lty=1)
    for (ou in mag.out) {
      lines(t,x[ou,],col=1,lty=2)      
    }
    for (ou in sh.out) {
      lines(t,x[ou,],col=1,lty=1)    	
    }
    if (legend) { 
    leg.text<-c("Shape Outliers", "Magnitude Outliers")
    legend("bottomright",leg.text,col=1,lty=c(1,2))
    }
    
    ### plot 2: OutlierGram  MBD vs. MEI
    plot(epi,mbd,type="n",xlab="Modified Epigrahp Index",ylab="Modified Band Depth",main="Outliergram")
    text(epi,mbd,labels=rownames(x),col =color)
    for (i in sh.out2) {
      j<-which(sh.out2==i)
      points(me.mb[1,j],me.mb[2,j],col =color[i],pch=1,cex=3)
      text(me.mb[1,j],me.mb[2,j],labels=rownames(x)[i],col =color[i])
    }     
    Ps=a0+a1*s.epi$x+a2*n^2*s.epi$x^2
    lines(s.epi$x,Ps,col=4)     
    lines(s.epi$x,Ps-limit,col=4,lty=2)
    
    
  }  ## end IF plotting  
  
} ## end IF n>1
return(list(modified.band.depth=mbd,modified.epigrhap.index=epi,dist=d,magnitude.outliers=mag.out,shape.outliers=sh.out,outliers=sort(unique(c(sh.out,mag.out))) ) )
}     


