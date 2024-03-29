#============================================================================
rm(list=ls())
##source ("C:/CR/fnpp5.R" )
##source ("C:/CR/MicrobondC3_C.r" )

source("https://github.com/namijeong/CFM/blob/main/fnpp5.R")

##공압실린더
MODE3 = c(2,1,2,2,3,1,2,3 ) 
C3 = c(2000000, 3000000, 5000000, 1000000, 4000000, 1000000, 5000000, 3000000 )


#============================================================================
data1 = C3[MODE3==1] ; data2 = C3[MODE3==2] ; data3 = C3[MODE3==3] ; 
# plot(  log(sort(data1)),  log(-log(1-ppoints(data1))), pch=2, xlab="logx", ylab="loglog"  )
# plot(  log(sort(data2)),  log(-log(1-ppoints(data2))), pch=2, xlab="logx", ylab="loglog"  )
# plot(  log(sort(data3)),  log(-log(1-ppoints(data3))), pch=2, xlab="logx", ylab="loglog"  )
##==========================================================================
idx  = MODE3
data.all = C3 
idx.sort = order( C3 )
idx1 = ( idx[idx.sort]==1)
idx2 = ( idx[idx.sort]==2)
idx3 = ( idx[idx.sort]==3)
data.sort = data.all[idx.sort]
Fall     = ppoints( data.all )

#==============================================================================
##postscript( file="C3Plot.ps", width=4.0, height=4.0)
#pdf( file="C3Plot.pdf", width=4.0, height=4.0)
 par(mar=c(5,5,5,5), omi=c(0,0,0,0), cex=0.6,mex=0.5)
#==============================================================================
xlim = range( log(data.all) );  
 ## ylim = range( log(-log(Fall) ) )
 ylim = c(-5.5, 1) 
 plot(  log(data.sort[idx1]),  log(-log(1-Fall[idx1])), main = "Weibull Probability", pch=2, xlim=xlim, ylim=ylim, 
        xlab="logt", ylab="log{-log(1-F(t))}",lwd = 1.5, cex =1.5  )
 points(log(data.sort[idx2]),  log(-log(1-Fall[idx2])), pch=4,lwd = 1.5, cex =1.5)
 points(log(data.sort[idx3]),  log(-log(1-Fall[idx3])), pch=19,lwd = 1.5, cex =1.5 )
 legend (13.8,1, pch=c(2,4,19),  legend=c("Leak", "Pressure", "Speed"), bty="n" ,lwd = 1.5, cex =1.5)
 legend (13.77, 0.3, lty=c(1,1) ,col=c(4,2),   legend=c("EM_CFM", "ReliSoft_CFM"), bty="n"  ,lwd = 1.5, cex =1.5)

##-----------------------------------------------------------------------
 X = sort(data.all) ; d = idx[ order(data.all) ]
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## Weibull Model
##EM-----------------------------------------------------------------------
 Sweibull <- function(x, alpha,lam) {
        scale1 = lam[1]^(-1/alpha[1])
        scale2 = lam[2]^(-1/alpha[2])
        scale3 = lam[3]^(-1/alpha[3])
   (1-pweibull(x,shape=alpha[1],scale=scale1))*
   (1-pweibull(x,shape=alpha[2],scale=scale2))*
   (1-pweibull(x,shape=alpha[3],scale=scale3)) 
 }
 para.weibull = weibull.cm.EM(X,d, eps=1.0E-5)
 Fweibull = 1-Sweibull(X, alpha=para.weibull$alpha, lam=para.weibull$lam)
 MSEweibull = mean( (Fweibull-Fall)^2 )
 xxx = seq(min(X),max(X), l=100)
 lines(log(xxx), log(-log(Sweibull( xxx, alpha=para.weibull$alpha, lam=para.weibull$lam))), lty=1, col =4 ,lwd = 2)
 
 ##ReliaSoft_pneumatic cylinder result-----------------------------------------------------------------------
 shap=c(1.35, 4.25, 3.97)
 scal=c(58065000, 26608000, 33166000)
 lam = (1/scal)^shap
 
 Sweibull <- function(x, shap,lam) {
   scale1 = lam[1]^(-1/shap[1])
   scale2 = lam[2]^(-1/shap[2])
   scale3 = lam[3]^(-1/shap[3])
   (1-pweibull(x,shape=shap[1],scale=scale1))*
     (1-pweibull(x,shape=shap[2],scale=scale2))*
     (1-pweibull(x,shape=shap[3],scale=scale3)) 
 }
 Fweibull = 1-Sweibull(X, shap=shap, lam=lam)
 MSEweibull = mean( (Fweibull-Fall)^2 )
 xxx = seq(min(X),max(X), l=100)
 lines(log(xxx), log(-log(Sweibull(xxx, shap=shap, lam=lam))), lty=1,lwd = 2, col = 2 )

##-----------------------------------------------------------------------
