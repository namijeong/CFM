#============================================================================
rm(list=ls())


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
## Exponential Model
##-----------------------------------------------------------------------
 Sexp <- function(x, lam) (1-pexp(x,rate=lam[1]))*(1-pexp(x,rate=lam[2]))*(1-pexp(x,rate=lam[3]))
 para.exp = expo.cm.EM(X,d, eps=1.0E-5)
 Fexp = 1-Sexp(X,lam=para.exp$lam)
 MSEexp = mean( (Fexp-Fall)^2 )
 xxx = seq(min(X),max(X), l=100)
## lines(log(xxx), log(-log(Sexp( xxx, lam=para.exp$lam))) )

##-----------------------------------------------------------------------
## Weibull Model
##-----------------------------------------------------------------------
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
 lines(log(xxx), log(-log(Sweibull( xxx, alpha=para.weibull$alpha, lam=para.weibull$lam))), lty=3 )

##-----------------------------------------------------------------------
## Log-normal Model
##-----------------------------------------------------------------------
 Slnorm <- function(x, mu, sd) {
        (1-plnorm(x, meanlog=mu[1], sdlog=sd[1]))*
        (1-plnorm(x, meanlog=mu[2], sdlog=sd[2]))*
        (1-plnorm(x, meanlog=mu[3], sdlog=sd[3]))
 }
 para.norm = norm.cm.EM(log(X),d, eps=1.0E-5)
 Flnorm = 1-Slnorm(X, mu=para.norm$mu, sd=para.norm$sd) 
 MSElnorm = mean( (Flnorm-Fall)^2 )
 xxx = seq(min(X),max(X), l=100)
 lines(log(xxx), log(-log(Slnorm( xxx, mu=para.norm$mu, sd=para.norm$sd))), lty=1 )

##-----------------------------------------------------------------------
## Wald Model 
##-----------------------------------------------------------------------
 Swald <- function(x, loc, scale) {
        (1-pwald(x,location=loc[1],scale=scale[1])) * 
        (1-pwald(x,location=loc[2],scale=scale[2])) *
        (1-pwald(x,location=loc[3],scale=scale[3])) 
 }
 para.wald = wald.cm.EM(X,d, maxits=500, eps=1.0E-3)  ###  wald.cm.EM is very slow. 
 Fwald = 1-Swald(X, loc=para.wald$mu, scale=para.wald$lam) 
 MSEwald = mean( (Fwald-Fall)^2 )
 xxx = seq(min(X),max(X), l=100)
 lines(log(xxx), log(-log(Swald( xxx, loc=para.wald$mu, scale=para.wald$lam))), lty=2 )
##=========================================================================


##=========================================================================
MSE = c(MSEexp, MSEweibull, MSElnorm)
names(MSE) = c("Exponential", "Weibull", "Lognorm")
print(MSE)

cat("\n\n ====================================\n")
cat(" Exponential parameter\n")
print(para.exp)


cat("\n\n ====================================\n")
cat(" Weibull parameter\n")
print(para.weibull)

cat("\n\n ====================================\n")
cat(" Lognormal parameter\n")
print(para.norm)

cat("\n\n ====================================\n")
cat(" Wald parameter\n")
print(para.wald)
