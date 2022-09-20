

R = function(x, shape, scale){
  exp(-(x/scale)^shape)}

#-----------------------------------------
x=seq(0,3*10^7, by=1000)

plot(x, R(x=x, shape = 0.6710, scale=449442)*R(x=x, shape = 4.3373, scale=340379)
     ,main = "Reliability",xlab="t(cycle)", ylab="R(t)",xlim=c(0,1*10^6), ylim=c(0,1), pch=20)
lines(x, R(x=x,shape = 0.6710, scale=449442)*R(x=x, shape = 4.3373, scale=340379), lty=1, col =1)
lines(x, R(x=x,shape = 0.6710, scale=449442), lty=2, col =2,lwd =1.5 )
lines(x, R(x=x,  shape = 4.3373, scale=340379), lty=3, col =4,lwd =1.5 )

legend (7*10^5,1, lty=c(2,3,1) ,col=c(2,4,1), legend=c("Surge(1)", "Wear(2)","System"), bty="n",lwd = c(1.5,1.5,2) )

#------------------------------------------
x=seq(0,5*10^7, by=10000)

plot(x, R(x=x, shape = 3.77, scale=5444598)*R(x=x, shape = 2.27, scale=4657881)*R(x=x,shape = 1.33, scale=8850255)
            ,main = "Reliability",xlab="t(cycle)", ylab="R(t)",xlim=c(0,10^7), ylim=c(0,1), pch=20)
 
lines(x, R(x=x, shape = 3.77, scale=5444598)*R(x=x, shape = 2.27, scale=4657881)*R(x=x,shape = 1.33, scale=8850255), lty=1, col =1)
lines(x, R(x=x, shape = 1.33, scale=8850255), lty=4, col =4,lwd =1.5 )
lines(x, R(x=x, shape = 2.27, scale=4657881), lty=3, col =3,lwd =1.5 )
lines(x, R(x=x, shape = 3.77, scale=5444598), lty=2, col =2,lwd =1.5 )

legend (0.65*10^7,1, lty=c(4,3,2,1) ,col=c(4,3,2,1), legend=c("Leak(A)", "Pressure(B)", "Speed(C)","System"), bty="n",lwd = c(1.5,1.5,1.5,2) )
#-----------------------------------------------------------------------------
##system MTTF

Rt1=function(x){
  exp(-(x/449442)^0.6710)*exp(-(x/340379)^4.3373)
}

integrate(Rt1,0,3*10^7)



Rt2=function(x){
  exp(-(x/5444598)^3.77)*exp(-(x/8850255)^1.33)*exp(-(x/4657881)^2.27)
}

integrate(Rt2,0,5*10^7)

#---------------------------------------------------------------------------------------------------------------------




