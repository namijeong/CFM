set.seed(1)
ITER = 30
t =  1000 * c( 275,  13, 147,  23, 181,  30,  65,  10, 300, 173,
               106, 300, 300, 212, 300, 300, 300,   2, 261, 293, 88, 247,  28, 143, 300,  23, 300,  80, 245, 266 )

##f = (shape/scale)*{(t/scale)^(shape-1)}

# ReliaSoft=============================================================================

#surge 2, 4,6,7,8,10,11,18,19,21,22,23,24,26,28
shape = 0.67
scale = 449430
f = r = numeric(ITER)
for( i in seq_len(ITER) ) {
  p = t[i]
  r[i] = exp(-(p/scale)^shape)
  f[i] = (shape/scale)*{(p/scale)^(shape-1)}
}
a = r[1]*r[2]*r[3]*r[4]*r[5]*r[6]*r[7]*r[8]*r[9]*r[10]*r[11]*r[12]*r[13]*r[14]*r[15]*r[16]*
  r[17]*r[18]*r[19]*r[20]*r[21]*r[22]*r[23]*r[24]*r[25]*r[26]*r[27]*r[28]*r[29]*r[30]*
  f[2]*f[4]*f[6]*f[7]*f[8]*f[10]*f[11]*f[18]*f[19]*f[21]*f[22]*f[23]*f[24]*f[26]*f[28]
a

#wear 1, 3, 5, 14,20,29,30
shape = 4.49
scale = 340380
r = numeric(ITER)
for( i in seq_len(ITER) ) {
  p = t[i]
  r[i] = exp(-(p/scale)^shape)
  f[i] = (shape/scale)*{(p/scale)^(shape-1)}
}
b =  r[1]*r[2]*r[3]*r[4]*r[5]*r[6]*r[7]*r[8]*r[9]*r[10]*r[11]*r[12]*r[13]*r[14]*r[15]*r[16]*
  r[17]*r[18]*r[19]*r[20]*r[21]*r[22]*r[23]*r[24]*r[25]*r[26]*r[27]*r[28]*r[29]*r[30]*
  f[1]*f[3]*f[5]*f[7]*f[14]*f[20]*f[29]*f[30]
b

Relia.likelihoodlog = log(a*b)
Relia.likelihood = a*b

# EM=============================================================================
set.seed(1)
ITER = 30
t = c(2000000, 3000000, 5000000, 1000000, 4000000,  1000000, 5000000, 3000000)
scal = W$lam = W$alpha = numeric(ITER)

source("https://raw.githubusercontent.com/AppliedStat/R-code/master/2006b/Rpp5.R")

M= list( 2, 1, 2, 1, 2, 1, 1, 1, 0, 1,
         1, 0, 0, 2, 0, 0, 0, 1, 1, 2, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2)

X = 1000 * c( 275,  13, 147,  23, 181,  30,  65,  10, 300, 173,
              106, 300, 300, 212, 300, 300, 300,   2, 261, 293, 88, 247,  28, 143, 300,  23, 300,  80, 245, 266 )


W = weibull.cm.EM(X,M)  

#scal[i] = W$lam[i]^(-(1/W$alpha[i]))


set.seed(1)
ITER = 30
t =  1000 * c( 275,  13, 147,  23, 181,  30,  65,  10, 300, 173,
               106, 300, 300, 212, 300, 300, 300,   2, 261, 293, 88, 247,  28, 143, 300,  23, 300,  80, 245, 266 )

##f = (shape/scale)*{(t/scale)^(shape-1)}

#surge 2, 4,6,7,8,10,11,18,19,21,22,23,24,26,28

f = r = numeric(ITER)
for( i in seq_len(ITER) ) {
  scal[i] = W$lam[i]^(-(1/W$alpha[i]))
  shape = W$alpha[1]
  scale = scal[1]
  p = t[i]
  r[i] = exp(-(p/scale)^shape)
  f[i] = (shape/scale)*{(p/scale)^(shape-1)}
}
a =  r[1]*r[2]*r[3]*r[4]*r[5]*r[6]*r[7]*r[8]*r[9]*r[10]*r[11]*r[12]*r[13]*r[14]*r[15]*r[16]*
  r[17]*r[18]*r[19]*r[20]*r[21]*r[22]*r[23]*r[24]*r[25]*r[26]*r[27]*r[28]*r[29]*r[30]*
  f[2]*f[4]*f[6]*f[7]*f[8]*f[10]*f[11]*f[18]*f[19]*f[21]*f[22]*f[23]*f[24]*f[26]*f[28]
a

#wear 1, 3, 5, 14,20,29,30

r = numeric(ITER)
for( i in seq_len(ITER) ) {
  scal[i] = W$lam[i]^(-(1/W$alpha[i]))
  shape = W$alpha[2]
  scale = scal[2]
  p = t[i]
  r[i] = exp(-(p/scale)^shape)
  f[i] = (shape/scale)*{(p/scale)^(shape-1)}
}
b =  r[1]*r[2]*r[3]*r[4]*r[5]*r[6]*r[7]*r[8]*r[9]*r[10]*r[11]*r[12]*r[13]*r[14]*r[15]*r[16]*
  r[17]*r[18]*r[19]*r[20]*r[21]*r[22]*r[23]*r[24]*r[25]*r[26]*r[27]*r[28]*r[29]*r[30]*
  f[1]*f[3]*f[5]*f[7]*f[14]*f[20]*f[29]*f[30]

b

EM.likelihoodlog = log(a*b)
EM.likelihood = a*b
#=============================================================================
cat("\n\n ====================================\n")
Relia.likelihoodlog
EM.likelihoodlog

Relia.likelihood
EM.likelihood
cat("\n\n ====================================\n")
#=============================================================================

