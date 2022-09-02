##
## Data from Table C3: Thick coating 
##  Harwell, M. (1995).  Microbond Tests for Ribbon Fibers, 
##  M.S. Thesis, Dept of Chemical Engineering, Clemson University. 
## D = Debonding,  B = Fiber break,  C = Coating Failure
##기계학회
MODEC3 = c(2,1,2,2,3,1,2,3 ) 
MicrobondC3 = c(2000000, 3000000, 5000000, 1000000, 4000000, 1000000, 5000000, 3000000 )

##릴리아
MODEC30 = c(1,1,1,1,1,
           1,1,1,1,1,
           
           2,2,2,2,2,
           2,2,2,2,2,
           2,2,2,2,2,
           2,2,2,2,2,
           2,2,2)

MicrobondC30 = c(524, 594, 914, 914, 1223,
                1366, 1519, 1547, 1946, 2324,
                
                18, 21, 64, 95, 110,
                122, 128, 144, 162, 180,
                181, 220, 247, 251, 255,
                269, 276, 337, 376, 412,
                452, 999, 1043)

##기존모수추정
shap=c(1.35, 4.25, 3.97)
scal=c(58065000, 26608000, 33166000)
lam = (1/scal)^shap


##modeC3 <- numeric( length(MODEC3) )
##modeC3[ MODEC3 == "D" ] <- 1
##modeC3[ MODEC3 == "B" ] <- 2
##modeC3[ MODEC3 == "C" ] <- 3

