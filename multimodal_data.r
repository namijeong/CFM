#Table7
MicrobondC3 = c(275000, 13000, 147000, 23000, 181000, 30000, 65000, 10000,
       300000, 173000, 106000, 300000, 300000, 212000, 300000, 300000,
       300000, 2000, 261000, 293000, 88000, 247000, 28000, 143000,
      300000, 23000, 300000, 80000, 245000, 266000)

MODEC3 = c(2, 1, 2 ,1, 2, 1, 1, 1,
          0, 1, 1, 0, 0, 2, 0, 0,
          0, 1, 1, 2, 1, 1, 1, 1,
          0, 1, 0, 1, 2, 2)
          
          
##기존모수추정
shap=c(0.67, 4.49)
scal=c(449430, 340380)
lam = (1/scal)^shap

#---------------------------------------------------------------------------------------

##공압실린더_릴리아
MODE3 = c(2,1,2,2,3,1,2,3 ) 
C3 = c(2000000, 3000000, 5000000, 1000000, 4000000, 1000000, 5000000, 3000000 )

##기존모수추정
shap=c(1.35, 4.25, 3.97)
scal=c(58065000, 26608000, 33166000)
lam = (1/scal)^shap

source("https://github.com/namijeong/CFM/blob/main/Graph.r")
#---------------------------------------------------------------------------------------
##유압실린더_릴리아
MODE3 = c(1,1,1,1,1,
           1,1,1,1,1,
           
           2,2,2,2,2,
           2,2,2,2,2,
           2,2,2,2,2,
           2,2,2,2,2,
           2,2,2)

C3 = c(524, 594, 914, 914, 1223,
        1366, 1519, 1547, 1946, 2324,
                
        18, 21, 64, 95, 110,
        122, 128, 144, 162, 180,
        181, 220, 247, 251, 255,
        269, 276, 337, 376, 412,
        452, 999, 1043)

source("https://github.com/namijeong/CFM/blob/main/Graph.r")
#---------------------------------------------------------------------------------------
###도시 전동차량 운행 고장 1-전자변불량 2- 도어엔진불량 3- 인터록스위치 불량

MODE3 = c(
  1,2,3,3,3,
  2,2,2,3,2,
  3,3,3,3,1,
  2,2,1,3,2,
  2,1,2,3,1,
  3,1,3,2,2)

#시간
C3 = c(
  16560, 24870, 6350, 21320, 19660,
  4740, 16880, 17940, 17590, 17160,
  6460, 6770, 4360, 18600, 23820, 
  12460, 19610, 19710, 12770, 13470, 
  16040, 14570, 16690, 9750, 6780,
  3330, 5180, 10400, 4900, 17140)

#주행거리
C3 = c(450506, 697554, 172471, 573073, 559522,
                139927, 423176, 542915, 496174, 70564, 
                187030, 215185, 124748, 559541, 671701,
                346425, 562824, 554458, 359387, 318089, 
                461940, 437440, 458981, 291694, 219112, 
                100370, 164028, 275809, 120488, 499158)

##기존모수추정값-시간
shap=c(2.3622,2.3622, 2.3622)
scal=c(15290,15290,15290)
lam = (1/scal)^shap

##기존모수추정값-주행거리
shap=c(2.1557,2.1557,2.1557 )
scal=c(416446, 416446,416446)
lam = (1/scal)^shap

source("https://github.com/namijeong/CFM/blob/main/Graph.r")
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
### 1은 A비행단 , 2는 B , 3은 C
MODE3 = c(1,1,1,1,1,1,1,
           1,1,1,1,1,1,1,
           1,1,1,1,1,1,1,
           1,1,1,1,1,1,1,
           1,1,1,1,1,1,1,
           1,1,1,1,1,1,1,
           1,1,1,1,1,1,1,
           
           2,2,2,2,2,2,2,
           2,2,2,
           
           3,3,3,3,3,3,3,
           3 )

###항공기 정비 계획
C3 = c(416, 1041, 569, 445, 435, 724, 790,
       805, 898, 369, 806, 792, 843, 1297,
       524, 842, 524, 305, 841, 823, 523,
       515, 522, 519, 524, 1538, 520, 524,
       516, 1535, 777, 524, 524, 671, 523,
       523, 524, 1450, 523, 812, 948, 517,
       524, 972, 544, 925, 440, 855, 515,
                 
       557, 316, 467, 575, 473, 475, 699,
       467, 895, 948,
                 
       312, 474, 366, 474, 474, 743, 475,
       959)

##기존모수추정
shap=c(2.3622,2.3622, 2.3622)
scal=c(15290,15290,15290)
lam = (1/scal)^shap

##scale = lam^(-1/shape)

source("https://github.com/namijeong/CFM/blob/main/Graph.r")
#---------------------------------------------------------------------------------------



##mode3 <- numeric( length(MODE3) )
##mode3[ MODE3 == "A" ] <- 1
##mode3[ MODE3 == "B" ] <- 2
##mode3[ MODE3 == "C" ] <- 3

