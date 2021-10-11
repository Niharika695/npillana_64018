
# Question 1
# Loading packages

library(lpSolve)
library(lpSolveAPI)

# creating lp with 9 variables and 0 constraints

lp <- make.lp(0,9)

#Specify the objective function

set.objfn(lp,c(420,420,420,360,360,360,300,300,300))
lp.control(lp, sense = 'max')

#Adding Constraints

add.constraint(lp, c(1,1,1,0,0,0,0,0,0), "<=", 750 )
add.constraint(lp, c(0,0,0,1,1,1,0,0,0), "<=", 900)
add.constraint(lp, c(0,0,0,0,0,0,1,1,1), "<=", 450)

add.constraint(lp, c(20,15,12,0,0,0,0,0,0), "<=", 13000)
add.constraint(lp, c(0,0,0,20,15,12,0,0,0), "<=", 12000)
add.constraint(lp, c(0,0,0,0,0,0,20,15,12), "<=", 5000)

add.constraint(lp, c(1,1,1,0,0,0,0,0,0), "<=", 900)
add.constraint(lp, c(0,0,0,1,1,1,0,0,0), "<=", 1200)
add.constraint(lp, c(0,0,0,0,0,0,1,1,1), "<=", 750)

add.constraint(lp ,c(900,-750,0,900,-750,0,900,-750,0), "=", 0)
add.constraint(lp ,c(0,450,-900,0,450,-900,0,450,-900), "=", 0)
add.constraint(lp ,c(450,0,-750,450,0,-750,450,0,-750),"=",0)

RowNames <-c("1-ProductionCapacity","2-ProductionCapacity","3-ProductionCapacity",
             "1-StorageSpace","2-StorageSpace","3-StorageSpace",
             "ForecastLarge","ForecastMedium","ForecastSmall",
             "PercentCapP1andP2","PercentCapP2andP3","PercentCapP1andP3")

ColNames <- c("1-PlantLarge","2-PlantLarge","3-PlantLarge",
              "1-PlantMedium","2-PlantMedium","3-PlantMedium",
              "1-PlantSmall","2-PlantSmall","3-PlantSmall")

solve(lp)
get.objective(lp)
get.constraints(lp)
dimnames(lp) <- list(RowNames, ColNames)
lp

# Solving Question 2

# Reduced Costs
get.sensitivity.obj(lp)
#Shadow Prices
get.sensitivity.rhs(lp)
#Dual solution
get.dual.solution(lp)

# Solving Question 3

Sensivity<-data.frame(get.sensitivity.rhs(lp)$duals[1:21],get.sensitivity.rhs(lp)$dualsfrom[1:21],get.sensitivity.rhs(lp)$dualstill[1:21])
names(Sensivity)<-c("Price","low","High")
Sensivity

# solving Question 4

lpdual <- make.lp(0,12)
set.objfn(lpdual, c(750,900,450,13000,12000,5000,900,1200,750,0,0,0))

lp.control(lpDual,sense='min',simplextype="dual")
add.constraint(lpdual ,c(1,0,0,20,0,0,1,0,0,900,0,450), ">=", 420)
add.constraint(lpdual ,c(0,1,0,0,20,0,1,0,0,-750,450,0), ">=", 420)
add.constraint(lpdual ,c(0,0,1,0,0,20,1,0,0,0,-900,-750), ">=", 420)
add.constraint(lpdual ,c(1,0,0,15,0,0,0,1,0,900,0,450), ">=", 360)
add.constraint(lpdual ,c(0,1,0,0,15,0,0,1,0,-750,450,0), ">=", 360)
add.constraint(lpdual ,c(0,0,1,0,0,15,0,1,0,0,-900,-750), ">=", 360)
add.constraint(lpdual ,c(1,0,0,12,0,0,0,0,1,900,0,450), ">=", 300)
add.constraint(lpdual ,c(0,1,0,0,12,0,0,0,1,-750,450,0), ">=", 300)
add.constraint(lpdual ,c(0,0,1,0,0,12,0,0,1,0,-900,-750), ">=", 300)


solve(lpdual)                     
get.objective(lpdual)           
get.variables(lpdual)
get.constraints(lpdual) 