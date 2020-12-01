library(gurobi)
library(slam)


###Example problem 1
model <- list()
model$obj <- c(4,5,3,7,6)
model$modelsense <- "min"
model$rhs <- c(20,10,15,600)
model$sense <- c(">=",">=",">=",">=")
model$vtype <- "C"
model$A <- matrix(c(10,  20,  10,  30,  20,
                    5,   7,   4,   9,   2,
                    1,   4,   10,  2,   1,
                    500, 450, 160, 300, 500), nrow=4, byrow=T)
result <- gurobi(model, list())
print(result$status)
F.names = c('Seeds','Raisins','Flakes','Pecans','Walnuts')
names(result$x) = F.names
print(result$x)
print(result$objval)

###Example Problem 2
model <- list()
model$obj <- (0.5, 0.6, 0.7)
model$modelsense <- "min"
model$rhs <- c(1500000, 1200000, 2000000, 0, 0, 4000000)
model$sense <- c("<=","<=","<=",">=",">=", "=")
model$vtype <- "C"
model$A <- matrix(c(1,0,0,
                    0,1,0,
                    0,0,1,
                    -3,-18,7,
                    -1,-4,2,
                    1,1,1), nrow=6, byrow=T)
result <- gurobi(model, list())
print(result$status)
F.names = c("Brazilian",'Colombian','Peruvian')
names(result$x) = F.names
print(result$x)
print(result$objval)


#(75B + 60C + 85P) / (B + C + P) >= 78
#75B + 60C + 85P >= 78B + 78C + 78P
#-3B -18C +7P >= 0
#
#(15B + 20C + 18P) / (B+C+P) >= 16
#15B + 20C + 18P >= 16B + 16C + 16P
#-B -4C +2P >= 0

#B <= 1500000
#C <= 1200000
#P <= 2000000

#B + C + P = 4000000