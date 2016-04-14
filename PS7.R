#PS7 Mauricio

#set wd
setwd("C:/Users/MauricioAndresVela/Documents/R/Clase/PS7")


sg.int<-function(g,lower,upper, dimensions, parallel=1, ...){ 
  
  #Require packages
  require("SparseGrid"); require(plyr); require(foreach); require(parallel)
  #require("doSNOW")
  
  #Put number of cores
  #registerDoSNOW(makeCluster(parallel, type = "SOCK"))
  #registerDoParallel(cores=parallel)
  cluster <- makeCluster(parallel)
  
  #Upper and lower
  lower<-floor(lower)
  upper<-ceiling(upper)
  if (any(lower>upper)) stop("lower must be smaller than upper")

  #Need to put dimensions according to lenght of lower and upper
  if (dimensions > length(lower) | dimensions > length(upper))  stop("Dimensions is incorrect")

  # Sequence for lower and upper fo each dimension , and expand.grid gives creates all permutations
  clusterExport(cluster, c("lower", "upper"))
  gridss<-as.matrix(expand.grid(parLapply(cluster, 1:dimensions, function(x) seq(lower[x], upper[x]-1, by=1))))

  #Create integration grid
  sp.grid <- createIntegrationGrid( 'KPU', dimension=dimensions, k=5 )
  nodes<-gridss[1,]+sp.grid$nodes
  weights<-sp.grid$weights
  for (i in 2:nrow(gridss)) {
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes)  
    weights<-c(weights,sp.grid$weights)
  }
  
  #evaluate function g in parallel
  gx.sp <- parApply(cluster, nodes, 1, g)
  #gx.sp <- aaply(.data=nodes, .margins = 1, .parallel = parallel, .fun = g) #showing warning 
  val.sp <- gx.sp %*%weights
  #Stop cluster
  stopCluster(cluster)
  
  return(val.sp)
  
}


###################################

#TEST HAT
library(testthat)

#Check inputs
test_that('Check inputs',{
    g <- function(x) x[1]^2+x[2]^2
    dimensions <- 2
    lower <- c(0,0)
    upper <- c(10,10)
    expect_is(dimensions, "numeric")
    expect_is(lower, "numeric")
    expect_is(upper, "numeric")
    expect_that(length(lower), equals(length(upper)))
})

#Check output 
test_that("Check output", {
  g <- function(x) x[1]^2+x[2]^2
  dimensions <- 2
  lower <- c(0,0)
  upper <- c(10,10)  
  int <- as.numeric(sg.int(g,lower=lower,upper=upper, dimensions=dimensions,parallel=2))
  real_solution <- 20000/3
  expect_equal(int, real_solution, tolerance=0.01)
})


#################################################
#CHECK SPEED
library(microbenchmark)
#TWO DIMENSIONS
g <- function(x) x[1]^2+x[2]^2
lower <- c(0,0)
upper <- c(10,10)

microbenchmark(
  "1 core" = sg.int(g, dim=2, lower=lower, upper=upper, parallel=1),
  "3 core" = sg.int(g, dim=2, lower=lower, upper=upper, parallel=3),
  times=1
)
#Worst with 3 cores


#THREE DIMENSIONS
g <- function(x) x[1]^2+x[2]^2+x[3]^2
lower <- c(0,0,0)
upper <- c(10,10,10)

microbenchmark(
  "1 core" = sg.int(g, dim=3, lower=lower, upper=upper, parallel=1),
  "3 core" = sg.int(g, dim=3, lower=lower, upper=upper, parallel=3),
  times=1
)
#Still worst with 3 cores but no much difference as before 



###################################
#Include adaptIntegrate

library(cubature)

g <- function(x) x[1]^2+x[2]^2
lower <- c(0,0)
upper <- c(10,10)


#Check if the results are equal

test_that("Equal with adaptIntegrate", {
  g <- function(x) x[1]^2+x[2]^2
  dimensions <- 2
  lower <- c(0,0)
  upper <- c(10,10)  
  int <- as.numeric(sg.int(g,lower=lower,upper=upper, dimensions=dimensions,parallel=2))
  withadaptIntegrate <- adaptIntegrate(g, lower, upper)$integral
  expect_equal(int, withadaptIntegrate, tolerance=0.01)
})

#Test speed
microbenchmark(
  "1 core" = sg.int(g, dim=3, lower=lower, upper=upper, parallel=1),
  "3 core" = sg.int(g, dim=3, lower=lower, upper=upper, parallel=3),
  times=1
)


