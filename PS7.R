#PS7 Mauricio

#set wd
setwd("C:/Users/MauricioAndresVela/Documents/R/Clase/PS7")


sg.int<-function(g,...,lower,upper, dimensions, parallel=1){ 
  
  #Require packages
  require("SparseGrid"); require("doSNOW")

  #Put number of cores
  registerDoSNOW(makeCluster(parallel, type = "SOCK"))

  
  #Upper and lower
  lower<-floor(lower)
  upper<-ceiling(upper)
  if (any(lower>upper)) stop("lower must be smaller than upper")

  #Need to put dimensions according to lenght of lower and upper
  if(any(dim != c(length(lower), length(upper))))  stop("Dimensions is incorrect")

  # Sequence for lower and upper fo each dimension , and expand.grid gives creates all permutations
  gridss<-as.matrix(expand.grid(lapply(1:dim, function(x){seq(lower[x], upper[x]-1, by=1)})))

  #Create integration grid
  sp.grid <- createIntegrationGrid( 'KPU', dimension=dimensions, k=5 )
  nodes<-gridss[1,]+sp.grid$nodes
  weights<-sp.grid$weights
  for (i in 2:nrow(gridss)) {
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes)  
    weights<-c(weights,sp.grid$weights)
  }
  
  #evaluate function g
  gx.sp <- apply(nodes, 1, g,...)
  val.sp <- gx.sp %*%weights
  val.sp
}
