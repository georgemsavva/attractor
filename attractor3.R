
pickover <- function(prev, p){
  c(
    sin(p[1]*prev[2])+p[3]*sin(p[1]*prev[1]),
    sin(p[2]*prev[1])+p[4]*sin(p[2]*prev[2])
  )
}

george <- function(prev, p){
  c(
    triangleCPP(p[2]*prev[2])+p[3]*triangleCPP(p[2]*prev[1]),
    triangleCPP(p[1]*prev[1])+p[4]*triangleCPP(p[2]*prev[1])
  )
}

lordsDreams <- list(c(-2.905148, -2.030427, 1.440550, 0.703070),
                    c(-2.951292, 1.187750, 0.517396, 1.090625),
                    c(2.668752, 1.225105, 0.709998, 0.637272),
                    c(1.380932, 2.656301, 1.157857, 1.272576),
                    c(2.733940, 1.369945, 1.471923, 0.869182),
                    c(1.008118, 2.653920, 0.599124, 0.650700))
kingsDream <- list(c(-0.966918, 2.879879, 0.765145, 0.744728))


computeAttractor <- function(params,funs,n=1e4,startX){
  X=matrix(NA, nrow=n, ncol=length(startX))
  X[1,] <- startX
  for(i in 2:n) {
    X[i,] <- funs(prev=X[i-1,], params)
  }
  X
}

findLimits <- function(params, funs, n=1e4,startX){
  att1 <- computeAttractor(params, funs, n=n, startX)
  xlim <- 1.1*c(min(att1[-c(1:1000),1]), max(att1[-c(1:1000),1]))
  ylim <- 1.1*c(min(att1[-c(1:1000),2]), max(att1[-c(1:1000),2]))
  return(list(xlim,ylim))
}


makeImg <- function(m, palette, fun=identity){
  nc = ncol(m)
  nr = nrow(m)
  m = fun(m)/max(fun(m)) # fun is a function that maps (0,1) into (0,1), probably a power.
  rgbs = palette(m)
  as.cimg(c(rgbs[,1],rgbs[,2],rgbs[,3]) , x=nc, y=nr, cc=3)
}

#cimg1 <- makeImg(.999^kingCPP, viridsRamp)
#save.image(cimg1, file="testMap.png")

