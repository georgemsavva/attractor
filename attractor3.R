
pickover <- function(prev, p){
  c(
    sin(p[1]*prev[2])+p[3]*sin(p[1]*prev[1]),
    sin(p[2]*prev[1])+p[4]*sin(p[2]*prev[2])
  )
}

george14 <- function(prev, p){
  c(
    sin(p[2]*prev[2])+p[3]*cos(p[2]*prev[1])+p[5]*sin(prev[1]*p[7]),
    sin(p[1]*prev[1])+p[4]*cos(p[1]*prev[2])+p[6]*sin(prev[2]*p[8])
  )
}
george16 <- function(prev, p){
  c(
    p[7]*(sin(p[1]*prev[2])+cos(p[1]*prev[1])) + p[5]*(sin(p[2]*prev[2])+cos(p[2]*prev[1]))+ p[4]*(sin(p[3]*prev[2]+p[6])+cos(p[3]*prev[1]+p[6])),
    p[7]*(sin(p[1]*prev[1])-cos(p[1]*prev[2])) + p[5]*(sin(p[2]*prev[1])-cos(p[2]*prev[2]))+ p[4]*(sin(p[3]*prev[1]+p[6])-cos(p[3]*prev[2]+p[6]))
    
  )
}

george15 <- function(prev, p){
  c(
    #X[0] = sin(p[1]*prev[1])+p[2]*cos(p[1]*prev[0]) - (prev[1]>0)*1;
    #X[1] = sin(p[0]*prev[0])+p[3]*cos(p[0]*prev[1]) + (prev[0]<0)*1;
    sin(p[2]*prev[2])+p[3]*cos(p[2]*prev[1])- (prev[2]>0)*1,
    sin(p[1]*prev[1])+p[4]*cos(p[1]*prev[2])+ (prev[1]<0)*1
  )
}

georgeM3 <- function(prev, p){
  c(
    # X[0] = abs(sin(p[1]*prev[1]))+pow(sin(p[1]*prev[0]),2);
    #X[1] = abs(sin(p[0]*prev[0]))+pow(sin(p[0]*prev[1]),2);
    abs(sin(p[1]*prev[2]))+p[3]*sin(p[1]*prev[1])^2,
    abs(sin(p[2]*prev[1]))+p[4]*sin(p[2]*prev[2])^2
  )
}

georgeM2 <- function(prev, p){
  c(
    #X[0] = sin(p[1]*prev[1])+pow(sin(p[1]*prev[0]),2);
    #X[1] = sin(p[0]*prev[0])+pow(sin(p[0]*prev[1]),2);
    (sin(p[2]*prev[2]))+sin(p[2]*prev[1])^2,
    (sin(p[1]*prev[1]))+sin(p[1]*prev[2])^2
  )
}



george <- function(prev, p){
  c(
  triangleCPP(p[2]*prev[2])+p[3]*triangleCPP(p[2]*prev[1]),
  triangleCPP(p[1]*prev[1])+p[4]*triangleCPP(p[1]*prev[2])
  )
}
george2 <- function(prev, p){
  c(
    triangleCPP(p[2]*prev[2])+p[3]*triangleCPP(p[2]*prev[1]+pi/2),
    triangleCPP(p[1]*prev[1])+p[4]*triangleCPP(p[1]*prev[2]+pi/2)
  )
}
george3 <- function(prev, p){
  c(
    sin(p[2]*prev[2])+p[3]*sin(p[2]*prev[1]+pi/2),
    triangleCPP(p[1]*prev[1])+p[4]*triangleCPP(p[1]*prev[2]+pi/2)
  )
}
george12 <- function(prev, p){
  c(
    sin(p[2]*prev[2])+p[3]*sawtoothCPP(p[2]*prev[1]+pi/2),
    sawtoothCPP(p[1]*prev[1])+p[4]*sawtoothCPP(p[1]*prev[2]+pi/2)
  )
}

george9 <- function(prev, p){
  c(
    sin(p[2]*prev[2])+p[3]*triangleCPP(p[2]*prev[1]+pi/2),
    sin(p[1]*prev[1])+p[4]*triangleCPP(p[1]*prev[2]+pi/2)
  )
}
george10 <- function(prev, p){
  c(
    sawtoothCPP(p[2]*prev[2])+p[3]*sawtoothCPP(p[2]*prev[1]+pi/2),
    sawtoothCPP(p[1]*prev[1])+p[4]*sawtoothCPP(p[1]*prev[2]+pi/2)
  )
}
george11 <- function(prev, p){
  c(
    sawtoothCPP(p[2]*prev[2])+p[3]*sawtoothCPP(p[2]*prev[1]+pi/2),
    triangleCPP(p[1]*prev[1])+p[4]*triangleCPP(p[1]*prev[2]+pi/2)
  )
}



lordsDreams <- list(c(-2.905148, -2.030427, 1.440550, 0.703070),
                    c(-2.951292, 1.187750, 0.517396, 1.090625),
                    c(2.668752, 1.225105, 0.709998, 0.637272),
                    c(1.380932, 2.656301, 1.157857, 1.272576),
                    c(2.733940, 1.369945, 1.471923, 0.869182),
                    c(1.008118, 2.653920, 0.599124, 0.650700))
kingsDream <- list(c(-0.966918, 2.879879, 0.765145, 0.744728))

#NumericVector iterate(NumericVector prev, NumericVector p, int mutation)
computeAttractor2 <- function(params,mutation,n=1e4,startX){
  X=matrix(NA, nrow=n, ncol=length(startX))
  X[1,] <- startX
  for(i in 2:n) {
    X[i,] <- iterate(prev=X[i-1,], p=params, mutation=mutation)
  }
  X
}

findLimits <- function(params, funs, n=1e4,startX, extend=0.1){
  att1 <- computeAttractor(params, funs, n=n, startX)
  xlim <- c(min(att1[,1]), max(att1[,1]))
  ylim <- c(min(att1[,2]), max(att1[,2]))
  xlim <- xlim + c(-1,1)*(xlim[2]-xlim[1])*extend
  ylim <- ylim + c(-1,1)*(ylim[2]-ylim[1])*extend
  return(list(xlim,ylim))
}

findLimits2 <- function(params, mutation, n=1e4,startX, extend=0.1){
  att1 <- computeAttractor2(params, mutation, n=n, startX)
  xlim <- c(min(att1[,1]), max(att1[,1]))
  ylim <- c(min(att1[,2]), max(att1[,2]))
  xlim <- xlim + c(-1,1)*(xlim[2]-xlim[1])*extend
  ylim <- ylim + c(-1,1)*(ylim[2]-ylim[1])*extend
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

