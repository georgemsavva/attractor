
computeCliffordAttractor <- function(a,b,c,d,n=1e5,x0=0,y0=0){
  x=y=rep(0,n)
  for(i in 2:n) {
    x[i]=sin(a*y[i-1])+c*cos(a*x[i-1])
    y[i]=sin(b*x[i-1])+d*cos(b*y[i-1])
    data.frame(x,y)
  }
}


computeAttractor <- function(params,funs,n=1e5,startX=c(0,0)){
  X=matrix(NA, nrow=n, ncol=length(startX))
  X[1,] <- startX
  for(i in 2:n) {
    X[i,] <- funs(prev=X[i-1,], params)
  }
  X
}


clifford <- function(prev, p){
  c(sin(p[1]*prev[2])+p[3]*cos(p[1]*prev[1]),
  sin(p[2]*prev[1])+p[4]*cos(p[2]*prev[2]))
}

lorenz <- function(prev, p){
  c(prev[1] + p[4] * p[1] * (prev[2] - prev[1]),
    prev[2] + p[4] * (prev[1] * (p[2] - prev[3]) - prev[2]),
    prev[3] + p[4] * (prev[1] * prev[2] - p[3] * prev[3])
  )
}

computeAttractor(c(1,1,1,1), clifford)
