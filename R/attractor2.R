
computeAttractor <- function(params,funs,n=1e4,startX){
  X=matrix(NA, nrow=n, ncol=length(startX))
  X[1,] <- startX
  for(i in 2:n) {
    X[i,] <- funs(prev=X[i-1,], params)
  }
  X
}

iterateAttractor <- function(params,funs,X){
  Y=X
  for(i in 1:(dim(X)[1])) {
    Y[i,] <- funs(prev=X[i,], params)
  }
  Y
}

clifford <- function(prev, p){
  c(
    sin(p[1]*prev[2])+p[3]*cos(p[1]*prev[1]),
    sin(p[2]*prev[1])+p[4]*cos(p[2]*prev[2])
  )
}

lorenz <- function(prev, p){
  c(
    prev[1] +  p[4] * p[1] * (prev[2] - prev[1]),
    prev[2] +  p[4] * (prev[1] * (p[2] - prev[3]) - prev[2]),
    prev[3] +  p[4] * (prev[1] * prev[2] - p[3] * prev[3])
  )
}

pngAttractor <- function(X,filename, 
                         width=2000,
                         height=2000, 
                         h=.6,s=0,v=1, alpha=0.1,
                         bg="white",
                         mar=rep(5,4),
                         type="p"){
  png(filename, width=width, height=height, type="cairo")
  par(bg=bg,mar=mar)
  plot(X[,1], X[,2], axes=F, ann=F,
       pch=".",  col=hsv(h=h,s=s,v=v,alpha=alpha), type=type)
  dev.off()
}


