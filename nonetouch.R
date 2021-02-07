
# none can touch
N=10
pos <- matrix(runif(2*N,-5,5), ncol=2)
vel <- matrix(rep(-.001,2*N), ncol=2)
vel[,1] <- pos[,2]*.01 / (pos[,2]^2 + pos[,1]^2)
vel[,2] <- -pos[,1]*.01/ (pos[,2]^2 + pos[,1]^2)
vel[1,] <- c(0,0)
pos[1,] <- c(0,0)
acc <- matrix(rep(0,2*N), ncol=2)
mass <- c(2,runif(N-1,0,.1))
col <- viridis::viridis(N)
png("nbody.png", width=3000, height=3000, type="cairo")
par(bg="black")
plot(NA, cex=mass,col=col, pch=20, xlim=c(-4,4), ylim=c(-4,4), axes=F, ann=F)
g=0.001
w=0.2
for(i in 1:10000){
  vel <- vel + acc
  pos <- pos + vel
  for(j in 2:N){
    #each red is attracted to the blues, but repelled by the reds
    #each blue is attracted to the reds, but repelled by the blue
    acc[j,]=c(0,0)
    for(k in 1:1){
      d1 <- (pos[k,1] - pos[j,1])
      d2 <- (pos[k,2] - pos[j,2])
      r <- sqrt(d1^2 + d2^2)
      if(r>w){
      acc[j,1] <- mass[k] * acc[j,1]+ g*d1 / r^(3)
      acc[j,2] <- mass[k] * acc[j,2]+ g*d2 / r^(3)
      }
    #print(c(d1,d2,r,i,j,k))
    
    }
    
    #print(acc[j,])
  }
  if(i>0000) points(pos[,1]-pos[1,1],pos[,2]-pos[1,2], col=col, pch=20, cex=2*mass^(1/5))
}
dev.off()
shell("nbody.png")
