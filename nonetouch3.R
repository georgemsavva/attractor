
# none can touch
for(l in 400:1000){
print(l)
  N=20
dims=3
seed <- sample(1000000,1)
set.seed(seed)
pos <- matrix(runif(2*N,-4,4), ncol=2)
vel <- matrix(rep(-.001,2*N), ncol=2)
r <- sqrt(pos[,2]^2 + pos[,1]^2)
speed <- rnorm(N,0.05,0.02)/2
vel[,1] <- pos[,2]*speed*(r^(-2.5))  
vel[,2] <- -pos[,1]*speed*(r^(-2.5))  
vel[1,] <- c(0,0)
pos[1,] <- c(0,0)
acc <- matrix(rep(0,2*N), ncol=2)
#mass <- pmax(c(50,20*.4^(2:(N))),.01)
mass <- pmax(c(50,20,20*.4^(runif(N-2,1,15))),.01)
col <- hsv(runif(N),runif(N,0,.6),1, alpha = .03)
png(sprintf("pngs/nbody_2_%05d.png",l), width=3000, height=3000, type="cairo")
par(bg="black")
plot(NA, cex=mass,col=col, pch=20, xlim=c(-5,5), ylim=c(-5,5), axes=F, ann=F)
g=0.00001
w=0.1
delta=0.1
for(i in 1:100000){
  vel <- vel + g*acc*delta
  pos <- pos + vel*delta
  for(j in 2:N){
    #each red is attracted to the blues, but repelled by the reds
    #each blue is attracted to the reds, but repelled by the blue
    acc[j,]=c(0,0)
    for(k in 1:N){
      if(mass[j]<mass[k]){
        d1 <- (pos[k,1] - pos[j,1])
        d2 <- (pos[k,2] - pos[j,2])
        r <- sqrt(d1^2 + d2^2)
        if(r>w){
        acc[j,1] <-  acc[j,1]+ mass[k] *d1 / r^(3)
        acc[j,2] <-  acc[j,2]+ mass[k] *d2 / r^(3)
      }
      }
    #print(c(d1,d2,r,i,j,k))
    
    }
    
    #print(acc[j,])
  }
  if(i>0000) points(pos[,1]-pos[1,1],pos[,2]-pos[1,2], col=col, pch=20, cex=c(40,pmin(4*mass[-1]^0.4,5)))
}
text(-4.3,-4.9, labels = paste0("Star system 2\nSeed = ",seed), cex=4, col="white")
dev.off()
print(l)
#shell("nbody.png")
}