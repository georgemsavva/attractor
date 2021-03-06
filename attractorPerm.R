#permute

library(combinat)
perms <- permn(1:4)

#get 4 random numbers


clif1 <- computeAttractor(v, clifford, startX = c(0,0), n = 1e4) 
plot(clif1)

perms[[1]]
v[perms[[2]]]


set.seed(12345)
v <- matrix(NA, nrow=1000, ncol=4)
for(i in 1:1000){
  v[i,] <- runif(4,-3,3)
}

for(i in c(11,275,39)){
graphics.off()
png(filename=sprintf("pngs/cliff_%05d.png",i), width=2000, height=2000, type="cairo")
par(mfrow = c(5,5), mar=3*c(1,1,1,1), oma=5*c(1,1,1,1))
for(perm in perms){
  clif1 <- computeAttractor(v[i,perm], clifford, startX = c(0,0), n = 2e6) 
  plot(clif1, pch=".", axes=F, ann=F, col=hsv(.7,.1,.1,.02))
}
plot(NA,xlim=c(0,1), ylim=c(0,1), axes=F, ann=F)
text(0.1,1, labels=sprintf("Permuted\nClifford\nAttractors\na=%0.5f\nb=%0.5f\nc=%0.5f\nd=%0.5f\n",v[1],v[2],v[3],v[4]), 
     cex=4, adj=c(0,1), family="mono")
dev.off()
}

set.seed(12345)
v <- matrix(NA, nrow=1000, ncol=4)
for(i in 1:1000){
  v[i,] <- runif(4,-3,3)
}

set.seed(12345)
for(i in 1:1000){
  graphics.off()
  png(filename=sprintf("pngs/dejong%05d.png",i), width=2000, height=2000, type="cairo")
  v <- runif(4,-3,3)
  print(c(i,v))
  par(mfrow = c(5,5), mar=3*c(1,1,1,1), oma=5*c(1,1,1,1))
  for(perm in perms){
    clif1 <- computeAttractor(v[i,perm], dejong, startX = c(0,0), n = 5e5) 
    plot(clif1, pch=".", axes=F, ann=F, col="#22000015")
  }
  plot(NA,xlim=c(0,1), ylim=c(0,1), axes=F, ann=F)
  text(0.1,1, labels=sprintf("Permuted\nde Jong\nAttractors\na=%0.5f\nb=%0.5f\nc=%0.5f\nd=%0.5f\n",v[1],v[2],v[3],v[4]), 
       cex=4, adj=c(0,1), family="mono")
  dev.off()
}

library(Rcpp)
cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')
add
add(1,2,3)
