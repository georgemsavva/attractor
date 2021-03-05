  # This does 1 to 100.
  # george1params <- c(runif(1,-4,4),runif(1,-4,4),runif(1,-4,4),runif(1,-2,2),runif(1,-1,1),runif(1,-1,1),runif(1,-20,20),runif(1,-20,20))

for(i in 151:200) {
  set.seed(i)
  #george1params <-(c(1,7,-11,runif(1,-2,2),runif(1,-1,1),runif(1,0,2*pi),runif(1,0,2*pi),runif(1,-20,20)))
  #george1params <-(c(1,7,-11,runif(1,-4,4),runif(1,-4,4),runif(1,-2*pi,2*pi),runif(1,-2*pi,2*pi),runif(1,-20,20)))
  george1params <-(c(1,11,-14,runif(1,-4,4),runif(1,-4,4),runif(1,-2*pi,2*pi),runif(1,-2*pi,2*pi),runif(1,-20,20)))
  print(george1params)
  mutation=16
  lims <- findLimits(george1params, funs = george16, startX=c(0.5,0.5))
  george1 <- imageAttractorCPP(startPos = c(0.5,0.5), 
                               n = 1e8, res=2000, 
                               p=george1params,
                               xlim=lims[[1]], ylim=lims[[2]],
                               mutation=mutation)
  ramp <- colorRamp(c(brewer.pal(9, name = sample(pallist,1))))
  cimg1 <- makeImg(.9^sqrt(george1), ramp)
  save.image(cimg1, file=sprintf("pngs/zmystery%d_%d.png",mutation,i))
}
