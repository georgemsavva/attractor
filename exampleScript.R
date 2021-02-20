
library(RColorBrewer)
library(imager)
library(Rcpp)

source("attractor3.R")
sourceCpp("attractor.cpp")

## Example script that uses the package:

lordsDreams <- list(c(-2.905148, -2.030427, 1.440550, 0.703070),
                    c(-2.951292, 1.187750, 0.517396, 1.090625),
                    c(2.668752, 1.225105, 0.709998, 0.637272),
                    c(1.380932, 2.656301, 1.157857, 1.272576),
                    c(2.733940, 1.369945, 1.471923, 0.869182),
                    c(1.008118, 2.653920, 0.599124, 0.650700))
kingsDream <- list(c(-0.966918, 2.879879, 0.765145, 0.744728))
crofters <- list(c(2.755364, 2.791253,0,0))

lims <- findLimits(kingsDream[[1]], funs = pickover, startX=c(0.1,0.1))
kingCPP <- imageAttractorCPP(startPos = c(0.1,0.1), 
                             n = 1e7, res=4000, 
                             p=kingsDream[[1]],
                             xlim=lims[[1]], ylim=lims[[2]])


# <- c(1.364845,1.096446,1.920278,1.290250)

lims
saveRDS(george1, "george1.rds")
george1 <-readRDS("george1.rds")

pallist <- rownames(subset(brewer.pal.info, category=="seq"))

for(i in 305:400){
george1params <- round(c(runif(1,1,3),runif(1,1,3),runif(1,-2,2),runif(1,-2,2)),5)
print(george1params)
lims <- findLimits(george1params, funs = george2, startX=c(0.5,0.5))
george1 <- imageAttractorCPP(startPos = c(0.5,0.5), 
                             n = 4e8, res=2000, 
                             p=george1params,
                             xlim=lims[[1]], ylim=lims[[2]],
                             mutation=6)
ramp <- colorRamp(brewer.pal(9, name = sample(pallist,1)))
cimg1 <- makeImg(.995^george1, ramp)
cimg1 <- draw_text(cimg1, text=sprintf("GS attractor: a=%0.5f, b=%0.5f, c=%0.5f, d=%0.5f",
          george1params[1],
          george1params[2],
          george1params[3],
          george1params[4]), x = 20,y=20,fsize = 50, col="white")
save.image(cimg1, file=sprintf("george%05d.png",i))
gc()
}


crofterCPP <- imageAttractorCPP(startPos = c(0.1,0.1), 
                             n = 1e9, res=4000, 
                             p=crofters[[1]],
                             xlim=c(-1.1,2.2), ylim=c(-1.1,2.2), mutation = 2)

saveRDS(crofterCPP, "crofterCPP.rds")
crofterCPP <- readRDS("crofterCPP.rds")
ramp <- colorRamp(brewer.pal(9, name = "PuBu"))

cimg1 <- makeImg(.995^crofterCPP, ramp)
save.image(cimg1, file="testMap.png")

cimg1 <- makeImg(kingCPP2>0, ramp)
save.image(cimg1, file="testMap.png")


cimg2 <- cimg1
R(cimg2) <- imagerExtra::EqualizeADP(R(cimg1))
G(cimg2) <- imagerExtra::EqualizeADP(G(cimg1))
B(cimg2) <- imagerExtra::EqualizeADP(B(cimg1))

save.image(cimg2, file="testMapEq.png")
