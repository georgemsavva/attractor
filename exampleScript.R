library(imagerExtra)
library(RColorBrewer)
library(imager)

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
                             n = 1e9, res=4000, 
                             p=kingsDream[[1]],
                             xlim=lims[[2]], ylim=lims[[1]])

crofterCPP <- imageAttractorCPP(startPos = c(0.1,0.1), 
                             n = 1e9, res=4000, 
                             p=crofters[[1]],
                             xlim=c(-1.1,2.2), ylim=c(-1.1,2.2), mutation = 2)
saveRDS(crofterCPP, "crofterCPP.rds")
crofterCPP <- readRDS("crofterCPP.rds")
ramp <- colorRamp(brewer.pal(9, name = "PuBu"))

cimg1 <- makeImg(.995^crofterCPP, ramp)
save.image(cimg1, file="testMap.png")


cimg2 <- cimg1
R(cimg2) <- imagerExtra::EqualizeADP(R(cimg1))
G(cimg2) <- imagerExtra::EqualizeADP(G(cimg1))
B(cimg2) <- imagerExtra::EqualizeADP(B(cimg1))

save.image(cimg2, file="testMapEq.png")
