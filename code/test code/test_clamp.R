#########
# a potential caveat 
#Maxent uses random background points from the background as a approximation of the distribution of environmental conditions of the study area. The default number of background points is 10000, which generally could represent well the background. However, there is a special situation that environmental conditions of occurrences may exceed the range of conditions of the 10000 background points, and such situation is expected to be more common when extreme conditions exist or when the study area has high number of pixels (e.g., high resolution). Such situation will lead to a problem for clamping, which clamping begins within the range of conditions of occurrences (in version 3.3.3k). To avoid such issue, we may need to manually add environmental conditions of occurrences to the background points if the former exceed the latter.
p <- extract(clim,occ_train) 
a <- extract(clim,bg)  
a <- rbind(p,a)  ## add conditions of occurrences to that of the background points
pa <- c(rep(1,nrow(p)), rep(0,nrow(a))) 
pder <- as.data.frame(rbind(p,a)) 


# here we simulate a scenario that conditions of occurrences exceed that of the backround points
set.seed(1)
p <- sample(1:20,30,replace=T)
min(p)

set.seed(1)
a <- sample(1:20000,10000,replace=T)
min(a)

# by default, the clamping function shows an issue

p <- data.table::data.table(bio1=c(1,2,3,4))
a <- sample(1:1000)

mod4_clamp <- maxent(x=pder[c("bio1","bio11")], 
                     p=pa, 
                     path=paste0(getwd(),"/maxent_outputs4_clamp"), 
                     args=prepPara(para_userfeatures="LQ",
                                   para_betamultiplier=1,
                                   para_doclamp = TRUE,
                                   para_projectionlayers="D:/projects/2017_7_workshop_enm_R/data/bioclim") ) 








########################################################


# example 1, when only 1 var of occ exceed bg
env <- 1:20000
set.seed(1)
a <- sample(env,10000,replace=T)
min(a)

# set.seed(1)
# p <- sample(1:20,30,replace=T)
# min(p)
p <- c(-10,-9,-8,1,2,3,4,5)

sim_env <- seq(-25,25,0.1)
sim_env <- as.data.frame(sim_env)
names(sim_env) <- "var1"
sim_env$Species <- "virtualsp1"
sim_env$X <- 0
sim_env$Y <- 0
names(sim_env)
sim_env <- sim_env[c("Species","X","Y","var1")]
write.csv(sim_env,"sim_env.csv",row.names = F)

# by default, the clamping function shows an issue
p <- as.data.frame(p)
a <- as.data.frame(a)
names(p) <- "var1"
names(a) <- "var1"
pder <- rbind(p,a)
pa <- c(rep(1,nrow(p)),rep(0,nrow(a)))

mod4_sim1 <- maxent(x=pder,p=pa, 
                     path=paste0(getwd(),"/maxent_outputs4_sim1"), 
                     args=prepPara(#para_userfeatures="LQ",
                                   #para_betamultiplier=1,
                                   para_doclamp = TRUE,
                                   para_projectionlayers="D:/projects/2017_7_workshop_enm_R/sim_env.csv") ) 
ped1 <- read.csv(paste0(getwd(),"/maxent_outputs4_sim1/species_sim_env.csv"))
plot(sim_env$var1,ped1$species.logistic.values)
abline(v=min(p))

# example 2,
env <- 1:20000
set.seed(1)
a <- sample(env,10000,replace=T)
min(a)
env2 <- 100:20000
set.seed(1)
a2 <- sample(env2,10000,replace=T)
min(a2)
a <- cbind(a,a2)

# set.seed(1)
# p <- sample(1:20,30,replace=T)
# min(p)
p <- -10:10
p1 <- 90:110
p <- cbind(p,p1)

sim_env1 <- seq(-25,25,0.5)
sim_env2 <- seq(80,120,0.5)
sim_env <- expand.grid(sim_env1,sim_env2)
names(sim_env) <- c("var1","var2")
sim_env$Species <- "virtualsp1"
sim_env$X <- 0
sim_env$Y <- 0
names(sim_env)
sim_env <- sim_env[c("Species","X","Y","var1","var2")]
write.csv(sim_env,"sim_env_2var.csv",row.names = F)

p <- as.data.frame(p)
a <- as.data.frame(a)
names(p) <- c("var1","var2")
names(a) <- c("var1","var2")
pder <- rbind(p,a)
pa <- c(rep(1,nrow(p)),rep(0,nrow(a)))

mod4_sim2 <- maxent(x=pder,p=pa, 
                    path=paste0(getwd(),"/maxent_outputs4_sim2"), 
                    args=prepPara(#para_userfeatures="LQ",
                      #para_betamultiplier=1,
                      para_doclamp = TRUE,
                      para_projectionlayers="D:/projects/2017_7_workshop_enm_R/sim_env_2var.csv") ) 
ped2 <- read.csv(paste0(getwd(),"/maxent_outputs4_sim2/species_sim_env_2var.csv"))
library(rgl)
plot3d(sim_env$var1,sim_env$var2,ped2$species.logistic.values)
plot3d(p$var1,p$var2,0   ,add=T ,col="red")

# example 2, revised
var1 <- seq(1,10,0.5)
var2 <- seq(10,20,0.5)
map <- expand.grid(var1,var2)
plot(map$Var1,map$Var2,xlim=c(-5,10),ylim=c(5,20))
a <- map

n <- 100
rho <- sqrt(runif(n))
theta <- runif(n, 0, 2*pi)
x <- rho * cos(theta)+1
y <- rho * sin(theta)+10
points(x, y, add=T,col="red")
p <- as.data.frame(cbind(var1=x,var2=y))

sim_env1 <- seq(-5,10,0.1)
sim_env2 <- seq(5,20,0.1)
sim_env <- expand.grid(sim_env1,sim_env2)
names(sim_env) <- c("var1","var2")
sim_env$Species <- "virtualsp1"
sim_env$X <- 0
sim_env$Y <- 0
names(sim_env)
sim_env <- sim_env[c("Species","X","Y","var1","var2")]
write.csv(sim_env,"sim_env3.csv",row.names = F)

p <- as.data.frame(p)
a <- as.data.frame(a)
names(p) <- c("var1","var2")
names(a) <- c("var1","var2")
pder <- rbind(p,a)
pa <- c(rep(1,nrow(p)),rep(0,nrow(a)))

source("code/Appendix2_prepPara.R")
mod4 <- maxent(x=pder,p=pa, 
              path=paste0(getwd(),"/maxent_outputs4_sim3"), 
                    args=prepPara(para_userfeatures="LQ",
                                  para_betamultiplier=1,
                      para_doclamp = TRUE,
                      para_projectionlayers="D:/projects/2017_7_workshop_enm_R/sim_env3.csv") ) 
ped <- read.csv(paste0(getwd(),"/maxent_outputs4_sim3/species_sim_env3.csv"))

sim_env <- cbind(sim_env, ped$species.logistic.values)

library(rgl)
plot3d(sim_env$var1,sim_env$var2,ped$species.logistic.values)
plot3d(x=p$var1,y=p$var2,max(ped$species.logistic.values)   ,add=T ,col="red")

s <- akima::interp(sim_env$var1,sim_env$var2,ped$species.logistic.values)
surface3d(s$x,s$y,s$z)

for(seg in seq(9,10,0.5) ){
sim <- subset(sim_env,var2==seg)
plot(sim$var1,sim$`ped$species.logistic.values`)#look at the center
#min(p$var1)
#min(a$var1)
}
