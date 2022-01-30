set.seed(31415927) # Pour la reproductibilité

# Tirage aléatoire de 101 nombres entre 0 et 1 
X <- runif(101)

sort(X)[51]
(med <- median(X))

sum(X)/length(X)
(moy <- mean(X))

X^2
mean((X-moy)^2)
var(X)  
sum((X-moy)^2)/(length(X)-1)

# Tirage aléatoire de 100 nombres entre 0 et 1 
Y <- runif(100)

sort(Y)[5]
quantile(Y, 0.05, type = 1)
quantile(Y, 0.05)

sort(Y)[50]
quantile(Y, 0.5, type = 1)
(sort(Y)[50]+sort(Y)[51])/2
median(Y)

# QUelques plots...
hist(Y)
abline(v = median(Y), col = "blue")
abline(v = mean(Y), col = "red")
abline(v = quantile(Y, 0.05), col = "green")
abline(v = quantile(Y, 0.95), col = "green")

# Tirage aléatoire de 100 nombres selon une loi gaussienne standard 
Y <- rnorm(1000)

sort(Y)[50]
quantile(Y, 0.05, type = 1)
quantile(Y, 0.05)

sort(Y)[500]
quantile(Y, 0.5, type = 1)
(sort(Y)[500]+sort(Y)[501])/2
median(Y)

# QUelques plots...
hist(Y, col = "lightblue", main = "My first hystogram")
abline(v = median(Y), col = "blue", lwd = 3)
abline(v = mean(Y), col = "red", lwd = 3, lty = 2)
abline(v = quantile(Y, 0.05), col = "green", lwd = 3, lty = 2)
abline(v = quantile(Y, 0.95), col = "green", lwd = 3, lty = 2)
legend("topright", 
       legend = c("Median", "Mean", "Quantiles"),
       col    = c("blue", "red", "green"), 
       lty    = c(1,2,2),
       lwd    = 3)

# Tirage aléatoire de 100 nombres selon une loi gamma fortement dissymétrique (voir partie 2)
Y <- rgamma(1000, scale = 5, shape = 1)

quantile(Y, 0.05)
quantile(Y, 0.50)
median(Y)
mean(Y)
quantile(Y, 0.95)

# QUelques plots...
hist(Y, col = "lightblue", main = "My second hystogram", breaks = 15, xlim = c(0,20))
abline(v = median(Y), col = "blue", lwd = 3)
abline(v = mean(Y), col = "red", lwd = 3, lty = 2)
abline(v = quantile(Y, 0.05), col = "green", lwd = 3, lty = 2)
abline(v = quantile(Y, 0.95), col = "green", lwd = 3, lty = 2)
legend("topright", 
       legend = c("Median", "Mean", "Quantiles"),
       col    = c("blue", "red", "green"), 
       lty    = c(1,2,2),
       lwd    = 3)

# Densité, fonction de répartition
mu <- -2
sd <- 0.4
Y <- rnorm(1e5, mean = mu, sd = sd)

hist(Y, 
     col = "lightblue", 
     main = "Normal distribution", 
     breaks = 51, 
     freq = FALSE, 
     ylim = c(0,1))
lines(x = sort(Y), y = dnorm(sort(Y), mean = mu, sd = sd), col = "darkblue", lwd = 2)
points(x = mean(Y), y = 0, lwd = 8, col = "orange")
segments(x0 = mean(Y)-1.96*sd(Y), y0 = 0, x1 = mean(Y)+1.96*sd(Y), y1 = 0, col = "orange", lwd = 3)
abline(v = quantile(Y, 0.025), col = "green", lwd = 2, lty = 2)
abline(v = quantile(Y, 0.975), col = "green", lwd = 2, lty = 2)
abline(h = 0.025, col = "green", lwd = 2, lty = 2)
abline(h = 0.975, col = "green", lwd = 2, lty = 2)
lines(x = sort(Y), y = pnorm(sort(Y), mean = mu, sd = sd), col = "green", lwd = 2)
legend("right", 
       legend = c("Mean and 95% CI", "Quantiles and probabilities", "CDF", "PDF"),
       col    = c("orange", "green", "green", "darkblue"), 
       lty    = c(1, 2, 1, 1),
       lwd    = 3)

mu <- 0
sd <- 1
qnorm(0.025)
qnorm(0.975)
pnorm(-2)
pnorm(2)
dnorm(-2)
dnorm(2)

mu <- 0
sd <- 5
qnorm(0.025, mean = mu, sd = sd)
qnorm(0.975, mean = mu, sd = sd)
qnorm(0.025, mean = mu, sd = sd)/sd
qnorm(0.975, mean = mu, sd = sd)/sd

# Distributions multivariées
library(tidyverse) # Ici, essentiellement pour les graphiques
library(ggpubr)

nsample <- 2^10
lim <- 5 
sd1 <- 2
sd2 <- 0.75
df <- data.frame(X1 = rnorm(nsample, mean = 0, sd = sd1),
                 X2 = rnorm(nsample, mean = 0, sd = sd2))

g <- ggplot(df) +
  geom_point(aes(x = X1, y = X2), size = 4, alpha = 0.5) +
  geom_density_2d(aes(x = X1, y = X2), size = 1, color = "red") +
  xlim(c(-lim, lim)) +
  ylim(c(-lim, lim)) +
  coord_equal() +
  theme_bw()

theta <- pi/4
rot <- matrix(data = c(cos(theta), -sin(theta),
                       sin(theta),  cos(theta)), ncol = 2)
df2 <- as.data.frame(as.matrix(df)%*%rot)
names(df2) <- c("X1", "X2")
h <- ggplot(df2) +
  geom_point(aes(x = X1, y = X2), size = 4, alpha = 0.5) +
  geom_density_2d(aes(x = X1, y = X2), size = 1, color = "red") +
  xlim(c(-lim, lim)) +
  ylim(c(-lim, lim)) +
  coord_equal() +
  theme_bw()
ggarrange(g, h, ncol = 2, align = "hv")

library(mvtnorm)
(Sigma <- diag(c(sd1^2, sd2^2)))
df3 <- as.data.frame(rmvnorm(nsample, mean = rep(0, 2), sigma = Sigma))
names(df3) <- names(df)
i <- ggplot(df3) +
  geom_point(aes(x = X1, y = X2), size = 4, alpha = 0.5) +
  geom_density_2d(aes(x = X1, y = X2), size = 1, color = "red") +
  xlim(c(-lim, lim)) +
  ylim(c(-lim, lim)) +
  coord_equal() +
  theme_bw()

(Sigmarot <- t(rot) %*% Sigma %*% rot)
df4 <- as.data.frame(rmvnorm(nsample, mean = rep(0, 2), sigma = Sigmarot))
names(df4) <- names(df)
j <- ggplot(df4) +
  geom_point(aes(x = X1, y = X2), size = 4, alpha = 0.5) +
  geom_density_2d(aes(x = X1, y = X2), size = 1, color = "red") +
  xlim(c(-lim, lim)) +
  ylim(c(-lim, lim)) +
  coord_equal() +
  theme_bw()
ggarrange(g, h,
          i, j, 
          ncol = 2, nrow = 2,
          align = "hv")

# Distributions marginales
library(ggExtra)
im <- ggMarginal(i, type = "histogram", color = "darkgrey", fill = "black", 
                 xparams = list(breaks = seq(-5, 5, 1.25), alpha = 0.75), 
                 yparams = list(breaks = seq(-5, 5, 1.25), alpha = 0.75))
jm <- ggMarginal(j, type = "histogram", color = "darkgrey", fill = "black", 
                 xparams = list(breaks = seq(-5, 5, 1.25), alpha = 0.75), 
                 yparams = list(breaks = seq(-5, 5, 1.25), alpha = 0.75))
Final <- ggarrange(im, jm, 
          ncol = 2,
          align = "hv")
Final
ggsave(Final, filename = "Final.jpg", width = 20, units = "cm")

# Distributions multivariées 3D
nsample <- 2^10

(Sigma <- matrix(data = c( 1, -1, 0,
                          -1,  2, 0,
                           0,  0, 3), ncol = 3, byrow = T))
df3d <- as.data.frame(rmvnorm(nsample, mean = rep(0, 3), sigma = Sigma))
names(df3d) <- paste0("X", 1:3)
df3d$`Distance from origin` <- sqrt(df3d$X1^2+df3d$X2^2+df3d$X3^2)
source("Functions.R")
lim <- 5
scatter3D_fancy(x = df3d$X1, y = df3d$X2, z = df3d$X3,
                posx = -lim, posy = lim, posz = -lim,
                colvar = df3d$`Distance from origin`, 
                theta = 45, phi = 25,
                main = "3D gaussian distribution",  clab = c("Distance from", "origin"),
                xlim = c(-lim, lim),
                ylim = c(-lim, lim),
                zlim = c(-lim, lim))

