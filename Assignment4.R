#Question 1
(mouse<- read.csv("mouse.txt", header = FALSE))

t <- seq(1:9)
lny <- log(mouse$V1)

#Q1 a
lrm <- lm(lny ~ t, data = mouse)
summary(lrm)

(alphaha1_hat <- summary(lrm)$coef[1,1])
(alphaha2_hat <- summary(lrm)$coef[2,1])

(y0 <- exp(alphaha1_hat))
(r <- exp(alphaha2_hat) -1)

#Q1 b
(instagrowth <- 100*alphaha2_hat)

(compgrowth <- 100*r)


#Question 2
(sound<- read.csv("sound.txt", header = FALSE, sep = " ",
                  col.names = c('distance','intensity')))

x <- sound$distance
y1 <- sound$intensity
z <- 1/x

(lrm2 <- lm(y1 ~ z, data = sound))
summary(lrm2)

(beta1_hat1 <- summary(lrm2)$coef[1,1])
(beta2_hat2 <- summary(lrm2)$coef[2,1])
#Q2 a
asymptotic <- beta1_hat1

#Q2 b
plot(x, y1, abline())
