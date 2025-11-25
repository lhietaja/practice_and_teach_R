library(tidyverse)
library(lavaan)
library(lavaanPlot)
library(mediationPlot)

# https://lavaan.ugent.be/tutorial/mediation.html

## Simple example from lavaan website ----
### simulate data
set.seed(1234)
X <- rnorm(100)
M <- 0.5*X + rnorm(100)
Y <- 0.7*M + rnorm(100)
Data <- data.frame(X = X, Y = Y, M = M)

### specify model
model <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
'
### fit model and examine
fit <- sem(model, data = Data)
summary(fit)

### lavaanPlot
lavaanPlot(fit, coefs = TRUE, sig = .05, 
           graph_options = list(rankdir = "LR"))

### plot parameters
plot <- parameterEstimates(fit) %>% 
  filter(op == "~" | op == ":=") %>% 
  mutate(label = factor(label, levels = c("c", "a", "b", "ab", "total"),
                        labels = c("direct effect", "M<-X", "Y<-M", 
                                   "indirect effect", "total effect"))) %>% 
  ggplot(aes(y = label, x = est)) +
  geom_point() +
  geom_linerange(aes(xmax = ci.upper, xmin = ci.lower)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(limits=rev) +
  theme_classic()
plot

## suppression example ----
### simulate data
set.seed(1234)
Xb <- rnorm(300)
Mb <- 0.35*Xb + rnorm(300)
Yb <- -0.2*Mb + 0.1*Xb + rnorm(300)
Data2 <- data.frame(X = Xb, Y = Yb, M = Mb)

### specify model
model2 <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
'
### fit model and examine
fit2 <- sem(model, data = Data2)
summary(fit2)

### lavaanPlot
lavaanPlot(fit2, coefs = TRUE, sig = .05, 
           graph_options = list(rankdir = "LR"))


### plot parameters
plot2 <- parameterEstimates(fit2) %>% 
  filter(op == "~" | op == ":=") %>% 
  mutate(label = factor(label, levels = c("c", "a", "b", "ab", "total"),
                        labels = c("direct effect", "M<-X", "Y<-M", 
                                   "indirect effect", "total effect"))) %>% 
  ggplot(aes(y = label, x = est)) +
  geom_point() +
  geom_linerange(aes(xmax = ci.upper, xmin = ci.lower)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(limits=rev) +
  theme_classic()
plot2

## full-sem example ----

set.seed(1234)
n <- 500

# latent variable X
X1 <- rnorm(n)
X2 <- 0.8 * X1 + rnorm(n)
X3 <- 0.8 * X1 + rnorm(n)

# latent variable M (mediator)
M1 <- 0.7 * X1 + rnorm(n)
M2 <- 0.8 * M1 + rnorm(n)
M3 <- 0.8 * M2 + rnorm(n)

#  latent variable Y (dependent variable)
Y1 <- 0.2 * X1 + 0.7 * M1 + rnorm(n)
Y2 <- 0.8 * Y1 + rnorm(n)
Y3 <- 0.8 * Y2 + rnorm(n)

data3 <- data.frame(X1, X2, X3, M1, M2, M3, Y1, Y2, Y3)

### specify model
model3 <- '
  # Measurement model
  X =~ X1 + X2 + X3  
  M =~ M1 + M2 + M3  
  Y =~ Y1 + Y2 + Y3  

  # Structural model
  M ~ a*X  
  Y ~ b*M  
  Y ~ c*X  

  # Indirect effect
  ab := a*b 
  # Total effect
  total := c + ab 
'

### fit model and examine
fit3 <- sem(model3, data = data3)
summary(fit3, fit.measures = TRUE, standardized = TRUE)

fit3b <- sem(model3, se = "bootstrap", data = data3)
summary(fit3b, fit.measures = TRUE, standardized = TRUE)

### lavaanPlot
lavaanPlot(fit3, coefs = TRUE, sig = .05)

lavaanPlot(fit3b, coefs = TRUE, sig = .05)
### plot parameters
normi <- parameterEstimates(fit3) %>% 
  filter(op == "~" | op == ":=") %>% 
  mutate(label = factor(label, levels = c("c", "a", "b", "ab", "total"),
                        labels = c("direct effect", "M<-X", "Y<-M", 
                                   "indirect effect", "total effect")),
         se = "delta")
boot <- parameterEstimates(fit3b) %>% 
  filter(op == "~" | op == ":=") %>% 
  mutate(label = factor(label, levels = c("c", "a", "b", "ab", "total"),
                        labels = c("direct effect", "M<-X", "Y<-M", 
                                   "indirect effect", "total effect")),
         se = "boot")

plot3data <- rbind(normi, boot)

plot3 <- plot3data %>% 
  ggplot(aes(y = label, x = est, group = se, color = se)) +
  geom_point(aes(color = se),
             position = position_dodge(width = 0.25)) +
  geom_linerange(aes(color = se, xmax = ci.upper, xmin = ci.lower),
                 position = position_dodge(width = 0.25)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(limits=rev) +
  theme_classic()
plot3
