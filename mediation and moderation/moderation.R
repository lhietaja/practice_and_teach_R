library(tidyverse)
library(lavaan)
library(lavaanPlot)
library(modsem)

#https://modsem.org/articles/modsem.html

head(oneInt)
oneInt2 <- oneInt %>% 
  mutate(x1 = 0.5*x1 + rnorm(2000),
         x2 = 0.6*x2 + rnorm(2000),
         x3 = 0.6*x3 + rnorm(2000),
         z1 = 0.5*z1 + rnorm(2000),
         z2 = 0.6*z2 + rnorm(2000),
         z3 = 0.7*z3 + rnorm(2000),
         y1 = 0.7*y1 + rnorm(2000),
         y2 = 0.6*y2 + rnorm(2000),
         y3 = 0.5*y3 + rnorm(2000))


## simple approach ----

### specify model
model <- '
  # Measurement model
  X =~ x1 + x2 + x3  
  Z =~ z1 + z2 + z3
  Y =~ y1 + y2 + y3  

  # Structural model
  Y ~ X + Z + X:Z 
'

### fit basic model and examine
fit <- modsem(model, data = oneInt)
summary(fit, fit.measures = TRUE, standardized = TRUE)

### fit LMS model and examine
fit2 <- modsem(model, method = "lms", data = oneInt)
summary(fit2, fit.measures = TRUE, standardized = TRUE)

### plot parameters
dblcent <- parameter_estimates(fit) %>% 
  filter(op == "~") %>% 
  dplyr::select(rhs, est, ci.lower, ci.upper) %>% 
  mutate(type = "dblcent",
         rhs = if_else(rhs == "XZ", "X:Z", rhs),
         parameter = factor(rhs, levels = c("X","Z","X:Z")))

lms <- parameter_estimates(fit2) %>% 
  filter(op == "~") %>% 
  dplyr::select(rhs, est, ci.lower, ci.upper) %>% 
  mutate(type = "lms",
         parameter = factor(rhs, levels = c("X","Z","X:Z")))

interactions <- rbind(dblcent, lms)

interactions %>% 
  ggplot(aes(y = parameter, x = est, group = type, color = type)) +
  geom_point(aes(color = type),
             position = position_dodge(width = 0.25)) +
  geom_linerange(aes(color = type, xmax = ci.upper, xmin = ci.lower),
                 position = position_dodge(width = 0.25)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(limits=rev) +
  theme_classic()

### classic plots
plot_interaction(x = "X", z = "Z", y = "Y",
                 vals_z = c(-1,0,1), model = fit)


plot_interaction(x = "X", z = "Z", y = "Y",
                 vals_z = c(-1,0,1), model = fit2)

### Johnson-Neyman plots
plot_interaction(x = "X", z = "Z", y = "Y",
                 vals_z = c(-1,0,1), model = fit3)
fit3 <- modsem(model, data = oneInt2) #with less perfect data
plot_jn(x = "X", z = "Z", y = "Y", model = fit2)

## multigroup approach ----
oneInt3 <- oneInt %>% 
  mutate(Z = rowMeans(across(starts_with("Z"))),
         group = cut(Z, breaks=c(-Inf, 0.5, 0.6, Inf), labels=c("low","middle","high"))) %>% 
  dplyr::select(-Z)

head(oneInt3)

model2 <- '
  # Measurement model
  X =~ x1 + x2 + x3  
  Y =~ y1 + y2 + y3  

  # Structural model
  Y ~ X
'

multigroup1 <- sem(model2, data = oneInt3,
                   group = "group",
                   group.equal = c("intercepts", "loadings"))
multigroup2 <- sem(model2, data = oneInt3,
                   group = "group",
                   group.equal = c("intercepts", "loadings", "regressions"))

lavTestLRT(multigroup1, multigroup2)

summary(multigroup1, fit.measures = TRUE, standardized = TRUE)

### plot parameters
parameter_estimates(multigroup1) %>% 
  filter(op == "~") %>% 
  mutate(group = factor(group, levels = c(2,3,1),
                        labels=c("low","middle","high"))) %>% 
  ggplot(aes(y = rhs, x = est, group = group, color = group)) +
  geom_point(aes(color = group),
             position = position_dodge(width = 0.25)) +
  geom_linerange(aes(color = group, xmax = ci.upper, xmin = ci.lower),
                 position = position_dodge(width = 0.25)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(limits=rev) +
  theme_classic()
