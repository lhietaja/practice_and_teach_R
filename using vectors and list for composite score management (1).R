library(tidyverse)
library(faux)

data <- rnorm_multi(n=100, mu = 0, sd = 1, r=0.5, varnames=c("x1","x2","x3",
                                                             "y1","y2","y3"))
var1 <- c("x1", "x2", "x3")
var2 <- c("y1", "y2", "y3")

all_vars <- list("var1" = var1, "var2" = var2) #list of character vectors
all_vars2 <- list("var1" = data[var1], "var2" = data[var2]) #list of data.frame objects

newdata <- data %>% zap_labels %>%
  select(all_of(var1), all_of(var2)) %>%
  mutate(VAR1 = rowMeans(data[var1], na.rm=TRUE),
         VAR2 = rowMeans(data[var2], na.rm=TRUE))
head(newdata)

psych::alpha(data[var1])
psych::alpha(data[var2])

# OR LOOPING OVER LISTS! e.g. count all alphas

# WITH CHARACTER VECTORS
alphas <- list()
for(i in 1:length(all_vars)) {
  output <- alpha(data[all_vars[[i]]])
  alphas[[length(alphas) + 1]] <- output
}
names(alphas) <- names(all_vars)

# WITH "DATA.FRAME" VECTORS
alphas <- sapply(all_vars2, psych::alpha, simplify = FALSE, USE.NAMES = TRUE)
alphas

# there is also a tidyverse solution
library(tidyverse)
alphas <- 
  all_vars2 %>%
  map(psych::alpha)
  
