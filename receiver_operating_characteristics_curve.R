#esimerkkidata
dataROC %<>%
  select(Y = turnover2, X = BBI15)

write_sav(dataROC, "dataROC.sav")
write.csv(dataROC, "dataROC.csv")

#dataROC <- read.csv("dataROC.csv")
dataROC <- read_spss("dataROC.sav")
dataROC %<>% drop_na() # poistaa puuttuvat

# lataa ensin paketit (vain kerran)
install.packages("pROC")
install.packages("cutoff")

# ja avaa ne (joka kerta)
library(pROC)
library(cutoff)

# tämä on valinnaista, mutta voi helpottaa koodin lukemista
  # dataROC$X tarkoittaa "dataROC" muuttuja X

pROC::roc(dataROC$Y, dataROC$X) #perus ROC / AUC analyysi

roc <- pROC::roc(dataROC$Y,dataROC$X, percent=TRUE,
            # arguments for auc
            partial.auc=c(100, 90), partial.auc.correct=TRUE,
            partial.auc.focus="sens",
            # arguments for ci
            ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
            print.auc=TRUE, show.thres=TRUE)
roc

cutoff::roc(dataROC$X, dataROC$Y) #perus ROC / AUC analyysi + cutoff (eri paketilla)

#lisätietoa
https://www.rdocumentation.org/packages/pROC/versions/1.17.0.1

#cutoff -paketti
https://cran.r-project.org/web/packages/cutoff/cutoff.pdf

#vaihtoehtoinen paketti jolla analyysit voi tehdä
install.packages("ROCR")
library(ROCR)
https://cran.r-project.org/web/packages/ROCR/vignettes/ROCR.html
