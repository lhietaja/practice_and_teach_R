# lataa ensin paketit (vain kerran R:n asennuksen/ päivityksen jälkeen)
install.packages(tidyverse) #hyvä kokoelma yleispaketteja
install.packages("pROC")
install.packages("cutoff")

# ja avaa ne (joka kerta)
library(tidyverse)
library(haven) #osaa lukea spss-datan hyvin
library(pROC)
library(cutoff)

# avaa esimerkkidata
dataROC <- haven::read_spss("dataROC.sav") #objekti tallennetaan <- komennolla, sen vasemmalla puolella oleva on siis käyttäjän itse antama nimi objektille
dataROC %<>% drop_na() # poistaa puuttuvat ja tallentaa päälle
head(dataROC) # katso datan ensimmäiset rivit (muitakin datan tarkastelukomentoja on)

# Y kaksiluokkainen selitettävä, X  vaihtelee välillä 13-74

# ROC-ANALYYSIT ----
pROC::roc(dataROC$Y, dataROC$X) #perus ROC / AUC analyysi, "roc" funktioita voi olla muissakin paketeissa pROC:: kertoo että käytetään pROC -paketin versiota

# tehdään ROC-analyysit, tallenetaan sen "oma_roc" -objektiksi ja lisätään kuva + auc -kuvaan,  pROC-paketin dokumentaatiosta löytyy lisää vaihtoehtoja mitä analyysiin tai kuvaan voi lisätä
oma_roc <- pROC::roc(dataROC$Y,dataROC$X,
                 plot=TRUE,
                 print.auc=TRUE)
oma_roc #printtaa ylempänä luodun roc-objektin

# seuraavaksi haetaan koordinaatteja roc -objektille
coords(oma_roc) # perus

coords(oma_roc, "best", best.method = "youden") # hakee "parhaan" youden indeksillä, best.methodia voi vaihtaa 
coords(oma_roc, "best", best.method = "youden", best.weights = c(1, 0.5)) # painotettu youden, oletuspainot 1=relative cost, prevalence=0.5

coords(oma_roc, 50) # tarkkuus raja-arvolle 50 esim.

# nämä voi tallentaa erinäisiksi objekteiksi ja vaikka tallentaa exceliin sitten

# lisätietoa
# https://www.rdocumentation.org/packages/pROC/versions/1.17.0.1 


# Youden cutoffien hakeminen myös cutoff-paketilla on helppoa
cutoff::roc(dataROC$X, dataROC$Y) #perus ROC / AUC analyysi + cutoff Youden indeksillä

# cutoff -paketilla saa raja-arvoja estimoitua myös muilla metodeilla

# cutoff -paketti
# https://cran.r-project.org/web/packages/cutoff/cutoff.pdf

# vaihtoehtoinen paketti jolla ROC-analyysit voi tehdä
install.packages("ROCR")
library(ROCR)
# https://cran.r-project.org/web/packages/ROCR/vignettes/ROCR.html
