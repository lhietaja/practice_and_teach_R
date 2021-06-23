install.packages("MplusAutomation") #Mplus automation
install.packages("SBSDiff") #Chi-square difference test
install.packages("openxlsx") #Excel


library(MplusAutomation)
library(SBSDiff)
library(openxlsx)

# MITTAUSINVARIANSSI ----
MODELS <- readModels(c( "Model1.out",
                        "Model2.out",
                        "Model3.out",
                        "Model4.out" ) )
MODELS_MI <- SummaryTable(DIDS, keepCols=c("Filename", "ChiSqM_DF", "ChiSqM_Value",
                                          "ChiSqM_ScalingCorrection", "ChiSqM_PValue", "RMSEA_Estimate",
                                          "CFI", "TLI", "SRMR"), sortBy = "Filename")
MODELS_MI <- cbind(MODELS_MI, rbind(
  sbs.chi(MODELS_MI$ChiSqM_Value[1], MODELS_MI$ChiSqM_Value[1], 
          MODELS_MI$ChiSqM_DF[1], MODELS_MI$ChiSqM_DF[1], 
          MODELS_MI$ChiSqM_ScalingCorrection[1], MODELS_MI$ChiSqM_ScalingCorrection[1]),
  
    sbs.chi(MODELS_MI$ChiSqM_Value[2], MODELS_MI$ChiSqM_Value[1], 
            MODELS_MI$ChiSqM_DF[2], MODELS_MI$ChiSqM_DF[1], 
            MODELS_MI$ChiSqM_ScalingCorrection[2], MODELS_MI$ChiSqM_ScalingCorrection[1]),
  
      sbs.chi(MODELS_MI$ChiSqM_Value[3], MODELS_MI$ChiSqM_Value[2], 
              MODELS_MI$ChiSqM_DF[3], MODELS_MI$ChiSqM_DF[2], 
              MODELS_MI$ChiSqM_ScalingCorrection[3], MODELS_MI$ChiSqM_ScalingCorrection[2]),
  
        sbs.chi(MODELS_MI$ChiSqM_Value[4], MODELS_MI$ChiSqM_Value[3], 
                MODELS_MI$ChiSqM_DF[4], MODELS_MI$ChiSqM_DF[3], 
                MODELS_MI$ChiSqM_ScalingCorrection[4], MODELS_MI$ChiSqM_ScalingCorrection[3]),
          deparse.level = 1, make.row.names = FALSE 
  ) 
)

MODELS_MI

#TO EXCEL ----
results <- createWorkbook()

# CFA
addWorksheet(results, "FITS")

writeData(results, sheet = "FITS", x = MODELS_MI, colNames = T, rowNames = T)

### SAVE
saveWorkbook(results, "models.xlsx", overwrite = T)
