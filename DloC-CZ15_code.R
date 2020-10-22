# 
# Data wrangling
#

# Packages ----------------------------------------------------------------

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, 
                     dependencies = TRUE)
  sapply(pkg, 
         require, 
         character.only = TRUE)
}

packages <- c("ggplot2", 
              "dplyr", 
              "tidyverse", 
              "psych", 
              "lavaan", 
              "readxl",
              "semTools", 
              "plyr",
              "lavaan",
              "corrplot")
ipak(packages)

# Data --------------------------------------------------------------------

# Load Data
DLoC_Dataset <- read_excel("DLoC-CZ15_data.xlsx", 
                           sheet = 1)

# Load codebook
DLoC_Codebook <- read_excel("DLoC-CZ15_data.xlsx", 
                            sheet = 2)

# Descriptive statistics --------------------------------------------------

## Driving licence

### Driving licence for descriptive statistics
DLoC_Driving_Licence_descriptive <- DLoC_Dataset %>%
  select(Q33_1:Q33_2) %>%
  mutate(Driving_Licence = case_when(Q33_1 == 1 | Q33_2 == 1 ~ 'Driving Licence',
                                     TRUE ~ 'No driving Licence')) %>%
  select(Driving_Licence) %>%
  group_by(Driving_Licence) %>%
  dplyr::rename(`Driving licence?` = Driving_Licence) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count))*100,1))

### Driving licence as a variable
Driving_Licence_variable <- DLoC_Dataset %>%
  select(Q33_1:Q33_2) %>%
  mutate(Driving_Licence = case_when(Q33_1 == 1 | Q33_2 == 1 ~ 'Driving Licence',
                                     TRUE ~ 'No driving Licence')) %>%
  select(Driving_Licence) %>%
  dplyr::rename(`Driving licence?` = Driving_Licence)

## DLoC

### Prepare list of variables for descriptive statistics
DLoC_list <- as.matrix(DLoC_Codebook[1:30, 3])

# Create labels for factor levels
labels_DLoC <- c("Disagree very much", "Disagree quite a bit", "Disagree some",
                 "Agree a little", "Agree quite a bit", "Agree very much")

# Force to factors
DLoC <- DLoC_Dataset %>%
  mutate(Driving_Licence = case_when(Q33_1 == 1 | Q33_2 == 1 ~ 'Driving Licence',
                                     TRUE ~ 'No driving Licence')) %>%
  filter(Driving_Licence == "Driving Licence") %>%
  select(Q22_1:Q22_30) %>%
  mutate_all(as.ordered)

# Label values
DLoC <- as.data.frame(lapply(DLoC, function(x) { revalue(x, c("0"="Disagree very much", 
                                                              "1"="Disagree quite a bit", 
                                                              "2"="Disagree some",
                                                              "3"="Agree a little", 
                                                              "4"="Agree quite a bit", 
                                                              "5"="Agree very much")) }))

DLoC <- as.data.frame(lapply(DLoC, function(x) { ordered(x, levels = c("Disagree very much",
                                                                       "Disagree quite a bit", 
                                                                       "Disagree some",
                                                                       "Agree a little", 
                                                                       "Agree quite a bit", 
                                                                       "Agree very much")) }))

# Name variables
names(DLoC) <- DLoC_list

DLoC_summary <- DLoC %>% 
  select_all() %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "Variables") %>%
  spread(names, x) %>%
  select(-Variables, -`NA's`) %>%
  mutate(RowSumsValid = rowSums(.)) %>%
  mutate_all(., funs("percent"= round((./RowSumsValid)*100,1))) %>%
  select(`Disagree very much`, `Disagree very much_percent`,
         `Disagree quite a bit`, `Disagree quite a bit_percent`,
         `Disagree some`, `Disagree some_percent`,
         `Agree a little`, `Agree a little_percent`,
         `Agree quite a bit`, `Agree quite a bit_percent`,
         `Agree very much`, `Agree very much_percent`,
         RowSumsValid)

DLoC_variables <- DLoC %>% 
  select_all() %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), 
         .id = "Variables") %>%
  spread(names, 
         x) %>%
  select(Variables)

DLoC_Summary_Final <- cbind(DLoC_variables, 
                            DLoC_summary)

#
# CFA, SEM
#

# Data ------------------------------------------------------------------------------------------------------------------------------------------

DLoC_CFA_Model_All <- DLoC_Dataset %>%
  mutate(Driving_Licence = case_when(Q33_1 == 1 | Q33_2 == 1 ~ 'Driving Licence',
                                     TRUE ~ 'No driving Licence'),
         Age = 2017 - Q29) 

DLoC_CFA_Model <- DLoC_Dataset %>%
  select(id:Q22_30, 
         Q33_1:Q33_2,
         Q30) %>%
  mutate(Driving_Licence = case_when(Q33_1 == 1 | Q33_2 == 1 ~ 'Driving Licence',
                                     TRUE ~ 'No driving Licence')) 

DLoC_CFA_Model_Drivers <- DLoC_Dataset %>%
  mutate(Driving_Licence = case_when(Q33_1 == 1 | Q33_2 == 1 ~ 'Driving Licence',
                                     TRUE ~ 'No driving Licence'),
         Age = 2017 - Q29) %>%
  filter(Driving_Licence == 'Driving Licence')


DLoC_CFA_Model_Drivers <- na.omit(DLoC_CFA_Model_Drivers)

DLoC_CFA_Model[, 2:34] <- as.data.frame(lapply(DLoC_CFA_Model[, 2:34], 
                                               ordered))

# lavCor <- names("DLoC_E_1")

DLoC_CFA_Model <- na.omit(DLoC_CFA_Model)

# Descriptives ----------------------------------------------------------------------------------------------------------------------------------

# Model -----------------------------------------------------------------------------------------------------------------------------------------
## CFA


# Two-factor model --------------------------------------------------------

modelDLoC = '
EDLoC =~  Q22_1 + Q22_2 + Q22_3 + Q22_4 + Q22_5 +
          Q22_11 + Q22_12 + Q22_13 + Q22_14 + Q22_15 +
          Q22_21 + Q22_22 + Q22_23 + Q22_24 + Q22_25
IDLoC =~  Q22_6 + Q22_7 + Q22_8 + Q22_9 + Q22_10 +
          Q22_16 + Q22_17 + Q22_18 + Q22_19 + Q22_20 +
          Q22_26 + Q22_27 + Q22_28 + Q22_29 + Q22_30'

modelDLoC_orthogonal = '
EDLoC =~  Q22_1 + Q22_2 + Q22_3 + Q22_4 + Q22_5 +
          Q22_11 + Q22_12 + Q22_13 + Q22_14 + Q22_15 +
          Q22_21 + Q22_22 + Q22_23 + Q22_24 + Q22_25
IDLoC =~  Q22_6 + Q22_7 + Q22_8 + Q22_9 + Q22_10 +
          Q22_16 + Q22_17 + Q22_18 + Q22_19 + Q22_20 +
          Q22_26 + Q22_27 + Q22_28 + Q22_29 + Q22_30

EDLoC ~~ 0*IDLoC' # orthogonal factors


DLoC_No_Grouping = cfa(modelDLoC, 
                       DLoC_CFA_Model, 
                       estimator = "WLSMV")

summary(DLoC_No_Grouping, 
        fit.measures = TRUE, 
        standardized = TRUE)

DLoC_Drivers = cfa(modelDLoC,
                  DLoC_CFA_Model_Drivers,
                  estimator = "WLSMV")

summary(DLoC_Drivers,
        fit.measures = TRUE,
        standardized = TRUE)


## Estimating factor scores

## Modification indices

modind = modificationIndices(DLoC_Drivers, 
                             sort = TRUE)

modind[order(-modind$mi), ]

head(modind[order(modind$mi, 
                   decreasing=TRUE),], 
     10)

# McDonald's omega
DLoC_CFA_Omega <- DLoC_CFA_Model %>% dplyr::select(Q22_1:Q22_30)

MBESS::ci.reliability(data=DLoC_CFA_Omega, 
                      type="categorical", 
                      conf.level = 0.95, 
                      interval.type="bca",
                      B=1000)


# One-factor model --------------------------------------------------------

modelDLoC_One_Factor = '
EDLoC =~  Q22_1 + Q22_2 + Q22_3 + Q22_4 + Q22_5 +
          Q22_11 + Q22_12 + Q22_13 + Q22_14 + Q22_15 +
          Q22_21 + Q22_22 + Q22_23 + Q22_24 + Q22_25 + 
          Q22_6 + Q22_7 + Q22_8 + Q22_9 + Q22_10 +
          Q22_16 + Q22_17 + Q22_18 + Q22_19 + Q22_20 +
          Q22_26 + Q22_27 + Q22_28 + Q22_29 + Q22_30'

modelDLoC_One_Factor = '
DLoC =~   Q22_2 + Q22_4 + Q22_11 + Q22_13 + Q22_22 + Q22_23 + Q22_25 + 
          Q22_8 + Q22_9 + Q22_16 + Q22_18 + Q22_19 + Q22_20 + Q22_26 + Q22_27'

DLoC_One_Factor = cfa(modelDLoC_One_Factor, 
                      DLoC_CFA_Model, 
                      estimator = "WLSMV")

summary(DLoC_One_Factor, 
        fit.measures = TRUE, 
        standardized = TRUE)

DLoC_Drivers_1F = cfa(modelDLoC_One_Factor,
                   DLoC_CFA_Model_Drivers,
                   estimator = "WLSMV")

summary(DLoC_Drivers_1F,
        fit.measures = TRUE,
        standardized = TRUE)

# McDonald's omega
DLoC_CFA_Omega <- DLoC_Dataset %>% 
                  mutate(Driving_Licence = case_when(Q33_1 == 1 | Q33_2 == 1 ~ 'Driving Licence',
                                     TRUE ~ 'No driving Licence')) %>%
                  filter(Driving_Licence == 'Driving Licence') %>%
                  dplyr::select(Q22_2, Q22_4, Q22_11, Q22_13, Q22_22,
                                Q22_23, Q22_25, Q22_8, Q22_9, Q22_16,
                                Q22_18, Q22_19, Q22_20, Q22_26, Q22_27)

DLoC_CFA_Omega <- mutate_all(DLoC_CFA_Omega, as.ordered)

DLoC_CFA_Omega[, 8:15] <- lapply(DLoC_CFA_Omega[, 8:15], function(x) { revalue(x,c("0"="5",
                                                                                   "1"="4",
                                                                                   "2"="3",
                                                                                   "3"="2",
                                                                                   "4"="1",
                                                                                   "5"="0" ))})
Start <- Sys.time()
MBESS::ci.reliability(data=DLoC_CFA_Omega, 
                      type="categorical", 
                      conf.level = 0.95, 
                      interval.type="bca",
                      B=1000)
End <- Sys.time()
Result = End-Start
Print(Result)

alpha(DLoC_CFA_Omega <- mutate_all(DLoC_CFA_Omega, as.numeric))

# Final SEM ---------------------------------------------------------------

modelDLoC_One_Factor_SEM = '
DLoC =~   Q22_2 + Q22_4 + Q22_11 + Q22_13 + Q22_22 + Q22_23 + Q22_25 + 
          Q22_8 + Q22_9 + Q22_16 + Q22_18 + Q22_19 + Q22_20 + Q22_26 + Q22_27

DLoC ~ Q6_1 + Q6_2 + Q6_9 + Q6_10 + Q14_2 + Q4 + Q30 + Q31 + Q43 + Age'

DLoC_One_Factor = cfa(modelDLoC_One_Factor_SEM, DLoC_CFA_Model_Drivers, estimator = "WLSMV")
summary(DLoC_One_Factor, fit.measures = TRUE, standardized = TRUE)

# lavaan 0.6-7 ended normally after 98 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         95
# 
# Used       Total
# Number of observations                           871         919
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               579.450     675.126
# Degrees of freedom                               230         230
# P-value (Chi-square)                           0.000       0.000
# Scaling correction factor                                  0.977
# Shift parameter                                           82.086
# simple second-order correction                             
# 
# Model Test Baseline Model:
#   
#   Test statistic                              9770.957    4709.446
# Degrees of freedom                               255         255
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.136
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.963       0.900
# Tucker-Lewis Index (TLI)                       0.959       0.889
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.042       0.047
# 90 Percent confidence interval - lower         0.038       0.043
# 90 Percent confidence interval - upper         0.046       0.051
# P-value RMSEA <= 0.05                          0.999       0.868
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.047       0.047

# Regressions:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# DLoC ~                                                                
# Q6_1             -0.020    0.034   -0.585    0.559   -0.040   -0.033
# Q6_2              0.133    0.035    3.804    0.000    0.262    0.220
# Q6_9              0.058    0.036    1.609    0.108    0.115    0.102
# Q6_10            -0.126    0.038   -3.300    0.001   -0.249   -0.222
# Q14_2             0.009    0.011    0.844    0.398    0.019    0.035
# Q4               -0.002    0.022   -0.086    0.931   -0.004   -0.003
# Q30              -0.055    0.038   -1.448    0.148   -0.108   -0.054
# Q31              -0.028    0.018   -1.558    0.119   -0.055   -0.058
# Q43              -0.041    0.017   -2.392    0.017   -0.082   -0.094
# Age               0.004    0.001    3.415    0.001    0.008    0.133


# Invariance --------------------------------------------------------------

library(semTools)

# Gender
measurementInvariance(model = modelDLoC_One_Factor, 
                      data = DLoC_CFA_Model_All, 
                      group = "Q30")

# Age
DLoC_CFA_Model_All <- DLoC_CFA_Model_All %>%
                    dplyr::mutate(Age_Group = case_when(
                        Age >= 15 & Age <= 30 ~ '15-30',
                        Age >= 31 & Age <= 50 ~ '31-50',
                        Age >= 51 & Age <= 70 ~ '51-70',
                        Age >= 71 ~ '71+',
                        # Age >= 51 ~ '51+',
                        TRUE ~ 'Else'))

measurementInvariance(model = modelDLoC_One_Factor, 
                      data = DLoC_CFA_Model_All, 
                      group = "Age_Group")

## Drivers Only

# Gender
measurementInvariance(model = modelDLoC_One_Factor, 
                      data = DLoC_CFA_Model_Drivers, 
                      group = "Q30")

# Measurement invariance models:
#   
# Model 1 : fit.configural
# Model 2 : fit.loadings
# Model 3 : fit.intercepts
# Model 4 : fit.means
# 
# Chi-Squared Difference Test
# 
#                 Df   AIC   BIC   Chisq Chisq diff Df diff Pr(>Chisq)  
# fit.configural 180 43017 43449  974.23                                
# fit.loadings   194 42998 43363  983.50     9.2730      14    0.81317  
# fit.intercepts 208 42999 43296 1012.37    28.8688      14    0.01089 *
#   fit.means      209 42999 43292 1014.61     2.2413       1    0.13437  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# 
# Fit measures:
#   
#   cfi rmsea cfi.delta rmsea.delta
# fit.configural 0.815 0.099        NA          NA
# fit.loadings   0.816 0.095     0.001       0.004
# fit.intercepts 0.812 0.093     0.003       0.002
# fit.means      0.812 0.093     0.000       0.000

# Age
DLoC_CFA_Model_Drivers <- DLoC_CFA_Model_Drivers %>%
  dplyr::mutate(Age_Group = case_when(
    Age >= 18 & Age <= 50 ~ '18-50',
    Age >= 51  ~ '51+',
    TRUE ~ 'Else')) %>%
  dplyr::filter(Age_Group != 'Else')

measurementInvariance(model = modelDLoC_One_Factor, 
                      data = DLoC_CFA_Model_Drivers, 
                      group = "Age_Group")

# Measurement invariance models:
#   
#   Model 1 : fit.configural
# Model 2 : fit.loadings
# Model 3 : fit.intercepts
# Model 4 : fit.means
# 
# Chi-Squared Difference Test
# 
# Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)    
# fit.configural 180 42947 43379 1004.6                                  
# fit.loadings   194 42943 43307 1028.0     23.329      14  0.0551225 .  
# fit.intercepts 208 42932 43229 1044.9     16.891      14  0.2620387    
# fit.means      209 42944 43237 1059.6     14.693       1  0.0001265 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# 
# Fit measures:
#   
#   cfi rmsea cfi.delta rmsea.delta
# fit.configural 0.807 0.101        NA          NA
# fit.loadings   0.805 0.098     0.002       0.003
# fit.intercepts 0.804 0.095     0.001       0.003
# fit.means      0.801 0.095     0.003       0.001

# Structural Diagram ------------------------------------------------------
library(lavaanPlot)

lavaanPlot(model = DLoC_One_Factor,
           labels = list(DLoC = "Driving Locus of Control",
                         Q22_2 = "DE_02",
                         Q22_4 = "DE_04",
                         Q22_11 = "DE_06",
                         Q22_13 = "DE_08",
                         Q22_22 = "DE_12",
                         Q22_23 = "DE_13",
                         Q22_25 = "DE_15",
                         Q22_8 = "DI_03",
                         Q22_9 = "DI_04",
                         Q22_16 = "DI_06",
                         Q22_18 = "DI_08",
                         Q22_19 = "DI_09",
                         Q22_20 = "DI_10",
                         Q22_26 = "DI_11",
                         Q22_27 = "DI_12"),
           coefs = TRUE,
           stand = TRUE,
           stars = c("latent"),
           node_options = list(shape = "box", fontname = "Helvetica"))


# Corrplot ----------------------------------------------------------------

lavCor <- lavCor(DLoC_Drivers, 
                 ordered = TRUE, 
                 group = NULL,
                 output = "cor")

# lavCorTable <- as.data.frame(lavCor) %>%
#   select(Q22_1:Q22_5, Q22_11:Q22_15, Q22_21:Q22_25) %>%
#   filter_all(., any_vars(. > 0.5 & . !=1))

rownames(lavCor) <- c("DE_01","DE_02","DE_03","DE_04","DE_05",
                      "DE_06","DE_07","DE_08","DE_09","DE_10",
                      "DE_11","DE_12","DE_13","DE_14","DE_15",
                      "DI_01","DI_02","DI_03","DI_04","DI_05",
                      "DI_06","DI_07","DI_08","DI_09","DI_10",
                      "DI_11","DI_12","DI_13","DI_14","DI_15")

colnames(lavCor) <- c("DE_01","DE_02","DE_03","DE_04","DE_05",
                      "DE_06","DE_07","DE_08","DE_09","DE_10",
                      "DE_11","DE_12","DE_13","DE_14","DE_15",
                      "DI_01","DI_02","DI_03","DI_04","DI_05",
                      "DI_06","DI_07","DI_08","DI_09","DI_10",
                      "DI_11","DI_12","DI_13","DI_14","DI_15")

col <- colorRampPalette(c("#FFFFFF", "#525252", "#FFFFFF", "#dbdbdb", "#525252"))
corrplot::corrplot(lavCor, 
                   col=col(200), 
                   method="color", 
                   addCoef.col = "black", 
                   diag=FALSE, type="upper", 
                   tl.col="black", 
                   tl.srt=45, 
                   tl.cex = 0.8, 
                   number.cex= 0.8, 
                   order="alphabet",
                   addshade = "all",
                   addrect = 4
)


# Reviews -----------------------------------------------------------------

## Internal consistency of the 

library("psych")

review_ca <- as.data.frame(DLoC_CFA_Model_All %>%
              dplyr::filter(Driving_Licence == 'Driving Licence') %>%
              dplyr::select(Q6_1:Q14_2))

alpha(review_ca)


# Norms -------------------------------------------------------------------

# Packages ----------------------------------------------------------------

library(cNORM) # install.packages("cNORM")

DLoC_CFA_Model_Drivers <- na.omit(DLoC_CFA_Model_Drivers)

DLoC_Drivers_1F = cfa(modelDLoC_One_Factor,
                      DLoC_CFA_Model_Drivers,
                      estimator = "WLSMV")

DLoCCFAPredictedScores = as.data.frame(lavPredict(DLoC_Drivers_1F, type = "ov"), method = "regression")

DLoCCFAPredictedScores <- DLoCCFAPredictedScores %>%
  dplyr::mutate(DLoC_SUM = Q22_2 + Q22_4 + Q22_11 + Q22_13 + Q22_22 + Q22_23 + Q22_25 + 
                  Q22_8 + Q22_9 + Q22_16 + Q22_18 + Q22_19 + Q22_20 + Q22_26 + Q22_27) %>%
  mutate(Age = DLoC_CFA_Model_Drivers$Age)

DLoC_CFA_Model_Drivers_cNORM <- rankByGroup(DLoCCFAPredictedScores, "Age", "DLoC_SUM")

write.csv(DLoC_CFA_Model_Drivers_cNORM, "DLoC_CFA_Model_Drivers_cNORM.csv")
